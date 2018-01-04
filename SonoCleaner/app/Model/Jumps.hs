-- Manual operations for eliminating jumps (level-shifts)

{-# LANGUAGE MultiWayIf #-}

module Model.Jumps
  ( setZero
  , setMedianSlope

  , interpolateGroup
  , matchGroup
  ) where

import           Control.Arrow       ((&&&))
import           Control.Lens        hiding (indices)
import           Data.Maybe          (fromMaybe)
import qualified Data.Vector.Unboxed as V

import           Types.Indices
import           Types.LevelShifts

import           Model.Slope
import           Model.TraceState

-------------------------------------------------------------------------------
-- Manual corrections on single level-shifts
-------------------------------------------------------------------------------

samplingRadius :: Index1
samplingRadius = 4

type ReplaceSingleLevelShift
  =  IVector Index1 Double
  -> IIntMap Index1 Double
  -> Index1
  -> Maybe Double

setZero' :: ReplaceSingleLevelShift
setZero' _ _ _ = Just 0

setMedianSlope' :: ReplaceSingleLevelShift
setMedianSlope' ds jumps i
  | null slopes = Nothing
  | otherwise   = Just $ median $ V.fromList slopes
  where
  slopes = getSurroundingSlopes ds jumps samplingRadius i

singleToTrace
  :: ReplaceSingleLevelShift
  -> Hold
  -> Double
  -> Index1
  -> IIntMap Index1 Double
  -> TraceState
  -> TraceState
singleToTrace replace hold offset i jumps traceState =
  fromMaybe traceState $ do
    oldHeight <- iimLookup i jumps
    let ds = snd $ traceState ^. diffSeries
    newHeight <- (+offset) <$> replace ds jumps i
    let updates = [(i, newHeight)]
        leftShift = case hold of
          HoldLeft  -> 0
          HoldRight -> oldHeight - newHeight
    pure $ updateDiffSeries leftShift updates traceState

setZero, setMedianSlope
  :: Hold
  -> Double
  -> Index1
  -> IIntMap Index1 Double
  -> TraceState
  -> TraceState
setZero        = singleToTrace setZero'
setMedianSlope = singleToTrace setMedianSlope'

-- avgSlopeLeft' :: Index1 -> SingleShift
-- avgSlopeLeft' radius ds jumps i _ =
--   -- new stuff, not finished
--   let slopes = filter (not . flip iimMember jumps)
--              $ iiToList
--              $ iiBoundToIVector ds
--              $ IndexInterval (i- radius, i-1)
  -- let boundedRadius = i - max 0 (i - radius)
  --     i0 = i-boundedRadius
  --     prevDiffs = V.map snd
  --       $ V.filter (not . flip M.member jumps . fst)
  --       $ V.zip (V.generate boundedRadius (+i0))
  --       $ V.slice i0 boundedRadius ds
  -- in  if V.null prevDiffs then Nothing else Just $ unboxedAverage prevDiffs

-- estimateSlopeRight' :: Int -> SingleShift
-- estimateSlopeRight' radius ds jumps i _ =
--   let boundedRadius = min (V.length ds - 1) (i + radius) - i
--       i0 = i+1
--       nextDiffs = V.map snd
--         $ V.filter (not . flip M.member jumps . fst)
--         $ V.zip (V.generate boundedRadius (+i0))
--         $ V.slice (i+1) boundedRadius ds
--   in  if V.null nextDiffs then Nothing else Just $ unboxedAverage nextDiffs

-- -- Average the left and right estimates; or, if one side doesn't exist, use the
-- -- side that does.
-- estimateSlopeBoth' :: Int -> SingleShift
-- estimateSlopeBoth' radius ds jumps i h =
--   let left  = estimateSlopeLeft'  radius ds jumps i h
--       right = estimateSlopeRight' radius ds jumps i h
--       avg   = fmap (/2) $ (+) <$> left <*> right
--   in  avg <|> left <|> right

-------------------------------------------------------------------------------
-- Manual operations on pairs and groups of jumps
-------------------------------------------------------------------------------

-- 'Line'
interpolateGroup ::
     Double
  -> [Index1]
  -> IIntMap Index1 Double
  -> TraceState
  -> TraceState
interpolateGroup _ indices _ traceState =
  case indices of
    (_:_:_) ->
      let -- j1 = last indices
          -- v = (traceState ^. series)
          -- pointInterval = I.undiff $ I.fromEndpoints (j0, j1)
          -- yPair = over both (v V.!) $ I.getEndpoints pointInterval
          -- updates = I.interpolationUpdates pointInterval yPair
          updates = interpolationUpdates (traceState ^. series)
                  $ iiUndiff $ IndexInterval $ (head &&& last) indices
      in  updateDiffSeries 0 updates traceState
    _  -> traceState

-- 'Sum'
matchGroup
  :: Double
  -> [Index1]
  -> IIntMap Index1 Double
  -> TraceState
  -> TraceState
matchGroup offset indices jumps traceState
  | (_:_:_) <- indices
  , Just slopes <- traverse (`iimLookup` jumps) indices
  = let ds = snd $ traceState ^. diffSeries
        slopeEsts = map (estimateSlope ds jumps samplingRadius) indices
        slopeErrors = zipWith (-) slopes slopeEsts
        totalError = sum slopeErrors
        newSlopes = totalError + head slopeEsts : tail slopeEsts
          & _last %~ subtract offset
          & _head %~ (+offset)
        updates = zip indices newSlopes
    in  updateDiffSeries 0 updates traceState
  | otherwise = traceState
