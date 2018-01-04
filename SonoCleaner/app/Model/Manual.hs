-- Manually applied corrections to level-shifts

module Model.Manual
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
-- Manually applied corrections on single level-shifts
-------------------------------------------------------------------------------

samplingRadius :: Index1
samplingRadius = 4

type ReplaceSingleLevelShift
  =  IVector Index1 Double
  -> IIntMap Index1 Double
  -> Index1
  -> Maybe Double

-- 'Zero'
setZero' :: ReplaceSingleLevelShift
setZero' _ _ _ = Just 0

-- 'Slope'
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

-------------------------------------------------------------------------------
-- Manually applied corrections on groups of level-shifts
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
      let updates = interpolationUpdates (traceState ^. series)
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
