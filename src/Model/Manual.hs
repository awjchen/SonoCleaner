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

samplingRadius :: Int
samplingRadius = 4

type ReplaceSingleLevelShift
  =  IVector Index1 Double
  -> IIntSet Index1
  -> Index1
  -> Maybe Double

-- 'Zero'
setZero' :: ReplaceSingleLevelShift
setZero' _ _ _ = Just 0

-- 'Slope'
setMedianSlope' :: ReplaceSingleLevelShift
setMedianSlope' ds badSegments i
  | null slopes = Nothing
  | otherwise   = Just $ median $ V.fromList slopes
  where
  slopes = getSurroundingSlopes ds badSegments samplingRadius i

singleToTrace
  :: ReplaceSingleLevelShift
  -> Hold
  -> Double
  -> Index1
  -> IIntSet Index1
  -> TraceState
  -> TraceState
singleToTrace replace hold offset i levelShifts traceState =
  fromMaybe traceState $ do
    let ds = snd $ traceState ^. diffSeries
        oldHeight = ivIndex ds i
        noSlopeInfo = mappend levelShifts $ traceState ^. modifiedSegments
    newHeight <- (+offset) <$> replace ds noSlopeInfo i
    let updates = [(i, newHeight)]
        leftShift = case hold of
          HoldLeft  -> 0
          HoldRight -> oldHeight - newHeight
    pure $ updateDiffSeries leftShift updates traceState

setZero, setMedianSlope
  :: Hold
  -> Double
  -> Index1
  -> IIntSet Index1
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
  -> IIntSet Index1
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
  -> IIntSet Index1
  -> TraceState
  -> TraceState
matchGroup offset indices levelShifts traceState
  | (_:_:_) <- indices
  = let ds = snd $ traceState ^. diffSeries
        slopes = map (ivIndex ds) indices
        slopeEsts = map (estimateSlope ds noSlopeInfo samplingRadius) indices
          where noSlopeInfo = mappend levelShifts
                            $ traceState ^. modifiedSegments
        slopeErrors = zipWith (-) slopes slopeEsts
        totalError = sum slopeErrors
        newSlopes = slopeEsts
          & _last %~ subtract offset
          & _head %~ (+(offset + totalError))
        updates = zip indices newSlopes
    in  updateDiffSeries 0 updates traceState
  | otherwise = traceState
