-- A crude method for detecting level-shifts.

-- Like other reported methods, we examine the "first and second derivatives".
-- However, in order to handle poorer data where level-shifts occur at every
-- data point in some time interval, we look for pairs of high-curvature points
-- between which the total change in slope is below some user-defined threshold;
-- we interpret this as a sign that the data has returned to "normal", assuming
-- it started "normal". We then label as level-shifts all slope segments lying
-- between pairs of high-curvature points.

-- In order to prevent erroneous pairs between distant high-curvature points, we
-- arbitrarily restrict the pairs to include a maximum number of segments of low
-- slope (`maxNonLevelShifts`).

-- We do not search intelligently for these pairs of high-curvature points: we
-- simply start with the first high-curvature point and scan forward along the
-- rest for a suitable partner; if a partner cannot be found for a
-- high-curvature point, we just ignore it and move on. The result is that
-- failures of this method are catastrophic and easy to detect, which is a good
-- thing, all else equal.

{-# LANGUAGE TupleSections #-}

module Model.Labelling
  ( labelLevelShifts
  ) where

import           Control.Arrow                     ((***))
import           Control.Lens
import qualified Data.Vector.Unboxed               as V

import           Types.Indices

import           Model.TraceState

-------------------------------------------------------------------------------

maxNonLevelShifts :: Int
maxNonLevelShifts = 4

labelLevelShifts :: Double -> Double -> TraceState -> IIntSet Index1
labelLevelShifts maxNoise levelShiftTolerance traceState =
  let dv  = snd $ traceState ^. diffSeries
      ddv = snd $ snd $ traceState ^. diff2Series
      levelShifts = labelLevelShifts' maxNoise levelShiftTolerance (dv, ddv)
  in  iisFromList1 $ V.toList levelShifts

labelLevelShifts'
  :: Double
  -> Double
  -> (IVector Index1 Double, IVector Index2 Double)
  -> V.Vector Index1
labelLevelShifts' maxNoise levelShiftTolerance =
  -- We extend the data to detect level-shifts at the trace edges
  dimap (ivExtend1 1 *** ivExtend2 1)
        (V.map $ iTranslate (-1))
        labelLevelShifts''
  where
    slopeLimit = levelShiftTolerance - maxNoise
    curveLimit = levelShiftTolerance - maxNoise
    -- noise may affect the second derivative twice as much as the first
    matchLimit = 2*maxNoise

    labelLevelShifts''
      :: (IVector Index1 Double, IVector Index2 Double) -> V.Vector Index1
    labelLevelShifts'' (dv, ddv) =
        ddv
      & ivFindIndices2 ((> curveLimit) . abs) -- 1. find high-curvature points
      & V.unfoldr (matchSlopes matchLimit slopeLimit dv) -- 2. match pairs of high-curvature points
      & V.concatMap (iiToVector1 . innerInterval) -- 3. label everything within the pairs

matchSlopes
  :: Double
  -> Double
  -> IVector Index1 Double
  -> V.Vector Index2
  -> Maybe ((IndexInterval Index2), V.Vector Index2)
matchSlopes matchLimit slopeLimit dv = go
  where
    go :: V.Vector Index2 -> Maybe (IndexInterval Index2, V.Vector Index2)
    go v
      | V.null v = Nothing
      | otherwise =
      let i0 = V.unsafeHead v; is = V.unsafeTail v
          maybeMatchingInterval =
           -- get the index of the first such interval
             (V.!? 0) $ V.map fst
           -- keep only intervals over which there is no total change in slope
           $ V.dropWhile ((> matchLimit) . abs . snd) $ V.indexed
           $ V.map (uncurry subtract . iiIndex dv . iiUndiff)
           -- limit the number of non-level-shifts in the interval
           $ V.takeWhile
             ((<= maxNonLevelShifts) . countNonLevelShifts . innerInterval) -- quadratic ?
           -- form candidate intervals
           $ V.map (IndexInterval . (i0,)) is
      in  case maybeMatchingInterval of
        Nothing -> go (V.unsafeTail v)
        Just n ->
          let matchingInterval = IndexInterval (i0, is V.! n)
              remainingSlopes = V.drop (n+1) is
          in  Just (matchingInterval, remainingSlopes)

    countNonLevelShifts :: IndexInterval Index1 -> Int
    countNonLevelShifts = ivCount ((< slopeLimit) . abs) . (`ivSlice` dv)

-------------------------------------------------------------------------------
-- Utility
-------------------------------------------------------------------------------

innerInterval :: IndexInterval Index2 -> IndexInterval Index1
innerInterval = iiShrink . iiUndiff
