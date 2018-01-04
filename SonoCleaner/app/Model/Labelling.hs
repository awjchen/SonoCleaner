-- Automatic detection and elimination of level-shifts.

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
-- Automatic labelling of level-shifts
-------------------------------------------------------------------------------
-- See the 'Procedural details' section of the user's guide.

maxNonJumps :: Int
maxNonJumps = 4

labelLevelShifts :: Double -> Double -> TraceState -> IIntMap Index1 Double
labelLevelShifts maxNoise jumpTolerance traceState =
  let dv  = snd $ traceState ^. diffSeries
      ddv = snd $ snd $ traceState ^. diff2Series
      jumps = V.map graph $ labelLevelShifts' maxNoise jumpTolerance (dv, ddv)
        where graph i = (i, dv `ivIndex` i)
  in  iimFromList1 $ V.toList jumps

labelLevelShifts'
  :: Double
  -> Double
  -> (IVector Index1 Double, IVector Index2 Double)
  -> V.Vector Index1
labelLevelShifts' maxNoise jumpTolerance =
  -- We extend the data to detect jumps at the trace edges
  dimap (ivExtend1 1 *** ivExtend2 1) (V.map (subtract 1)) labelLevelShifts''
  where
    slopeLimit = jumpTolerance - maxNoise
    curveLimit = jumpTolerance - maxNoise
    matchLimit = 2*maxNoise

    labelLevelShifts''
      :: (IVector Index1 Double, IVector Index2 Double) -> V.Vector Index1
    labelLevelShifts'' (dv, ddv) =
        V.concatMap (iiToVector . innerInterval) -- label everything within the pairs
      $ V.unfoldr (matchSlopes matchLimit slopeLimit dv) -- match curvature points
      $ ivFindIndices2 ((> curveLimit) . abs) ddv -- find high curvature points

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
           -- limit the number of non-jumps in the interval
           $ V.takeWhile ((<= maxNonJumps) . countNonJumps . innerInterval) -- quadratic ?
           -- form candidate intervals
           $ V.map (IndexInterval . (i0,)) is
      in  case maybeMatchingInterval of
        Nothing -> go (V.unsafeTail v)
        Just n ->
          let matchingInterval = IndexInterval (i0, is V.! n)
              remainingSlopes = V.drop (n+1) is
          in  Just (matchingInterval, remainingSlopes)

    countNonJumps :: IndexInterval Index1 -> Int
    countNonJumps = ivCount ((< slopeLimit) . abs) . (`ivSlice` dv)

-------------------------------------------------------------------------------
-- Utility
-------------------------------------------------------------------------------

innerInterval :: IndexInterval Index2 -> IndexInterval Index1
innerInterval = iiShrink . iiUndiff
