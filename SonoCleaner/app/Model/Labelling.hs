-- Automatic detection and elimination of jumps (level-shifts).

{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}

module Model.Labelling
  ( labelTraceStateJumps
  ) where

import           Control.Arrow                     ((***))
import           Control.Lens
import qualified Data.IntMap.Strict                as M
import qualified Data.Vector.Unboxed               as V

import           Types.Series

import           Model.TraceState

-------------------------------------------------------------------------------
-- Automatic labelling of level-shifts
-------------------------------------------------------------------------------
-- See the 'Procedural details' section of the user's guide.

maxNonJumps :: Int
maxNonJumps = 4

labelTraceStateJumps :: Double -> Double -> TraceState -> M.IntMap Double
labelTraceStateJumps maxNoise jumpTolerance traceState =
  let dv  = snd $ traceState ^. diffSeries
      ddv = snd $ snd $ traceState ^. diff2Series
      jumps = V.map graph $ labelJumps maxNoise jumpTolerance (dv, ddv)
        where graph i = (i, dv V.! i)
  in  M.fromList $ V.toList jumps

labelJumps :: Double -> Double -> (V.Vector Double, V.Vector Double) -> V.Vector Int
labelJumps maxNoise jumpTolerance =
  -- We extend the slopes vector so that we may detect jumps at the trace edges
  dimap (extendDiff 1 *** extendDiff2 1) (V.map pred) labelJumps'
  where
    slopeLimit = jumpTolerance - maxNoise
    curveLimit = jumpTolerance - maxNoise
    matchLimit = 2*maxNoise

    highCurvaturePts :: V.Vector Double -> V.Vector Int
    highCurvaturePts =
        V.map fst . V.filter ((> curveLimit) . abs . snd) . V.imap (,)

    slopeMatchingIntervals :: V.Vector Double -> V.Vector Int -> V.Vector (Int, Int)
    slopeMatchingIntervals dv =
      V.unfoldr (matchSlopes matchLimit slopeLimit dv)

    jumpIntervals :: V.Vector (Int, Int) -> V.Vector (Int, Int)
    jumpIntervals  = V.map innerInterval

    jumps :: V.Vector (Int, Int) -> V.Vector Int
    jumps = V.concatMap (\(a, b) -> V.iterateN (b-a+1) succ a) -- [a..b]

    labelJumps' :: (V.Vector Double, V.Vector Double) -> V.Vector Int
    labelJumps' (dv, ddv) =
        jumps $ jumpIntervals
      $ slopeMatchingIntervals dv $ highCurvaturePts ddv

matchSlopes ::
     Double
  -> Double
  -> V.Vector Double
  -> V.Vector Int
  -> Maybe ((Int, Int), V.Vector Int)
matchSlopes matchLimit slopeLimit dv = go
  where
    go :: V.Vector Int -> Maybe ((Int, Int), V.Vector Int)
    go v
      | V.null v = Nothing
      | otherwise = let i0 = V.unsafeHead v; is = V.unsafeTail v in
      case   (V.!? 0)
           -- Keep only intervals that do not change the slope
           $ V.map fst $ V.dropWhile ((> matchLimit) . abs . snd) $ V.imap (,)
           $ V.map (uncurry subtract . over both (dv V.!) . outerInterval)
           -- Limit the number of non-jumps in the interval
           $ V.takeWhile ((<= maxNonJumps) . countNonJumps . innerInterval)
           -- Intervals in curvature indcies (indexing into ddv)
           $ V.map (i0,) is of
        Nothing -> go (V.unsafeTail v)
        Just n ->
          let markedSlopeInterval = (i0, is V.! n) -- closed interval
              remainingSlopes = V.drop (n+1) is
          in  Just (markedSlopeInterval, remainingSlopes)

    countNonJumps :: (Int, Int) -> Int
    countNonJumps (j0, j1) = -- slope indices (indexing into dv)
      V.length $ V.filter ((< slopeLimit) . abs) $ V.slice j0 (j1 - j0 + 1) dv
