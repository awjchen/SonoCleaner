-- Automatic detection and elimination of jumps (level-shifts).

{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}

module Model.Labelling
  ( labelTraceStateJumps
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

labelTraceStateJumps :: Double -> Double -> TraceState -> IIntMap Index1 Double
labelTraceStateJumps maxNoise jumpTolerance traceState =
  let dv  = snd $ traceState ^. diffSeries
      ddv = snd $ snd $ traceState ^. diff2Series
      jumps = V.map graph $ labelJumps maxNoise jumpTolerance (dv, ddv)
        where graph i = (i, dv `ivIndex` i)
  in  iimFromList1 $ V.toList jumps

labelJumps
  :: Double
  -> Double
  -> (IVector Index1 Double, IVector Index2 Double)
  -> V.Vector Index1
labelJumps maxNoise jumpTolerance =
  -- We extend the slopes vector so that we may detect jumps at the trace edges
  dimap (ivExtend1 1 *** ivExtend2 1) (V.map (subtract 1)) labelJumps'
  where
    slopeLimit = jumpTolerance - maxNoise
    curveLimit = jumpTolerance - maxNoise
    matchLimit = 2*maxNoise

    highCurvaturePts :: IVector Index2 Double -> V.Vector Index2
    highCurvaturePts = ivFindIndices2 ((> curveLimit) . abs)
        -- V.map fst . V.filter ((> curveLimit) . abs . snd) . V.imap (,)

    slopeMatchingIntervals :: IVector Index1 Double -> V.Vector Index2 -> V.Vector (IndexInterval Index2)
    slopeMatchingIntervals dv =
      V.unfoldr (matchSlopes matchLimit slopeLimit dv)

    jumpIntervals :: V.Vector (IndexInterval Index2) -> V.Vector (IndexInterval Index1)
    jumpIntervals = V.map innerInterval
    -- jumpIntervals  = V.map innerInterval

    jumps :: V.Vector (IndexInterval Index1) -> V.Vector Index1
    jumps = V.concatMap iiToVector -- [a..b]
    -- jumps = V.concatMap (\(a, b) -> V.iterateN (b-a+1) succ a) -- [a..b]

    labelJumps' ::
      (IVector Index1 Double, IVector Index2 Double) -> V.Vector Index1
    labelJumps' (dv, ddv) =
        jumps $ jumpIntervals
      $ slopeMatchingIntervals dv $ highCurvaturePts ddv

matchSlopes ::
     Double
  -> Double
  -> IVector Index1 Double
  -> V.Vector Index2
  -> Maybe ((IndexInterval Index2), V.Vector Index2)
matchSlopes matchLimit slopeLimit dv = go
  where
    go :: V.Vector Index2 -> Maybe (IndexInterval Index2, V.Vector Index2)
    go v
      | V.null v = Nothing
      | otherwise = let i0 = V.unsafeHead v; is = V.unsafeTail v in
      case   (V.!? 0) $ V.map fst
           -- Keep only intervals that do not change the slope
           $ V.dropWhile ((> matchLimit) . abs . snd) $ V.indexed
           $ V.map (uncurry subtract . iiIndex dv . iiUndiff)
           -- Limit the number of non-jumps in the interval
           $ V.takeWhile ((<= maxNonJumps) . countNonJumps . innerInterval)
           -- Intervals in curvature indcies (indexing into ddv)
           $ V.map (IndexInterval . (i0,)) is of
           -- $ V.map (i0,) is of
        Nothing -> go (V.unsafeTail v)
        Just n ->
          let markedSlopeInterval = IndexInterval (i0, is V.! n) -- closed interval
              remainingSlopes = V.drop (n+1) is
          in  Just (markedSlopeInterval, remainingSlopes)

    countNonJumps :: IndexInterval Index1 -> Int
    countNonJumps = ivCount ((< slopeLimit) . abs) . (`ivSlice` dv)
    -- countNonJumps (j0, j1) = -- slope indices (indexing into dv)
    --   V.length $ V.filter ((< slopeLimit) . abs) $ V.slice j0 (j1 - j0 + 1) dv

-- -------------------------------------------------------------------------------
-- -- Indices
-- -------------------------------------------------------------------------------

-- -- Mapping from intervals of indices in a diff series to those in the original
-- -- series
innerInterval :: IndexInterval Index2 -> IndexInterval Index1
innerInterval = iiShrink . iiUndiff

-- outerInterval :: (Int, Int) -> (Int, Int)
-- outerInterval (i0, i1) = (i0, i1+1)
