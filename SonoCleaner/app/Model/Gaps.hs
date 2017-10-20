module Model.Gaps
  ( LabelledStratum (..)
  , interpolateGapsTrace
  ) where

import           Control.Lens        hiding (indexed)
import qualified Data.IntSet         as S
import qualified Data.Vector.Unboxed as V

import           Types.Series

import           Model.TraceState

-------------------------------------------------------------------------------
-- Interpolation brush
-------------------------------------------------------------------------------

-- Using integers for 'xbounds' implies that there will be some inaccuracy in
-- representing the user-specified line dividing the 'good' and the 'bad' data.
-- This is probably okay, since the division between the upper and lower
-- "levels" should be clear, and the user can always undo and try again with a
-- larger zoom.
data LabelledStratum = LabelLower | LabelUpper

labelGaps
  :: LabelledStratum
  -> (Int, Int)
  -> (Double, Double)
  -> V.Vector Double
  -> [(Int, Int)]
labelGaps stratum (i0, i1) (y0, y1) s
  | i0 >= i1 = []
  | otherwise =
    let len = V.length s
        indexed = V.zip (V.generate len id) s
        sliced = V.slice i0' (i1'-i0'+1) indexed
          where i0' = max 0 i0; i1' = min (len-1) i1
        gapIndices = V.map fst $ V.filter (\(i, y) -> y `cmp` line i) sliced
          where cmp = case stratum of
                  LabelLower -> (<)
                  LabelUpper -> (>)
        onBoundary :: (Int, Int) -> Bool
        onBoundary (a, b) = a == i0 || b == i1
    in  filter (not.onBoundary)
          $ intervalsFromAdjacent $ V.toList gapIndices
  where
    line :: Int -> Double
    line i = let dydx = (y1-y0) / fromIntegral (i1-i0)
          in  y0 + fromIntegral (i-i0) * dydx

    intervalsFromAdjacent :: [Int] -> [(Int, Int)]
    intervalsFromAdjacent [] = []
    intervalsFromAdjacent (i:is) = go i i is where
      go :: Int -> Int -> [Int] -> [(Int, Int)]
      go start end []     = [(start, end)]
      go start end (j:js) =
        if j == end + 1 then go start j js else (start, end) : go j j js

interpolateGaps
  :: [(Int, Int)]
  -> V.Vector Double
  -> (V.Vector Double, [(Int, Int)])
interpolateGaps intervals v =
  let end = V.length v - 1
      touchesBoundary (l, r) = l == 0 || r == end
      targetIntervals = filter (not . touchesBoundary) intervals
      makeMods (l, r) =
         zip [l..r] $ interpolateN (r-l+1) (v V.! (l-1)) (v V.! (r+1))
      v' = v V.// concatMap makeMods targetIntervals
  in  (v', targetIntervals)

interpolateGapsTrace
  :: LabelledStratum
  -> (Int, Int)
  -> (Double, Double)
  -> TraceStateOperator
interpolateGapsTrace stratum xbounds ybounds =
  unsafeTraceStateOperator $ \ts ->
  let s = ts ^. series
      gapIntervals = labelGaps stratum xbounds ybounds s
      (s', modifiedIntervals) = interpolateGaps gapIntervals s
  in  ts & setSeries s'
         & modifiedJumps %~ S.union
           (S.fromList $ concatMap (\(a, b) -> [pred a..b]) modifiedIntervals)
           -- We need not check the bounds for the modified jumps because
           -- 'interpolateGaps' does not act on intervals containing the trace's
           -- endpoints.
