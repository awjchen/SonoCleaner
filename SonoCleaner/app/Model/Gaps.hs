module Model.Gaps
  ( ReplaceDataAboveOrBelow (..)
  , interpolateGaps
  ) where

import           Control.Arrow       ((&&&))
import           Control.Lens        hiding (indexed)

import           Types.Indices

import           Model.TraceState

-------------------------------------------------------------------------------
-- Interpolation brush
-------------------------------------------------------------------------------

data ReplaceDataAboveOrBelow = ReplaceLowerData | ReplaceUpperData

labelGaps
  :: ReplaceDataAboveOrBelow
  -> IndexInterval Index0
  -> (Double, Double)
  -> IVector Index0 Double
  -> [IndexInterval Index0]
labelGaps replaceWhat xInterval (y0, y1) s =
  let gapIndices = filter include [i0..i1]
  in  filter (not . onSelectionBoundary) $ groupRunsToIntvl gapIndices
      -- len = ivLength s
      -- indexed = V.zip (V.generate len id) s
      -- sliced = V.slice i0' (i1'-i0'+1) indexed
      --   where i0' = max 0 i0; i1' = min (len-1) i1
      -- gapIndices = V.map fst $ V.filter (\(i, y) -> y `cmp` cutLine i) sliced
      --   where cmp = case replaceWhat of
      --           LabelLower -> (<)
      --           LabelUpper -> (>)
  -- in  map IndexInterval
  --       $ filter (not.onSelectionBoundary)
  --       $ intervalsFromAdjacent $ V.toList gapIndices
  where
    (IndexInterval (i0, i1)) = iiBoundToIVector s xInterval

    cutLine :: Index0 -> Double
    cutLine i = let dydx = (y1-y0) / fromIntegral (i1-i0)
          in  y0 + fromIntegral (i-i0) * dydx

    include :: Index0 -> Bool
    include i = ivIndex s i `cmp` cutLine i
      where cmp = case replaceWhat of
              ReplaceLowerData -> (<)
              ReplaceUpperData -> (>)

    onSelectionBoundary :: IndexInterval Index0 -> Bool
    onSelectionBoundary (IndexInterval (a, b)) = a == i0 || b == i1

    groupRunsToIntvl :: [Index0] -> [IndexInterval Index0]
    groupRunsToIntvl = map (IndexInterval . (head &&& last)) . groupRuns

    -- intervalsFromAdjacent :: [Int] -> [(Int, Int)]
    -- intervalsFromAdjacent [] = []
    -- intervalsFromAdjacent (i:is) = go i i is where
    --   go :: Int -> Int -> [Int] -> [(Int, Int)]
    --   go start end []     = [(start, end)]
    --   go start end (j:js) =
    --     if j == end + 1 then go start j js else (start, end) : go j j js

interpolateGaps
  :: ReplaceDataAboveOrBelow
  -> IndexInterval Index0
  -> (Double, Double)
  -> TraceState
  -> TraceState
interpolateGaps replaceWhat xInterval ybounds traceState =
  updateDiffSeries 0 updates traceState
    where
      series' = traceState ^. series
      gapIntervals = labelGaps replaceWhat xInterval ybounds series'
      interpolationIntervals = map iiGrow gapIntervals
      updates = concatMap (interpolationUpdates series') interpolationIntervals
      -- interpolationIntervals = gapIntervals
      --   & filter (uncurry (&&) . ((/= 0) *** (/= lastIndex)) . I.getEndpoints)
      --   & map I.grow
      --   where lastIndex = V.length series' - 1
      -- yPairs = map (over both (series' V.!) . I.getEndpoints)
      --              interpolationIntervals
      -- updates = concatMap (uncurry interpolationUpdates)
      --         $ zip interpolationIntervals yPairs

--------------------------------------------------------------------------------
-- Utility
--------------------------------------------------------------------------------

-- Like `span` but applies the `eq` function on adjacent elements
span' :: (a -> a -> Bool) -> [a] -> ([a], [a])
span' _ [] = ([], [])
span' eq (x:xs) = let (as, bs) = go x xs in (x:as, bs) where
  go y zzs@(z:zs)
    | eq y z     = let (as, bs) = go z zs in (z:as, bs)
    | otherwise = ([], zzs)
  go _ [] = ([], [])

-- Like `groupBy`, except defined using `span'` instead of `span`
groupBy' :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy' eq = go
  where go xs = let (ys, xs') = span' eq xs
                in  if null ys then [xs'] else ys : go xs'
  -- null ys => xs' == [], but make this change after the refactoring

-- Input list assumed to have unique elements
groupRuns :: (Eq a, Enum a) => [a] -> [[a]]
groupRuns = groupBy' (\i j -> succ i == j)
