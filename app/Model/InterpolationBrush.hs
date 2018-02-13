module Model.InterpolationBrush
  ( ReplaceDataAboveOrBelow (..)
  , interpolationBrush
  ) where

import           Control.Arrow       ((&&&))
import           Control.Lens        hiding (indexed)

import           Types.Indices

import           Model.TraceState

--------------------------------------------------------------------------------

data ReplaceDataAboveOrBelow = ReplaceLowerData | ReplaceUpperData

labelGaps
  :: IVector Index0 Double
  -> ReplaceDataAboveOrBelow
  -> IndexInterval Index0
  -> (Double, Double)
  -> [IndexInterval Index0]
labelGaps s replaceWhat xInterval (y0, y1) =
  let gapIndices = filter include [i0..i1]
  in  filter (not . onSelectionBoundary) $ groupRunsToIntvl gapIndices
  where
    (IndexInterval (i0, i1)) = iiBoundByIVector s xInterval

    cutLine :: Index0 -> Double
    cutLine i = let dydx = (y1-y0) / fromIntegral (i1 `iMinus` i0)
          in  y0 + fromIntegral (i `iMinus` i0) * dydx

    include :: Index0 -> Bool
    include i = ivIndex s i `cmp` cutLine i
      where cmp = case replaceWhat of
              ReplaceLowerData -> (<)
              ReplaceUpperData -> (>)

    onSelectionBoundary :: IndexInterval Index0 -> Bool
    onSelectionBoundary (IndexInterval (a, b)) = a == i0 || b == i1

    groupRunsToIntvl :: [Index0] -> [IndexInterval Index0]
    groupRunsToIntvl = map (IndexInterval . (head &&& last)) . groupRuns

interpolationBrush
  :: ReplaceDataAboveOrBelow
  -> IndexInterval Index0
  -> (Double, Double)
  -> TraceState
  -> TraceState
interpolationBrush replaceWhat xInterval ybounds traceState =
  updateDiffSeries 0 updates traceState
    where
      series' = traceState ^. series
      gapIntervals = labelGaps series' replaceWhat xInterval ybounds
      interpolationIntervals = map iiGrow gapIntervals
      updates = concatMap (interpolationUpdates series') interpolationIntervals

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

-- Like `groupBy` except defined using `span'` instead of `span`
groupBy' :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy' eq = go
  where go xs = let (ys, xs') = span' eq xs
                in  if null ys then [] else ys : go xs'

groupRuns :: (Eq a, Enum a) => [a] -> [[a]]
groupRuns = groupBy' (\i j -> succ i == j)
