-- Interface to the plotting library, Chart
--
-- Defines the appearance, but not the content, of the graphical display.
-- This is the final point before which we pass control of rendering to Chart.
-- We apply here optimizations to reduce the cost of our rendering requests.

module View.ChartLayout
  ( chartLayout
  ) where

import           Control.Arrow            ((&&&))
import           Control.Lens             hiding (indices)
import           Data.Colour              (blend, opaque)
import           Data.Colour.Names
import           Data.Default
import           Data.Tuple               (swap)
import qualified Data.Vector.Unboxed      as V
import           Graphics.Rendering.Chart

import           View.Types

--------------------------------------------------------------------------------
-- The interpreter of the display specification
--------------------------------------------------------------------------------

chartLayout :: ChartSpec -> (Int, Int) -> Layout Double Double
chartLayout plotSpec (pixelsX, _) = layout where

  ------------------------------------------------------------------------------
  -- X-axis

  -- Add x-axis
  (lowerXBound, upperXBound) = plotXRange plotSpec
  xScaledAxis = scaledAxis def (lowerXBound, upperXBound)
  addXAxis layout' = layout'
    & layout_x_axis %~
          (laxis_generate .~ xScaledAxis)
        . (laxis_title .~ "Time (s)")
        . (laxis_title_style . font_size .~ 12)
        . (laxis_style . axis_label_style . font_size .~ 12)

  -- Obtain final axes bounds
  plotXBounds = (,) <$> head <*> last
              $ view axis_grid
              $ xScaledAxis [lowerXBound, upperXBound]

  timeStepsPerPixel =
    uncurry subtract plotXBounds / (plotTimeStep plotSpec * fromIntegral pixelsX)
  compressibleTimeSteps = floor $ timeStepsPerPixel / 2

  -- Obtain bounds in terms of indices
  plotXBoundsIndices = plotXBounds
    & over _1 (max 0                . toIndex) -- inclusive
    . over _2 (min maxLength . succ . toIndex) -- inclusive
    where toIndex = plotToIndex plotSpec
          maxLength = V.length (plotSeries plotSpec) - 1

  times = V.generate (ub-lb+1) (\i -> toTime (i+lb))
    where (lb, ub) = plotXBoundsIndices
          toTime = plotToTime plotSpec

  ------------------------------------------------------------------------------
  -- Y-axis

  (lowerYBound, upperYBound) = plotYRange plotSpec
  addYAxis layout' = layout'
    & layout_y_axis %~
        (laxis_generate .~ scaledAxis def (lowerYBound, upperYBound))
      . (laxis_title .~ "Distance (mm)")
      . (laxis_title_style . font_size .~ 12)
      . (laxis_style . axis_label_style . font_size .~ 12)
    & makeYGrid (lowerYBound, upperYBound)
    where
      makeYGrid :: (Double, Double) -> Layout a Double -> Layout a Double
      makeYGrid (lb, ub)
        | yRange > 200 = setYGrid 100 []
        | yRange > 20  = setYGrid 10  [4, 4]
        | yRange > 2   = setYGrid 1   [2, 6]
        | otherwise    = setYGrid 0.1 [1, 7]
        where
          yRange = ub - lb

          setYGrid :: Double -> [Double] -> Layout a Double -> Layout a Double
          setYGrid spacing dashes =
              set (layout_y_axis . laxis_override)
                  (axis_grid .~ ticks spacing (lb, ub))
            . set (layout_y_axis . laxis_style . axis_grid_style)
                  (def & line_dashes .~ dashes
                       & line_width  .~ 1
                       & line_color  .~ opaque lightgrey)

      ticks :: Double -> (Double, Double) -> [Double]
      ticks spacing (lb, ub) =
        let lbDivs = ceiling (lb/spacing) :: Int
            ubDivs = floor   (ub/spacing) :: Int
        in  map ((*spacing) . fromIntegral) [lbDivs..ubDivs]

  ------------------------------------------------------------------------------
  -- Opposite axes

  makeVisibleRightAxis = over layout_right_axis_visibility
    ( set axis_show_line   True
    . set axis_show_ticks  True
    . set axis_show_labels False )
  makeVisibleTopAxis = over layout_top_axis_visibility
    ( set axis_show_line   True
    . set axis_show_ticks  True
    . set axis_show_labels False )

  ------------------------------------------------------------------------------
  -- Line plotters

  makeLine strokeWidth color line = makeLines strokeWidth color [line]

  makeLines strokeWidth color lineList =
    def & plot_lines_values .~ lineList
        & plot_lines_style . line_color .~ color
        & plot_lines_style . line_width .~ strokeWidth

  ------------------------------------------------------------------------------
  -- Traces

  crop :: V.Vector Double -> V.Vector Double
  crop = V.slice lb (ub-lb+1)
    where (lb, ub) = plotXBoundsIndices

  simplifySeries' :: V.Vector (Double, Double) -> [(Double, Double)]
  simplifySeries' = if compressibleTimeSteps <= 1
    then V.toList
    else simplifySeries compressibleTimeSteps

  focused  = makeLines 1 (opaque black)
           $ map simplifySeries'
           $ splitAtIndices indices
           $ V.zip times $ crop
           $ plotSeries plotSpec
    where indices = map (subtract lb)
                  $ takeWhile (<= ub)
                  $ dropWhile (< lb)
                  $ plotJumpIndices plotSpec
          (lb, ub) = plotXBoundsIndices

  makeTrace = simplifySeries' . maybe V.empty (V.zip times . crop)

  original = makeLine 1 (opaque (blend 0.38 grey black))
           $ makeTrace $ plotOriginalSeries plotSpec
  twin     = makeLine 1 (opaque (blend 0.25 grey darkblue))
           $ makeTrace $ plotTwinSeries plotSpec
  custom   = makeLine 1 (opaque (blend 0.25 grey greenyellow))
           $ makeTrace $ plotCustomSeries plotSpec

  ------------------------------------------------------------------------------
  -- Jumps

  filterJumpIndices :: [Int] -> [Int]
  filterJumpIndices = takeWhile (< ub') . dropWhile (< lb')
    where (lb, ub) = plotXBoundsIndices
          lb' = pred lb; ub' = ub

  reifyJumpInterval :: (Int, Int) -> V.Vector (Double, Double)
  reifyJumpInterval (j0, j1) = V.zip times' values'
    where j1' = succ j1
          nPoints = j1'-j0+1
          times'  = V.map toTime (V.generate nPoints (+j0))
          values' = V.slice j0 nPoints (plotSeries plotSpec)
          toTime = plotToTime plotSpec

  reifyJump :: Int -> [(Double, Double)]
  reifyJump i = [(toTime i, v V.! i), (toTime i1, v V.! i1)]
    where v = plotSeries plotSpec
          toTime = plotToTime plotSpec
          i1 = succ i

  simplifyModifiedIndices :: [Int] -> [[(Double, Double)]]
  simplifyModifiedIndices indices =
    if compressibleTimeSteps <= 1
    then map reifyJump indices
    else let simplePaths = map ( simplifySeries compressibleTimeSteps
                               . reifyJumpInterval
                               . (head &&& last) )
                         $ groupRuns indices
             unTuple (x, y) = [x, y]
         in  map unTuple $ concatMap (\xs -> zip xs (tail xs)) simplePaths

  jumps   = zipWith3 makeLine (repeat 2) jumpColours
          $ map reifyJump
          $ filterJumpIndices
          $ plotJumpIndices plotSpec

  closed  = zipWith3 makeLine (repeat 2) closedColours
          $ simplifyModifiedIndices
          $ filterJumpIndices
          $ plotModifiedIndices plotSpec

  jumpColours   = cycle [opaque magenta, opaque yellow]
  closedColours = cycle [opaque white,   opaque cyan]

  highlight = case plotHighlightRegion plotSpec of
    Nothing     -> def
    Just (l, r) ->
      def & plot_lines_limit_values .~
              [ [(LValue l, LMin), (LValue l, LMax)]
              , [(LValue r, LMin), (LValue r, LMax)] ]
          & plot_lines_style . line_color .~ opaque greenyellow
          & plot_lines_style . line_width .~ 1

  ------------------------------------------------------------------------------
  -- Jump annotation

  annotation = case plotAnnotation plotSpec of
    Nothing          -> def
    Just (x, y, str) ->
      def & plot_annotation_hanchor .~ HTA_Left
          & plot_annotation_vanchor .~ VTA_Centre
          & plot_annotation_style   .~
              (def & font_size .~ 16
                   & font_color .~ opaque greenyellow
                   & font_weight .~ FontWeightBold)
          & plot_annotation_values  .~ [(x, y, str)]

  ------------------------------------------------------------------------------
  -- Layout

  plots =
    map toPlot (
    -- background: reference traces
      [ twin, custom, original ]
    -- current trace
      ++ [ focused ]
    -- foreground: modifications and highlights
      ++ [ highlight ] ++ closed ++ jumps )
    -- annotation
      ++ [ toPlot annotation ]

  layout = def
         & layout_title .~ plotTitle plotSpec
         & layout_title_style . font_color .~ plotTitleColour plotSpec
         & layout_background . fill_color .~ plotBackgroundColour plotSpec
         & layout_plots .~ plots
         & addXAxis
         & addYAxis
         & makeVisibleRightAxis & makeVisibleTopAxis

--------------------------------------------------------------------------------
-- Down-sampling traces
--------------------------------------------------------------------------------

simplifySeries :: V.Unbox a => Int -> V.Vector (a, Double) -> [(a, Double)]
simplifySeries bucketSize path
  | V.length path <= 2 = V.toList path
  | otherwise =
  let nDivisions = (pred (V.length path)) `div` bucketSize
      indices = (V.++) (V.generate nDivisions (*bucketSize))
                       (V.fromList [ mid (nDivisions*bucketSize) (V.length path)
                                   , V.length path])
        where mid i j = (i+j) `div` 2
      buckets = V.zip indices (V.tail indices)

      minMaxAcc (minAcc, maxAcc) x =
        let minAcc' = min x minAcc
            maxAcc' = max x maxAcc
        in  minAcc' `seq` maxAcc' `seq` (minAcc', maxAcc')

      simplifySegment (start, end) =
        let (xs, ys) = V.unzip $ V.slice start (end-start) path
            y1 = V.head ys; y2 = V.last ys
            minAndMax = V.foldl' minMaxAcc (y1, y1) (V.tail ys)
            -- for cosmetics
            (y1', y2') = if y1 < y2 then      minAndMax
                                    else swap minAndMax
        in  V.fromList [(V.head xs, y1'), (V.last xs, y2')]

  in  V.toList $ V.concatMap simplifySegment buckets

--------------------------------------------------------------------------------
-- Merging modified jump indices
--------------------------------------------------------------------------------
-- For the special case when there are many modified indices in a row, as
-- results from using the interpolation brush or the 'Line' correction.

-- assumed non-empty, sorted, with unique elements
span2 :: (a -> a -> Bool) -> [a] -> ([a], [a])
span2 _ [] = ([], [])
span2 f (x:xs) = let (as, bs) = go x xs in (x:as, bs) where
  go y zzs@(z:zs)
    | f y z     = let (as, bs) = go z zs in (z:as, bs)
    | otherwise = ([], zzs)
  go _ [] = ([], [])

-- assumed non-empty, sorted, with unique elements
groupRuns :: [Int] -> [[Int]]
groupRuns [] = []
groupRuns xs =
  let (run, xs') = span2 (\i j -> succ i == j) xs
  in  if null run then [xs'] else run : groupRuns xs'

--------------------------------------------------------------------------------
-- Misc.
--------------------------------------------------------------------------------

splitAtIndices :: V.Unbox a => [Int] -> V.Vector a -> [V.Vector a]
splitAtIndices jumpIndices tracePoints =
  map getSlice $ filter (uncurry (/=)) intervals
  where
    intervals :: [(Int, Int)]
    intervals = zip (0 : map succ jumpIndices)
                    (jumpIndices ++ [V.length tracePoints - 1])
    getSlice (i0, i1) = V.slice i0 (i1-i0+1) tracePoints
