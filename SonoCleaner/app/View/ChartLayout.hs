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
import           Data.Colour              (AlphaColour, blend, opaque)
import           Data.Colour.Names
import           Data.Default
import           Data.Tuple               (swap)
import qualified Data.Vector.Unboxed      as V
import           Graphics.Rendering.Chart

import           Types.Series             (unboxedMinAndMax)
import           View.Types

--------------------------------------------------------------------------------
-- The interpreter of the display specification
--------------------------------------------------------------------------------

chartLayout :: ChartSpec -> (Int, Int) -> Layout Double Double
chartLayout plotSpec (pixelsX, _) = layout where

  (addXAxis, xAxisParameters) = xAxis pixelsX plotSpec
  addYAxis                    = yAxis plotSpec

  tracePlotLines = traces plotSpec xAxisParameters
  jumpsPlotLines = jumps  plotSpec xAxisParameters

  highlight = highlightInterval (plotHighlightRegion plotSpec)

  annotation = jumpAnnotation (plotAnnotation plotSpec)

  plots =
    -- Order matters: later items will draw over earlier items
       map toPlot tracePlotLines
    ++ map toPlot jumpsPlotLines
    ++ [toPlot highlight, toPlot annotation]

  layout = def
    & layout_title .~ plotTitle plotSpec
    & layout_title_style . font_color .~ plotTitleColour plotSpec
    & layout_background . fill_color .~ plotBackgroundColour plotSpec
    & layout_plots .~ plots
    & addXAxis
    & addYAxis


--------------------------------------------------------------------------------
-- Component interpreters
--------------------------------------------------------------------------------

data XAxisParameters = XAxisParameters
  { boundsIndices         :: (Int, Int)
  , compressibleTimeSteps :: Int
  , times                 :: V.Vector Double
  }


xAxis
  :: Int
  -> ChartSpec
  -> ( Layout Double y -> Layout Double y
     , XAxisParameters )
xAxis pixelsX plotSpec =
  ( axis . oppositeAxis
  , XAxisParameters plotXBoundsIndices compressibleTimeSteps times)
  where
    (lb, ub) = plotXRange plotSpec
    xScaledAxis = scaledAxis def (lb, ub)

    axis = layout_x_axis %~
        (laxis_generate .~ xScaledAxis)
      . (laxis_title .~ "Time (s)")
      . (laxis_title_style . font_size .~ 12)
      . (laxis_style . axis_label_style . font_size .~ 12)

    oppositeAxis = over layout_right_axis_visibility
      ( set axis_show_line   True
      . set axis_show_ticks  True
      . set axis_show_labels False )

    -- Obtain final x-axis bounds, determined by Chart
    plotXBounds = (,) <$> head <*> last
                $ view axis_grid
                $ xScaledAxis [lb, ub]

    plotXBoundsIndices = plotXBounds
      & over _1 (max 0                . toIndex) -- inclusive
      . over _2 (min maxLength . succ . toIndex) -- inclusive
      where toIndex = plotToIndex plotSpec
            maxLength = V.length (plotSeries plotSpec) - 1

    compressibleTimeSteps = floor $ timeStepsPerPixel / 2
      where
        timeStepsPerPixel = timeSteps / fromIntegral pixelsX
        timeSteps = uncurry subtract plotXBounds / plotTimeStep plotSpec

    times = V.generate (i1-i0+1) (\i -> toTime (i+i0))
      where (i0, i1) = plotXBoundsIndices
            toTime = plotToTime plotSpec


yAxis :: ChartSpec -> Layout x Double -> Layout x Double
yAxis plotSpec = axis . oppositeAxis . grid
  where
    (lb, ub) = plotYRange plotSpec
    yScaledAxis = scaledAxis def (lb, ub)

    axis :: Layout x Double -> Layout x Double
    axis = layout_y_axis %~
        (laxis_generate .~ yScaledAxis)
      . (laxis_title .~ "Distance (mm)")
      . (laxis_title_style . font_size .~ 12)
      . (laxis_style . axis_label_style . font_size .~ 12)

    oppositeAxis :: Layout x Double -> Layout x Double
    oppositeAxis = over layout_top_axis_visibility
      ( set axis_show_line   True
      . set axis_show_ticks  True
      . set axis_show_labels False )

    grid :: Layout x Double -> Layout x Double
    grid
      | yRange > 200 = gridTicks 100 . gridDashes []
      | yRange > 20  = gridTicks 10  . gridDashes [4, 4]
      | yRange > 2   = gridTicks 1   . gridDashes [2, 6]
      | otherwise    = gridTicks 0.1 . gridDashes [1, 7]
      where
        yRange = ub - lb

        gridTicks :: Double -> Layout x Double -> Layout x Double
        gridTicks spacing =
          set (layout_y_axis . laxis_override) (set axis_grid ticks)
            where
              lbDivs = ceiling (lb/spacing) :: Int
              ubDivs = floor   (ub/spacing) :: Int
              ticks = map ((*spacing) . fromIntegral) [lbDivs..ubDivs]

        gridDashes :: [Double] -> Layout x Double -> Layout x Double
        gridDashes dashes =
          set (layout_y_axis . laxis_style . axis_grid_style)
              (def & line_dashes .~ dashes
                   & line_width  .~ 1
                   & line_color  .~ opaque lightgrey)


traces :: ChartSpec -> XAxisParameters -> [PlotLines Double Double]
traces plotSpec xAxisParams =
  -- Order matters: later items will draw over earlier items
  [ twinTrace
  , customTrace
  , originalTrace
  , focusedTrace ]
  where
    (lb, ub) = boundsIndices xAxisParams

    cropTrace :: V.Vector Double -> V.Vector Double
    cropTrace = V.slice lb (ub-lb+1)

    -- assumed sorted
    cropIndexList :: [Int] -> [Int]
    cropIndexList = map (subtract lb) . takeWhile (<= ub) . dropWhile (< lb)

    simplifySeries' :: V.Vector (Double, Double) -> [(Double, Double)]
    simplifySeries' = if compressibleTimeSteps xAxisParams <= 1
      then V.toList
      else simplifySeries (compressibleTimeSteps xAxisParams)

    makeMainTrace :: [Int] -> V.Vector Double -> [[(Double, Double)]]
    makeMainTrace splitIndices =
        map simplifySeries'
      . splitAtIndices (cropIndexList splitIndices)
      . V.zip (times xAxisParams)
      . cropTrace

    makeOptionalTrace :: Maybe (V.Vector Double) -> [(Double, Double)]
    makeOptionalTrace =
      simplifySeries' . maybe V.empty (V.zip (times xAxisParams) . cropTrace)

    focusedTrace =
        makeLines 1 (opaque black)
      $ makeMainTrace (plotJumpIndices plotSpec) (plotSeries plotSpec)

    originalTrace =
        makeLine 1 (opaque (blend 0.38 grey black))
      $ makeOptionalTrace $ plotOriginalSeries plotSpec
    twinTrace =
        makeLine 1 (opaque (blend 0.25 grey darkblue))
      $ makeOptionalTrace $ plotTwinSeries plotSpec
    customTrace =
        makeLine 1 (opaque (blend 0.25 grey greenyellow))
      $ makeOptionalTrace $ plotCustomSeries plotSpec


jumps :: ChartSpec -> XAxisParameters -> [PlotLines Double Double]
jumps plotSpec xAxisParams =
  -- Order matters: later items will draw over earlier items
  closedJumps ++ openJumps
  where
    (lb, ub) = boundsIndices xAxisParams

    filterJumpIndices :: [Int] -> [Int]
    filterJumpIndices = takeWhile (< ub') . dropWhile (< lb')
      where lb' = pred lb; ub' = ub

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
      if compressibleTimeSteps xAxisParams <= 1
      then map reifyJump indices
      else let simplePaths =
                   map ( simplifySeries (compressibleTimeSteps xAxisParams)
                 . reifyJumpInterval
                 . (head &&& last) )
                 $ groupRuns indices
               unTuple (x, y) = [x, y]
          in  map unTuple $ concatMap (\xs -> zip xs (tail xs)) simplePaths

    openJumps =
        zipWith3 makeLine (repeat 2) openColours
      $ map reifyJump
      $ filterJumpIndices
      $ plotJumpIndices plotSpec
      where openColours   = cycle [opaque magenta, opaque yellow]

    closedJumps =
        zipWith3 makeLine (repeat 2) closedColours
      $ simplifyModifiedIndices
      $ filterJumpIndices
      $ plotModifiedIndices plotSpec
      where closedColours = cycle [opaque white, opaque cyan]


highlightInterval :: Maybe (Double, Double) -> PlotLines Double y
highlightInterval Nothing = def
highlightInterval (Just (l, r)) =
  def & plot_lines_limit_values .~
          [ [(LValue l, LMin), (LValue l, LMax)]
          , [(LValue r, LMin), (LValue r, LMax)] ]
      & plot_lines_style . line_color .~ opaque greenyellow
      & plot_lines_style . line_width .~ 1


jumpAnnotation :: Maybe (Double, Double, String) -> PlotAnnotation Double Double
jumpAnnotation Nothing = def
jumpAnnotation (Just (x, y, str)) =
  def & plot_annotation_hanchor .~ HTA_Left
      & plot_annotation_vanchor .~ VTA_Centre
      & plot_annotation_style   .~
          (def & font_size .~ 16
                & font_color .~ opaque greenyellow
                & font_weight .~ FontWeightBold)
      & plot_annotation_values  .~ [(x, y, str)]


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

      simplifySegment (start, end) =
        let (xs, ys) = V.unzip $ V.slice start (end-start) path
            minAndMax = unboxedMinAndMax ys
            -- for cosmetics
            (y1', y2') = if V.head ys < V.last ys
              then      minAndMax
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
-- Utility
--------------------------------------------------------------------------------

makeLine :: Double -> AlphaColour Double -> [(x, y)] -> PlotLines x y
makeLine strokeWidth color line = makeLines strokeWidth color [line]

makeLines :: Double -> AlphaColour Double -> [[(x, y)]] -> PlotLines x y
makeLines strokeWidth color lineList =
  def & plot_lines_values .~ lineList
      & plot_lines_style . line_color .~ color
      & plot_lines_style . line_width .~ strokeWidth

splitAtIndices :: V.Unbox a => [Int] -> V.Vector a -> [V.Vector a]
splitAtIndices jumpIndices tracePoints =
  map getSlice $ filter (uncurry (/=)) intervals
  where
    intervals :: [(Int, Int)]
    intervals = zip (0 : map succ jumpIndices)
                    (jumpIndices ++ [V.length tracePoints - 1])
    getSlice (i0, i1) = V.slice i0 (i1-i0+1) tracePoints
