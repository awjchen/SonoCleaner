-- This module defines the appearance of the graphical display. It is an
-- interface to the plotting library 'Chart'.

-- This is the final point before which we pass control of rendering to Chart,
-- so we now apply optimizations to reduce our rendering requests.

module View.ChartLayout
  ( LayoutMode (..)
  , chartLayout
  ) where

import           Control.Lens             hiding (indices)
import           Data.Colour              (AlphaColour, blend, opaque)
import           Data.Colour.Names
import           Data.Default
import           Data.List                (groupBy)
import           Data.Semigroup
import           Data.Tuple               (swap)
import qualified Data.Vector.Unboxed      as V
import           Data.Void
import           Graphics.Rendering.Chart

import           Types.Indices
import           View.Types

--------------------------------------------------------------------------------
-- The "interpreter" of the display specification
--------------------------------------------------------------------------------

data LayoutMode
  = RegularMode
  | ScreenshotMode

chartLayout :: LayoutMode -> ChartSpec -> (Int, Int) -> Layout Double Double
chartLayout layoutMode plotSpec (pixelsX, _) = layout where

  (addXAxis, xAxisParameters) = xAxis pixelsX plotSpec
  addYAxis                    = yAxis plotSpec

  tracePlotLines = traces plotSpec xAxisParameters
  segmentsPlotLines = segments  plotSpec xAxisParameters

  highlight = highlightInterval $ over (_Just . both) (plotToTime plotSpec)
            $ plotHighlightRegion plotSpec

  annotation = segmentAnnotation $ over (_Just . _1) (plotToTime plotSpec)
             $ (plotAnnotation plotSpec)

  plots =
    -- Order matters: later items will draw over earlier items
       map toPlot tracePlotLines
    ++ map toPlot segmentsPlotLines
    ++ [toPlot highlight, toPlot annotation]

  title = case layoutMode of
    RegularMode    -> plotTitle plotSpec
    ScreenshotMode -> ""

  layout = def
    & layout_title .~ title
    & layout_title_style . font_color .~ plotTitleColour plotSpec
    & layout_background . fill_color .~ plotBackgroundColour plotSpec
    & layout_plots .~ plots
    & addXAxis
    & addYAxis


--------------------------------------------------------------------------------
-- Component "interpreters"
--------------------------------------------------------------------------------

data XAxisParameters = XAxisParameters
  { plotInterval          :: IndexInterval Index0
  , compressibleTimeSteps :: Int
  }


xAxis
  :: Int
  -> ChartSpec
  -> ( Layout Double y -> Layout Double y
     , XAxisParameters )
xAxis pixelsX plotSpec =
  ( axis . oppositeAxis
  , XAxisParameters plotXBoundsIndices compressibleTimeSteps')
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

    plotXBoundsIndices =
        iiBoundByIVector (plotSeries plotSpec)
      $ IndexInterval
      $ over both toIndex
      $ plotXBounds
      where toIndex = plotToIndex plotSpec

    compressibleTimeSteps' = floor $ timeStepsPerPixel / 2
      where
        timeStepsPerPixel = timeSteps / fromIntegral pixelsX
        timeSteps = uncurry subtract plotXBounds / plotTimeStep plotSpec


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
traces plotSpec xParams =
  -- Order matters: later items will draw over earlier items
  [ twinTrace
  , customTrace
  , originalTrace
  , focusedTrace ]
  where
    interval = plotInterval xParams

    simplifySeries' :: IVector Void (Double, Double) -> [(Double, Double)]
    simplifySeries' = if compressibleTimeSteps xParams < 2
      then V.toList . unsafeRunIVector
      else simplifySeries (compressibleTimeSteps xParams)

    makeOptionalTrace :: Maybe (IVector Index0 Double) -> [(Double, Double)]
    makeOptionalTrace = maybe []
      (simplifySeries' . ivSlice interval . ivZip (plotTimes plotSpec))

    focusedTrace =
        makeLines 1 (opaque black)
      $ map simplifySeries'
      $ splitAtSegments interval (plotLevelShifts plotSpec)
      $ ivZip (plotTimes plotSpec) (plotSeries plotSpec)

    originalTrace =
        makeLine 1 (opaque (blend 0.38 grey black))
      $ makeOptionalTrace $ plotOriginalSeries plotSpec
    twinTrace =
        makeLine 1 (opaque (blend 0.25 grey darkblue))
      $ makeOptionalTrace $ plotTwinSeries plotSpec
    customTrace =
        makeLine 1 (opaque (blend 0.25 grey greenyellow))
      $ makeOptionalTrace $ plotCustomSeries plotSpec


segments :: ChartSpec -> XAxisParameters -> [PlotLines Double Double]
segments plotSpec xParams =
  -- Order matters: later items will draw over earlier items
  closedSegments ++ openSegments
  where
    interval' = iiDiff $ plotInterval xParams

    plotSegment :: Index1 -> [(Double, Double)]
    plotSegment j = [(toTime i0, ivIndex v i0), (toTime i1, ivIndex v i1)]
      where (i0, i1) = runIndexInterval $ levelShiftEndpoints j
            v = plotSeries plotSpec
            toTime = plotToTime plotSpec

    simplifySegments' :: [Index1] -> [[(Double, Double)]]
    simplifySegments' indices =
      if   compressibleTimeSteps xParams < 4
      then map plotSegment indices
      else simplifySegments (plotSeries plotSpec)
                            (plotToTime plotSpec)
                            (compressibleTimeSteps xParams)
                            indices

    openSegments = zipWith3 makeLine (repeat 2) openColours
                 $ simplifySegments'
                 $ iisToList1 . iisBound interval'
                 $ openSegments'
      where openSegments' = plotLevelShifts plotSpec
            openColours = drop parity $ cycle [opaque magenta, opaque yellow]
            parity = (`mod` 2) $ iisSize
                   $ fst $ iisSplit (iiLeft interval') openSegments'

    closedSegments = zipWith3 makeLine (repeat 2) closedColours
                   $ simplifySegments'
                   $ iisToList1 . iisBound interval'
                   $ closedSegments'
      where closedSegments' = plotModifiedSegments plotSpec
            closedColours = drop parity $ cycle [opaque white, opaque cyan]
            parity = (`mod` 2) $ iisSize
                   $ fst $ iisSplit (iiLeft interval') closedSegments'


highlightInterval :: Maybe (Double, Double) -> PlotLines Double y
highlightInterval Nothing = def
highlightInterval (Just (l, r)) =
  def & plot_lines_limit_values .~
          [ [(LValue l, LMin), (LValue l, LMax)]
          , [(LValue r, LMin), (LValue r, LMax)] ]
      & plot_lines_style . line_color .~ opaque greenyellow
      & plot_lines_style . line_width .~ 1


segmentAnnotation
  :: Maybe (Double, Double, String) -> PlotAnnotation Double Double
segmentAnnotation Nothing = def
segmentAnnotation (Just (x, y, str)) =
  def & plot_annotation_hanchor .~ HTA_Left
      & plot_annotation_vanchor .~ VTA_Centre
      & plot_annotation_style   .~
          (def & font_size .~ 16
                & font_color .~ opaque greenyellow
                & font_weight .~ FontWeightBold)
      & plot_annotation_values  .~ [(x, y, str)]


--------------------------------------------------------------------------------
-- Downsampling
--------------------------------------------------------------------------------

simplifySeries
  :: V.Unbox a => Int -> IVector Void (a, Double) -> [(a, Double)]
simplifySeries bucketSize path
  | V.length path' <= 2 = V.toList $ unsafeRunIVector path
  | otherwise = concatMap (simplifySegment . (`iiSlice` path')) buckets
  where
    path' = unsafeRunIVector path
    len = V.length path'
    nDivisions = (pred len) `div` bucketSize -- pred beacuse the last two buckets are added later
    indices = fmap (*bucketSize) [0..nDivisions-1]
              ++ [ mid (nDivisions*bucketSize) len , len ]
      where mid i j = (i+j) `div` 2
    buckets = map IndexInterval $ zip indices (map pred $ tail indices)

    simplifySegment
      :: V.Unbox a => V.Vector (a, Double) -> [(a, Double)]
    simplifySegment segment =
      let (xs, ys) = V.unzip segment
          minAndMax = minMax ys
          -- for cosmetics
          (y1, y2) = if V.head ys < V.last ys
            then      minAndMax
            else swap minAndMax
      in  [(V.head xs, y1), (V.last xs, y2)]

simplifySegments
  :: IVector Index0 Double
  -> (Index0 -> Double)
  -> Int
  -> [Index1]
  -> [[(Double, Double)]]
simplifySegments v toTime bucketSize =
    concatMap (map plot' . evenConcat . map bounds')
  . groupBy (\a b -> b `iMinus` a < bucketSize)
  where

    bounds' :: Index1 -> SP Index1 Bounds
    bounds' j = SP j (makeBounds (ivIndex v i0) (ivIndex v i1))
      where (i0, i1) = runIndexInterval $ levelShiftEndpoints j

    union' :: SP Index1 Bounds -> SP Index1 Bounds -> SP Index1 Bounds
    union' (SP i0 b0) (SP _ b1) = SP i0 (b0 <> b1)

    evenConcat :: [SP Index1 Bounds] -> [SP Index1 Bounds]
    evenConcat (a:b:c:xs) = evenConcat $ (a `union'` b `union'` c):xs
    evenConcat xs         = xs

    plot' :: SP Index1 Bounds -> [(Double, Double)]
    plot' (SP j (Bounds l u)) = [(toTime i0, l), (toTime i1, u)]
      where (i0, i1) = runIndexInterval $ levelShiftEndpoints j

--------------------------------------------------------------------------------
-- Bounds
--------------------------------------------------------------------------------

data Bounds = Bounds !Double !Double

makeBounds :: Double -> Double -> Bounds
makeBounds a b = if a < b then Bounds a b else Bounds b a

instance Semigroup Bounds where
  (Bounds l u) <> (Bounds l' u') = Bounds (min l l') (max u u')

--------------------------------------------------------------------------------
-- Utility
--------------------------------------------------------------------------------

data SP a b = SP !a !b

makeLine :: Double -> AlphaColour Double -> [(x, y)] -> PlotLines x y
makeLine strokeWidth color line = makeLines strokeWidth color [line]

makeLines :: Double -> AlphaColour Double -> [[(x, y)]] -> PlotLines x y
makeLines strokeWidth color lineList =
  def & plot_lines_values .~ lineList
      & plot_lines_style . line_color .~ color
      & plot_lines_style . line_width .~ strokeWidth

splitAtSegments
  :: V.Unbox a
  => IndexInterval Index0
  -> IIntSet Index1
  -> IVector Index0 a
  -> [IVector Void a]
splitAtSegments cropInterval cutSegments v =
  map (`ivSlice` v) $ filter (not . iiIsSingleton) intervals
  where
    diffCropInterval = iiDiff cropInterval
    (firstCut, lastCut) = runIndexInterval $ iiGrow $ diffCropInterval
    boundedCutSegments = iisToList1 $ iisBound diffCropInterval cutSegments
    intervals :: [IndexInterval Index0]
    intervals = map (iiShrink . iiUndiff . IndexInterval)
              $ zip (firstCut : boundedCutSegments)
                    (boundedCutSegments ++ [lastCut])
