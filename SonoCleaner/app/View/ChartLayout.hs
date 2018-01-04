-- Interface to the plotting library, Chart
--
-- Defines the appearance, but not the content, of the graphical display.
-- This is the final point before which we pass control of rendering to Chart.
-- We apply here optimizations to reduce the cost of our rendering requests.

{-# LANGUAGE RecordWildCards #-}

module View.ChartLayout
  ( chartLayout
  ) where

import           Control.Lens             hiding (indices)
import           Data.Colour              (AlphaColour, blend, opaque)
import           Data.Colour.Names
import           Data.Default
import           Data.List                (groupBy)
import           Data.Semigroup
import           Data.Tuple               (swap)
import qualified Data.Vector.Unboxed      as V
import           Graphics.Rendering.Chart

import           Types.Indices
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
  , XAxisParameters plotXBoundsIndices compressibleTimeSteps)
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
      $ over _2 succ -- toIndex rounds down
      $ over both toIndex
      $ plotXBounds
    -- plotXBoundsIndices = IndexInterval $ plotXBounds
    --   & over _1 (max 0               . toIndex) -- inclusive
    --   . over _2 (min maxIndex . succ . toIndex) -- inclusive
      where toIndex = plotToIndex plotSpec
            -- maxIndex = V.length (plotSeries plotSpec) - 1

    compressibleTimeSteps = floor $ timeStepsPerPixel / 2
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
    croppedTimes = ivSlice interval $ plotTimes plotSpec

    cropIndexList :: IIntMap Index1 Double -> [Index1]
    cropIndexList = map (subtract $ iiLeft interval')
                  . iimKeys1 . iimBound interval'
      where interval' = iiDiff interval

    simplifySeries' :: IVector Index0 (Double, Double) -> [(Double, Double)]
    simplifySeries' = if compressibleTimeSteps xParams < 2
      then V.toList . unsafeRunIVector
      else simplifySeries (compressibleTimeSteps xParams)

    makeMainTrace
      :: IIntMap Index1 Double -> IVector Index0 Double -> [[(Double, Double)]]
    makeMainTrace splitIndices =
        map simplifySeries'
      . splitAtIndices (cropIndexList splitIndices)
      . ivZip croppedTimes
      . ivSlice interval

    makeOptionalTrace :: Maybe (IVector Index0 Double) -> [(Double, Double)]
    makeOptionalTrace =
        simplifySeries'
      . maybe mempty (ivZip croppedTimes . ivSlice interval)

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
jumps plotSpec xParams =
  -- Order matters: later items will draw over earlier items
  closedJumps ++ openJumps
  where
    interval' = iiDiff $ plotInterval xParams

    jumpEndpoints :: Index1 -> (Index0, Index0)
    jumpEndpoints j = runIndexInterval $ iiUndiff $ IndexInterval (j, j)

    plotJump :: Index1 -> [(Double, Double)]
    plotJump j = [(toTime i0, ivIndex v i0), (toTime i1, ivIndex v i1)]
      where (i0, i1) = jumpEndpoints j
            v = plotSeries plotSpec
            toTime = plotToTime plotSpec

    simplifyJumps' :: [Index1] -> [[(Double, Double)]]
    simplifyJumps' indices =
      if   compressibleTimeSteps xParams < 4
      then map plotJump indices
      else simplifyJumps (plotSeries plotSpec)
                         (plotToTime plotSpec)
                         (compressibleTimeSteps xParams)
                         indices

    openJumps = zipWith3 makeLine (repeat 2) openColours
              $ simplifyJumps'
              $ iimKeys1 . iimBound interval'
              $ openJumps'
      where openJumps' = plotJumpIndices plotSpec
            openColours = drop parity $ cycle [opaque magenta, opaque yellow]
            parity = (`mod` 2) $ iimSize
                   $ fst $ iimSplit (iiLeft interval') openJumps'

    closedJumps = zipWith3 makeLine (repeat 2) closedColours
                $ simplifyJumps'
                $ iisToList1 . iisBound interval'
                $ closedJumps'
      where closedJumps' = plotModifiedIndices plotSpec
            closedColours = drop parity $ cycle [opaque white, opaque cyan]
            parity = (`mod` 2) $ iisSize
                   $ fst $ iisSplit (iiLeft interval') closedJumps'


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
-- Downsampling
--------------------------------------------------------------------------------

simplifySeries
  :: V.Unbox a => Int -> IVector Index0 (a, Double) -> [(a, Double)]
simplifySeries bucketSize' path
  | ivLength path <= 2 = V.toList $ unsafeRunIVector path
  | otherwise =
  -- casting `bucketSize'` is okay because it is "affine-y":
  -- it will only be compared against the difference of two `Index0`.
  let bucketSize = index0 bucketSize'
      len = ivLength path
      -- pred beacuse the last two buckets are added later
      nDivisions = (pred len) `div` bucketSize
      indices = fmap (*bucketSize) [0..nDivisions-1]
                ++ [ mid (nDivisions*bucketSize) len , len ]
        where mid i j = (i+j) `div` 2
      buckets = map IndexInterval $ zip indices (map pred $ tail indices)
      -- slices = fmap (\(a, b) -> (a, b-a)) buckets

      simplifySegment
        :: V.Unbox a => IVector Index0 (a, Double) -> [(a, Double)]
      simplifySegment segment =
        let (xs, ys) = ivUnzip segment
            minAndMax = ivMinMax ys
            xs' = unsafeRunIVector xs
            ys' = unsafeRunIVector ys
            -- for cosmetics
            (y1', y2') = if V.head ys' < V.last ys'
              then      minAndMax
              else swap minAndMax
        in  [(V.head xs', y1'), (V.last xs', y2')]

  in  concatMap (simplifySegment . (`ivSlice` path)) buckets

simplifyJumps
  :: IVector Index0 Double
  -> (Index0 -> Double)
  -> Int
  -> [Index1]
  -> [[(Double, Double)]]
simplifyJumps v toTime bucketSize' =
    concatMap (map plot' . evenConcat . map bounds')
  . groupBy (\a b -> b - a < bucketSize)
  where
    -- casting `bucketSize'` is okay because it is "affine-y":
    -- it will only be compared against the difference of two `Index1`.
    bucketSize = index1 bucketSize'

    jumpEndpoints :: Index1 -> (Index0, Index0)
    jumpEndpoints j = runIndexInterval $ iiUndiff $ IndexInterval (j, j)

    bounds' :: Index1 -> SP Index1 Bounds
    bounds' j = SP j (makeBounds (ivIndex v i0) (ivIndex v (succ i1)))
      where (i0, i1) = jumpEndpoints j

    union' :: SP Index1 Bounds -> SP Index1 Bounds -> SP Index1 Bounds
    union' (SP i0 b0) (SP _ b1) = SP i0 (b0 <> b1)

    evenConcat :: [SP Index1 Bounds] -> [SP Index1 Bounds]
    evenConcat (a:b:c:xs) = evenConcat $ (a `union'` b `union'` c):xs
    evenConcat xs         = xs

    plot' :: SP Index1 Bounds -> [(Double, Double)]
    plot' (SP j (Bounds l u)) = [(toTime i0, l), (toTime (succ i1), u)]
      where (i0, i1) = jumpEndpoints j

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

splitAtIndices
  :: V.Unbox a => [Index1] -> IVector Index0 a -> [IVector Index0 a]
splitAtIndices jumpIndices v =
  map (`ivSlice` v) $ filter (not . iiIsSingleton) intervals
  where
    (firstCut, lastCut) =
      runIndexInterval $ iiGrow $ iiDiff $ IndexInterval (0, ivLength v)
    intervals :: [IndexInterval Index0]
    intervals = map (iiShrink . iiUndiff . IndexInterval)
              $ zip (firstCut : jumpIndices) (jumpIndices ++ [lastCut])
    -- getSlice (i0, i1) = V.slice i0 (i1-i0+1) v
