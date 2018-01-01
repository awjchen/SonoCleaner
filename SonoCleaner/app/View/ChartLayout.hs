-- Interface to the plotting library, Chart
--
-- Defines the appearance, but not the content, of the graphical display.
-- This is the final point before which we pass control of rendering to Chart.
-- We apply here optimizations to reduce the cost of our rendering requests.

{-# LANGUAGE RecordWildCards #-}

module View.ChartLayout
  ( chartLayout
  ) where

import           Control.Arrow            ((&&&))
import           Control.Lens             hiding (indices)
import           Data.Colour              (AlphaColour, blend, opaque)
import           Data.Colour.Names
import           Data.Default
import           Data.Function            (on)
import qualified Data.IntMap.Strict       as M
import qualified Data.IntSet              as S
import           Data.List                (groupBy)
import           Data.Tuple               (swap)
import qualified Data.Vector.Unboxed      as V
import           Graphics.Rendering.Chart

import qualified Types.IndexInterval      as I
import           Types.IntMap
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
  { plotInterval         :: I.IndexInterval
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

    plotXBoundsIndices = I.fromEndpoints $ plotXBounds
      & over _1 (max 0               . toIndex) -- inclusive
      . over _2 (min maxIndex . succ . toIndex) -- inclusive
      where toIndex = plotToIndex plotSpec
            maxIndex = V.length (plotSeries plotSpec) - 1

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
traces plotSpec XAxisParameters{..} =
  -- Order matters: later items will draw over earlier items
  [ twinTrace
  , customTrace
  , originalTrace
  , focusedTrace ]
  where
    (lb, ub) = I.getEndpoints plotInterval
    croppedTimes = I.slice plotInterval $ plotTimes plotSpec

    cropIndexList :: M.IntMap Double -> [Int]
    cropIndexList = map (subtract lb) . M.keys . boundIntMap (lb, pred ub)

    simplifySeries' :: V.Vector (Double, Double) -> [(Double, Double)]
    simplifySeries' = if compressibleTimeSteps < 2
      then V.toList
      else simplifySeries compressibleTimeSteps

    makeMainTrace :: M.IntMap Double -> V.Vector Double -> [[(Double, Double)]]
    makeMainTrace splitIndices =
        map simplifySeries'
      . splitAtIndices (cropIndexList splitIndices)
      . V.zip croppedTimes
      . I.slice plotInterval

    makeOptionalTrace :: Maybe (V.Vector Double) -> [(Double, Double)]
    makeOptionalTrace =
        simplifySeries'
      . maybe V.empty (V.zip croppedTimes . I.slice plotInterval)

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
jumps plotSpec XAxisParameters{..} =
  -- Order matters: later items will draw over earlier items
  closedJumps ++ openJumps
  where
    (lb, ub) = I.getEndpoints plotInterval

    plotJump :: Int -> [(Double, Double)]
    plotJump i = [(toTime i, v V.! i), (toTime i1, v V.! i1)]
      where v = plotSeries plotSpec
            toTime = plotToTime plotSpec
            i1 = succ i

    simplifyJumps' :: [Int] -> [[(Double, Double)]]
    simplifyJumps' indices =
      if   compressibleTimeSteps < 4
      then map plotJump indices
      else simplifyJumps (plotSeries plotSpec)
                         (plotToTime plotSpec)
                         compressibleTimeSteps
                         indices

    openJumps = zipWith3 makeLine (repeat 2) openColours
              $ simplifyJumps'
              $ M.keys . boundIntMap (lb, pred ub)
              $ openJumps'
      where openJumps' = plotJumpIndices plotSpec
            openColours = drop parity $ cycle [opaque magenta, opaque yellow]
            parity = (`mod` 2) $ M.size $ fst $ M.split lb openJumps'

    closedJumps = zipWith3 makeLine (repeat 2) closedColours
                $ simplifyJumps'
                $ S.toList . boundIntSet (lb, pred ub)
                $ closedJumps'
      where closedJumps' = plotModifiedIndices plotSpec
            closedColours = drop parity $ cycle [opaque white, opaque cyan]
            parity = (`mod` 2) $ S.size $ fst $ S.split lb closedJumps'


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

simplifySeries :: V.Unbox a => Int -> V.Vector (a, Double) -> [(a, Double)]
simplifySeries bucketSize path
  | V.length path <= 2 = V.toList path
  | otherwise =
  let nDivisions = (pred (V.length path)) `div` bucketSize
      indices = fmap (*bucketSize) [0..nDivisions-1]
              ++ [ mid (nDivisions*bucketSize) (V.length path)
                 , V.length path ]
        where mid i j = (i+j) `div` 2
      buckets = zip indices (tail indices)
      slices = fmap (\(a, b) -> (a, b-a)) buckets

      simplifySegment :: V.Unbox a => V.Vector (a, Double) -> [(a, Double)]
      simplifySegment segment =
        let (xs, ys) = V.unzip segment
            minAndMax = unboxedMinAndMax ys
            -- for cosmetics
            (y1', y2') = if V.head ys < V.last ys
              then      minAndMax
              else swap minAndMax
        in  [(V.head xs, y1'), (V.last xs, y2')]

  in  concatMap (simplifySegment . (flip (uncurry V.slice) path)) slices

simplifyJumps
  :: V.Vector Double
  -> (Int -> Double)
  -> Int
  -> [Int]
  -> [[(Double, Double)]]
simplifyJumps v toTime bucketSize =
    concatMap (map plot' . evenReduce . map bounds')
  . groupBy (\a b -> b - a < bucketSize)
  where
    bounds' :: Int -> SP Int Bounds
    bounds' i = SP i (makeBounds (v V.! i) (v V.! succ i))

    union' :: SP Int Bounds -> SP Int Bounds -> SP Int Bounds
    union' (SP i0 b0) (SP _ b1) = SP i0 (boundsUnion b0 b1)

    evenReduce :: [SP Int Bounds] -> [SP Int Bounds]
    evenReduce (a:b:c:xs) = evenReduce $ (a `union'` b `union'` c):xs
    evenReduce xs         = xs

    plot' :: SP Int Bounds -> [(Double, Double)]
    plot' (SP i (Bounds l u)) = [(toTime i, l), (toTime (succ i), u)]

--------------------------------------------------------------------------------
-- Bounds
--------------------------------------------------------------------------------

data Bounds = Bounds !Double !Double

makeBounds :: Double -> Double -> Bounds
makeBounds a b = if a < b then Bounds a b else Bounds b a

boundsUnion :: Bounds -> Bounds -> Bounds
boundsUnion (Bounds l u) (Bounds l' u') = Bounds (min l l') (max u u')

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

splitAtIndices :: V.Unbox a => [Int] -> V.Vector a -> [V.Vector a]
splitAtIndices jumpIndices tracePoints =
  map getSlice $ filter (uncurry (/=)) intervals
  where
    intervals :: [(Int, Int)]
    intervals = zip (0 : map succ jumpIndices)
                    (jumpIndices ++ [V.length tracePoints - 1])
    getSlice (i0, i1) = V.slice i0 (i1-i0+1) tracePoints
