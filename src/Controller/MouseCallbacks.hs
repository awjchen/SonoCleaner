module Controller.MouseCallbacks
  ( registerMouseCallbacks
  ) where

import           Control.Arrow                ((&&&))
import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad.IO.Class       (liftIO)
import           Data.Functor                 (void)
import           Graphics.Rendering.Chart
import           Graphics.UI.Gtk              hiding (Point, set)

import           Controller.AppState
import           Controller.GUIElements
import           Controller.GUIState
import           Controller.Interpreter
import           Controller.Mouse

import           Model
import           Types.Bounds
import           Types.Indices

--------------------------------------------------------------------------------

registerMouseCallbacks :: GUIElements -> AppHandle a -> IO ()
registerMouseCallbacks guiElems appH = do
  scrollWheelZoom (imageEventBox guiElems) appH
  mouseButtonCallbacks guiElems appH

--------------------------------------------------------------------------------

scrollWheelZoom :: EventBox -> AppHandle a -> IO ()
scrollWheelZoom eventBox appH = void $ eventBox `on` scrollEvent $ do
    pickFn <- liftIO $ atomically $ getAppPickFn appH
    scrollDirection <- eventScrollDirection
    modifiers <- eventModifier
    layoutPick <- (pickFn . uncurry Point) <$> eventCoordinates

    case layoutPick of
      Just LayoutPick_PlotArea{} -> liftIO $ do
        model <- atomically $ getAppModel appH
        let (rx, ry) = getTraceBounds model ^. toViewPort . viewPortRadii
            -- hardcoded lower bounds:
            -- may need to change them depending on data resolution
            radiiBounds = ((0.02, 2*rx), (0.1, max 200 (2*ry)))
            scaleFactor = case scrollDirection of
              ScrollUp   -> recip $ sqrt 2
              ScrollDown ->         sqrt 2
              _          -> 1

        modifyAppGUIState appH $ case modifiers of
          [Control] -> -- zoom x
            over (viewBounds . toViewPort . viewPortRadii . _1)
                 (bound (fst radiiBounds) . (*scaleFactor))
          [Shift] -> -- zoom y
            over (viewBounds . toViewPort . viewPortRadii . _2)
                 (bound (snd radiiBounds) . (*scaleFactor))
          [] -> -- zoom x and y
              over (viewBounds . toViewPort . viewPortRadii . _1)
                  (bound (fst radiiBounds) . (*scaleFactor))
            . over (viewBounds . toViewPort . viewPortRadii . _2)
                  (bound (snd radiiBounds) . (*scaleFactor))
          _ -> id

      _ -> pure ()

    pure False

--------------------------------------------------------------------------------

mouseButtonCallbacks :: GUIElements -> AppHandle a -> IO ()
mouseButtonCallbacks guiElems appH = do
  lastMousePressTMVar <- atomically newEmptyTMVar :: IO (TMVar MouseEvent)

  -- Mouse button presses
  void $ imageEventBox guiElems `on` buttonPressEvent $ do
    mouseEvent <- captureMouseEvent $ getAppPickFn appH
    liftIO $ atomically $ fillTMVar lastMousePressTMVar mouseEvent
    pure False

  -- Mouse button releases
  void $ imageEventBox guiElems `on` buttonReleaseEvent $ do
    maybeMouseEvent1 <- liftIO $ atomically $ tryReadTMVar lastMousePressTMVar
    case maybeMouseEvent1 of
      Nothing -> pure False
      Just mouseEvent1 -> do
        mouseEvent2 <- captureMouseEvent $ getAppPickFn appH

        -- See Controller.Mouse for the definition of mouse gestures
        liftIO $ case interpretMouseGesture mouseEvent1 mouseEvent2 of
          Just (MouseClickLeft pt) -> doPanning appH pt

          Just (MouseClickRight pt) -> do
            currentPage' <- atomically $
              view currentPage <$> getAppGUIState appH
            case currentPage' of
              MainPage -> doSelectSingleLevelShift appH (p_x pt)
              _ -> return ()

          Just (MouseDragRightX timeInterval) -> do
            currentPage' <- atomically $
              view currentPage <$> getAppGUIState appH
            case currentPage' of
              MainPage -> doSelectMultipleLevelShifts appH timeInterval
              CropPage _ -> doCropping appH timeInterval
              _ -> return ()

          Just (MouseDragRightXY keyMod pt1 pt2) -> do
            currentPage' <- atomically $
              view currentPage <$> getAppGUIState appH
            case currentPage' of
              MainPage -> doInterpolationBrush appH keyMod pt1 pt2
              _ -> pure ()

          _ -> pure ()

        pure False

doPanning :: AppHandle a -> Point -> IO ()
doPanning appH pt = modifyAppModelGUIState appH $ \(model, guiState) ->
  let cx = bound (getTraceBounds model ^. viewBoundsX) (p_x pt)
      cy = bound (getTraceBounds model ^. viewBoundsY) (p_y pt)
  in  ( model
      , set (viewBounds . toViewPort . viewPortCenter) (cx, cy) guiState )

doSelectSingleLevelShift :: AppHandle a -> Double -> IO ()
doSelectSingleLevelShift appH time = do
  (model, levelShifts) <- atomically $
    (appModel &&& resultLevelShifts . appResults) <$> getAppState appH
  case iisFindNearestIndex (nearestSlope model time) levelShifts of
    Nothing -> return ()
    Just j ->
      let s = getCurrentState model ^. series
          t = timeAtSlope model j
          h = mid' $ over both (ivIndex s)
            $ runIndexInterval $ levelShiftEndpoints j
      in  modifyAppGUIState appH $
              set currentPage (SinglePage j)
            . set (viewBounds.toViewPort.viewPortCenter) (t, h)

doSelectMultipleLevelShifts :: AppHandle a -> (Double, Double) -> IO ()
doSelectMultipleLevelShifts appH (xLeft, xRight) = do
  (model, levelShifts) <- atomically $
    (appModel &&& resultLevelShifts . appResults) <$> getAppState appH
  let lowerTarget = nearestSlope model xLeft
      upperTarget = nearestSlope model xRight
  case iisFindIntermediateIndices1
          (IndexInterval (lowerTarget, upperTarget)) levelShifts of
      Just (js@(_:_:_)) ->
        let t = mid' $ over both (timeAtSlope model) $ (head &&& last) js
        in  modifyAppGUIState appH $
                set currentPage (MultiplePage js)
              . set (viewBounds.toViewPort.viewPortCenter._1) t
      _ -> pure ()

doCropping :: AppHandle a -> (Double, Double) -> IO ()
doCropping appH bounds = do
  model <- atomically $ getAppModel appH
  let s = getCurrentState model ^. series
      interval@(IndexInterval (l, u)) =
          iiBoundByIVector s $ IndexInterval
        $ over both (nearestPoint model) bounds
      t = mid' $ over both (timeAtPoint model) (l, u)
  if iiLength interval < 3 then pure () else
    modifyAppGUIState appH $
        set currentPage (CropPage $ Just interval)
      . set (viewBounds . toViewPort . viewPortCenter . _1) t

doInterpolationBrush :: AppHandle a -> KeyModifier -> Point -> Point -> IO ()
doInterpolationBrush appH keyMod (Point x1 y1) (Point x2 y2) = do
  model <- atomically $ getAppModel appH
  let stratum = case keyMod of
        ModControl -> ReplaceLowerData
        ModShift   -> ReplaceUpperData
        _          -> ReplaceLowerData
      interval = IndexInterval
                    (nearestPoint model x1, nearestPoint model x2)
      operator = interpolationBrushOp stratum interval (y1, y2)
  modifyAppModel appH $ applyToModel operator

mid' :: (Double, Double) -> Double
mid' (x, y) = 0.5*(x+y)

fillTMVar :: TMVar a -> a -> STM ()
fillTMVar tmvar a = do
   _ <- tryTakeTMVar tmvar
   putTMVar tmvar a

