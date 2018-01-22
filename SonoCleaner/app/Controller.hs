-- The 'Controller' of "Model, View, Controller"
--
-- In this program, the Controller mediates all interaction with the user. It
-- collects the user's inputs and transforms them into requests to the Model and
-- View. A part of this is achieved by maintaining synchronization between the
-- state of GTK+ 3 widgets and an instance of a `GUIState` type.
--
-- This module deinfes `controllerMain`, which is basically the entry point of
-- the program.

{-# LANGUAGE LambdaCase #-}

module Controller
  ( controllerMain
  ) where

import           Control.Arrow                          ((&&&))
import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad.IO.Class                 (liftIO)
import           Control.Monad.Trans.Except
import           Data.Default
import           Graphics.Rendering.Chart
import qualified Graphics.Rendering.Chart.Backend.Cairo as Chart
import           Graphics.UI.Gtk                        hiding (set)
import           System.FilePath                        (dropExtension,
                                                         splitFileName)

import           Controller.GenericCallbacks
import           Controller.Glade
import           Controller.GUIElements
import           Controller.GUIState
import           Controller.Interpreter
import           Controller.Keybindings
import           Controller.Mouse
import           Controller.Sensitivity
import           Controller.Util
import           Model
import           Types.Bounds
import           Types.Indices
import           View

controllerMain :: IO ()
controllerMain = do

--------------------------------------------------------------------------------
-- Define mutable state
--------------------------------------------------------------------------------

  -- `modelTVar` is the single, global representation of the user's data.
  -- Is is filled with a proper value when a .ssa data file is opened.
  modelTVar <- newTVarIO initUndefinedModel :: IO (TVar Model)

  guiStateTVar <- newTVarIO def :: IO (TVar GUIState)

--------------------------------------------------------------------------------
-- Import GUI (from Glade)
--------------------------------------------------------------------------------

  _ <- initGUI

  builder <- builderNew
  builderAddFromString builder builderString

  guiElems <- importGUIElements builder

  windowSetDefaultSize (controllerWindow guiElems) 1024 640

--------------------------------------------------------------------------------
-- Setup "subprocesses"
--------------------------------------------------------------------------------

  -- for computations
  (initializeInterpreter, getChart, getMatches, getLevelShifts, getNewModel)
    <- setupInterpreter

  -- for rendering
  (pickFnTVar, requestDraw, requestScreenshot)
    <- setupRenderer (controllerWindow guiElems) (image guiElems)
                     (controllerWindowBox guiElems)

--------------------------------------------------------------------------------
-- Locking mechanism for GUI state synchronization and canvas updates
--------------------------------------------------------------------------------

-- Disclaimer: I am not very familliar with Gtk+ 3, and I don't know what I'm
-- doing.

-- For some reason, the triggering of one widget is often accompanied by the
-- triggering of other widgets, sometimes by design in Gtk+ 3 and sometimes
-- because of our synchronization of the Gtk+3 widget states with our `GUIState`
-- (or so I suspect). In any case, it would be expensive and redundant if each
-- of the widgets were to demand a canvas redraw. We prevent this by wrapping
-- every callback with a function `withUpdate`, which obtains a lock that
-- prevents both GUI state synchronization and canvas updates until all of the
-- callbacks have finished executing.

  withUpdate <- do
    updateLock <- atomically $ newTMVar () :: IO (TMVar ())

    let withUpdate :: IO () -> IO ()
        withUpdate action = do
          maybeLock <- atomically $ tryTakeTMVar updateLock
          action
          case maybeLock of
            Nothing -> return ()
            Just () -> update >> atomically (putTMVar updateLock ())
          where
            update :: IO ()
            update = do
              (model, guiState) <- atomically $
                (,) <$> readTVar modelTVar <*> readTVar guiStateTVar

              setGUIParameters guiElems guiState
              setGUISensitivity guiElems model guiState
              atomically $ getChart model guiState >>= requestDraw

    pure withUpdate

--------------------------------------------------------------------------------
-- Keyboard shortcuts
--------------------------------------------------------------------------------

  registerKeyboardShortcuts guiElems guiStateTVar

--------------------------------------------------------------------------------
-- Generic callbacks
--------------------------------------------------------------------------------
-- Callbacks requiring only basic functionality are defined within a more
-- structured and restrictive environemnt. In particular, all callbacks that
-- update parameters in `GUIState` are defined here in `registerCallbacks`.

  registerCallbacks guiElems modelTVar guiStateTVar withUpdate

--------------------------------------------------------------------------------
-- Special callbacks
--------------------------------------------------------------------------------
-- Callbacks requiring more than basic functionality are defined by hand.

  -- On attempting to close either the display or controller window, quit the
  -- program, but only after asking for confirmation.

  let quitWithConfirmation :: EventM EAny Bool
      quitWithConfirmation = liftIO $ do
        isResponseConfirm <-
          confirmDialog (controllerWindow guiElems) "Exit the program?"
        if isResponseConfirm
          then mainQuit >> return False
          else return True

  _ <- controllerWindow guiElems `on` deleteEvent $ quitWithConfirmation

  -- Saving .ssa files

  fileChooserSaveDialog <- do
    fc <- fileChooserDialogNew (Just "Save an .ssa file")
                               (Just (controllerWindow guiElems))
                               FileChooserActionSave
                               [   ("Cancel" :: String, ResponseCancel)
                                 , ("Save"   :: String, ResponseAccept) ]

    fileChooserSetSelectMultiple          fc False
    fileChooserSetDoOverwriteConfirmation fc True

    pure fc

  _ <- saveButton guiElems `on` buttonActivated $ do
    (originalDirectory, originalFileName) <-
      splitFileName . getFilePath <$> atomically (readTVar modelTVar)
    _ <- fileChooserSetCurrentFolder fileChooserSaveDialog originalDirectory
    fileChooserSetCurrentName fileChooserSaveDialog
      ("output_" ++ originalFileName)

    responseID <- dialogRun fileChooserSaveDialog
    widgetHide fileChooserSaveDialog

    case responseID of
      ResponseAccept -> do
        mFilePath <- fileChooserGetFilename fileChooserSaveDialog
        case mFilePath of
          Nothing -> return ()
          Just filePath -> guiRunExceptT (controllerWindow guiElems) $ do
                (liftIO $ atomically $ readTVar modelTVar)
            >>= saveSSAFile filePath
            >>= (liftIO . messageDialog (controllerWindow guiElems))
      _ -> return ()

  -- Opening .ssa files

  fileChooserOpenDialog <- do
    fc <- fileChooserDialogNew (Just "Open an .ssa file")
                               (Just (controllerWindow guiElems))
                               FileChooserActionOpen
                               [   ("Cancel" :: String, ResponseCancel)
                                 , ("Open"   :: String, ResponseAccept) ]

    fileChooserSetSelectMultiple fc False

    fileFilter <- fileFilterNew
    fileFilterAddPattern fileFilter ("*.ssa" :: String)
    fileChooserSetFilter fc fileFilter

    pure fc

  _ <- openButton guiElems `on` buttonActivated $ do
    responseID <- dialogRun fileChooserOpenDialog
    widgetHide fileChooserOpenDialog

    case responseID of
      ResponseAccept -> guiRunExceptT (controllerWindow guiElems) $ do
        mFilePath <- liftIO $ fileChooserGetFilename fileChooserOpenDialog
        case mFilePath of
          Nothing -> ExceptT $ return $ Left "Could not get any files."
          Just filePath -> do
            modelOld <- liftIO $ atomically $ readTVar modelTVar
            model <- loadSSAFile filePath modelOld
            liftIO $ withUpdate $ do
              atomically $ do
                guiState <- readTVar guiStateTVar
                writeTVar modelTVar model
                writeTVar guiStateTVar $ guiState &
                  setDefaultViewBounds model
                initializeInterpreter model guiState

              setComboBoxTextLabels ("None" : getLabels model)
                                    (referenceTraceComboBoxText guiElems)

              widgetShowAll (controllerWindow guiElems)

      _ -> return ()

  -- Screenshots

  fileChooserScreenshotDialog <- do
    fc <- fileChooserDialogNew (Just "Save a screenshot")
                               (Just (controllerWindow guiElems))
                               FileChooserActionSave
                               [   ("Cancel" :: String, ResponseCancel)
                                 , ("Save"   :: String, ResponseAccept) ]

    fileChooserSetSelectMultiple          fc False
    fileChooserSetDoOverwriteConfirmation fc True

    pure fc

  _ <- screenshotSaveButton guiElems `on` buttonActivated $ do
    (model, guiState, chartSpec) <- atomically $ do
      model <- readTVar modelTVar
      guiState <- readTVar guiStateTVar
      chartSpec <- getChart model guiState
      pure (model, guiState, chartSpec)
    let (originalDirectory, originalFileName) =
          splitFileName $ getFilePath model
        extension = guiState ^. screenshotFileFormat . _2
        extensionString = case extension of
          Chart.PNG -> "png"
          Chart.SVG -> "svg"
          Chart.PS  -> "ps"
          Chart.PDF -> "pdf"
    _ <- fileChooserSetCurrentFolder
           fileChooserScreenshotDialog originalDirectory
    fileChooserSetCurrentName fileChooserScreenshotDialog $ concat $
      [ "screenshot_"
      , dropExtension originalFileName
      , "_"
      , getLabel model
      , "."
      , extensionString
      ]

    responseID <- dialogRun fileChooserScreenshotDialog
    widgetHide fileChooserScreenshotDialog

    case responseID of
      ResponseAccept ->
        fileChooserGetFilename fileChooserScreenshotDialog >>= \case
          Just filePath -> requestScreenshot filePath extension chartSpec
          Nothing -> return ()
      _ -> return ()

  -- Run the automated procedure when switching to the 'Auto' page

  _ <- autoButton guiElems `on` buttonActivated $ withUpdate $ do
    matchLevels' <- atomically $ do
      model <- readTVar modelTVar
      guiState <- readTVar guiStateTVar
      matches <- getMatches model guiState

      modifyTVar' guiStateTVar (set currentPage AutoPage)

      pure $ matchLevels matches

    -- Increase the step size of the match level spin button with the number of
    -- match levels
    let lvls = fromIntegral matchLevels'
        sqrtLvls = fromIntegral (floor $ sqrt lvls :: Integer)
    adjustment <- adjustmentNew 0 0 lvls 1 sqrtLvls 0
    spinButtonSetAdjustment (matchLevelSpinButton guiElems) adjustment

  -- Cropping of traces

  _ <- applyCropButton guiElems `on` buttonActivated
    $ withUpdate $ atomically $ do
      guiState <- readTVar guiStateTVar
      case guiState ^. currentPage of
        (CropPage (Just indexInterval)) -> do
          model <- readTVar modelTVar
          let newModel = crop indexInterval model
          writeTVar modelTVar newModel
          modifyTVar' guiStateTVar resetGUIPreservingOptions
        _ -> return ()

  -- Applying transforms to the data

  let apply = withUpdate $ atomically $ do
        model <- readTVar modelTVar
        guiState <- readTVar guiStateTVar
        newModel <- getNewModel model guiState
        writeTVar modelTVar newModel
        modifyTVar' guiStateTVar resetGUIPreservingOptions

  _ <- autoApplyButton guiElems     `on` buttonActivated $ apply
  _ <- singleApplyButton guiElems   `on` buttonActivated $ apply
  _ <- multipleApplyButton guiElems `on` buttonActivated $ apply

--------------------------------------------------------------------------------
-- Mouse actions
--------------------------------------------------------------------------------
-- See Controller.Mouse for the definition of mouse gestures

  -- Zooming

  _ <- imageEventBox guiElems `on` scrollEvent $ do
    pickFn <- liftIO $ readTVarIO pickFnTVar
    scrollDirection <- eventScrollDirection
    modifiers <- eventModifier
    layoutPick <- (pickFn . uncurry Point) <$> eventCoordinates

    case layoutPick of
      Just LayoutPick_PlotArea{} -> liftIO $ withUpdate $ do
        model <- atomically (readTVar modelTVar)
        let (rx, ry) = getTraceBounds model ^. toViewPort . viewPortRadii
            -- hardcoded lower bounds:
            -- may need to change them depending on data resolution
            radiiBounds = ((0.02, 2*rx), (0.1, max 200 (2*ry)))
            scaleFactor = case scrollDirection of
              ScrollUp   -> recip $ sqrt 2
              ScrollDown ->         sqrt 2
              _          -> 1

        atomically $ modifyTVar' guiStateTVar $ case modifiers of
          [Control] ->
            over (viewBounds . toViewPort . viewPortRadii . _1)
                 (bound (fst radiiBounds) . (*scaleFactor))
          [Shift] ->
            over (viewBounds . toViewPort . viewPortRadii . _2)
                 (bound (snd radiiBounds) . (*scaleFactor))
          [] ->
              over (viewBounds . toViewPort . viewPortRadii . _1)
                  (bound (fst radiiBounds) . (*scaleFactor))
            . over (viewBounds . toViewPort . viewPortRadii . _2)
                  (bound (snd radiiBounds) . (*scaleFactor))
          _ -> id

      _ -> return ()
    return False

  -- Capturing mouse button presses

  lastMousePressTMVar <- atomically newEmptyTMVar :: IO (TMVar MouseEvent)

  _ <- imageEventBox guiElems `on` buttonPressEvent $ do
    mouseEvent <- captureMouseEvent pickFnTVar
    liftIO $ atomically $ fillTMVar lastMousePressTMVar mouseEvent
    return False

  -- Capturing mouse button releases

  _ <- imageEventBox guiElems `on` buttonReleaseEvent $ do
    maybeMouseEvent1 <- liftIO $ atomically $ tryReadTMVar lastMousePressTMVar
    case maybeMouseEvent1 of
      Nothing -> return False
      Just mouseEvent1 -> do
        mouseEvent2 <- captureMouseEvent pickFnTVar
        (model, guiState, levelShifts) <- liftIO $ atomically $ do
          model' <- readTVar modelTVar
          guiState' <- readTVar guiStateTVar
          levelShifts' <- getLevelShifts model' guiState'
          return (model', guiState', levelShifts')

        -- helper functions and variables
        let nearestPoint' = nearestPoint model
            nearestSlope' = nearestSlope model
            timeAtPoint' = timeAtPoint model
            timeAtSlope' = timeAtSlope model
            mid' (x, y) = 0.5*(x+y)
            s = getCurrentState model ^. series

        case interpretMouseGesture mouseEvent1 mouseEvent2 of
          -- Left-click: panning
          Just (MouseClickLeft pt) -> liftIO $ withUpdate $ atomically $ do
            let cx = bound (getTraceBounds model ^. viewBoundsX) (p_x pt)
                cy = bound (getTraceBounds model ^. viewBoundsY) (p_y pt)
            modifyTVar' guiStateTVar
              $ set (viewBounds . toViewPort . viewPortCenter) (cx, cy)

          -- Right-click: selecting a single level-shift
          Just (MouseClickRight pt) -> liftIO $ withUpdate $
            case guiState ^. currentPage of
              MainPage ->
                case iisFindNearestIndex (nearestSlope' (p_x pt)) levelShifts of
                  Nothing -> return ()
                  Just j ->
                    let t = timeAtSlope' j
                        h = mid' $ over both (ivIndex s)
                          $ runIndexInterval $ levelShiftEndpoints j
                    in  atomically $ modifyTVar' guiStateTVar $
                            set currentPage (SinglePage j)
                          . set (viewBounds.toViewPort.viewPortCenter) (t, h)
              _ -> return ()

          -- Right-click-and-drag: selecting a time interval for something
          Just (MouseDragRightX (xLeft, xRight)) -> liftIO $ withUpdate $
            case guiState ^. currentPage of
              -- Selecting the level-shifts within a time interval
              MainPage ->  do
                let lowerTarget = nearestSlope' xLeft
                    upperTarget = nearestSlope' xRight
                case iisFindIntermediateIndices1
                       (IndexInterval (lowerTarget, upperTarget)) levelShifts of
                    Just (js@(_:_:_)) ->
                      let t = mid' $ over both timeAtSlope' $ (head &&& last) js
                      in  atomically $ modifyTVar' guiStateTVar $
                              set currentPage (MultiplePage js)
                            . set (viewBounds.toViewPort.viewPortCenter._1) t
                    _ -> return ()

              -- Selecting crop boundaries
              CropPage _ ->
                let interval@(IndexInterval (l, u)) =
                        iiBoundByIVector s $ IndexInterval
                      $ over both nearestPoint' $ (xLeft, xRight)
                    t = mid' $ over both timeAtPoint' $ (l, u)
                in  if iiLength interval < 3 then return () else
                      atomically $ modifyTVar' guiStateTVar $
                          set currentPage (CropPage $ Just interval)
                        . set (viewBounds . toViewPort . viewPortCenter . _1) t
              _ -> return ()

          -- Right-click-and-drag with a key modifier: interpolation brush
          Just (MouseDragRightXY keyMod (Point x1 y1) (Point x2 y2)) ->
            liftIO $ withUpdate $ case guiState ^. currentPage of
              MainPage ->
                let stratum = case keyMod of
                      ModControl -> ReplaceLowerData
                      ModShift   -> ReplaceUpperData
                      _          -> ReplaceLowerData
                    interval = IndexInterval
                                 (nearestPoint' x1, nearestPoint' x2)
                    operator = interpolationBrushOp stratum interval (y1, y2)
                in  withUpdate $ atomically $
                      writeTVar modelTVar (applyToModel operator model)
              _ -> return ()

          _ -> return ()
        return False

--------------------------------------------------------------------------------
-- Start the GUI
--------------------------------------------------------------------------------

  widgetShowAll (controllerWindow guiElems)
  widgetHide (image guiElems)

  -- Since we initialize the Model undefined, we use `insensitizeAll` to prevent
  -- the use of GUI functionality requiring an actual model before we have one.
  insensitizeAll guiElems

  mainGUI
