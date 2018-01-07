-- The 'Controller' of "Model, View, Controller"
--
-- In this program, the Controller defines the GUI and mediates all interaction
-- with the user.
--
-- Additionally, since callbacks to GTK are defined by the controller,
-- the global state of the program is defined and initialized
-- by the controller to enable communication from within callbacks.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Controller
  ( controllerMain
  ) where

import           Control.Arrow               ((&&&))
import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Trans.Except
import           Data.Default
import qualified Data.Text                   as T
import           Graphics.Rendering.Chart
import           Graphics.UI.Gtk             hiding (set)
import           System.FilePath             (splitFileName)

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

-------------------------------------------------------------------------------
-- Run gui
-------------------------------------------------------------------------------

controllerMain :: IO ()
controllerMain = do

  -------------------------------------------------------------------------------
  -- Global state
  -------------------------------------------------------------------------------

  modelTVar <- newTVarIO initUndefinedModel :: IO (TVar Model)
  guiStateTVar <- newTVarIO def :: IO (TVar GUIState)

  -- Without this lock, GUI actions that both request rendering and trigger
  -- other such GUI actions would redraw the same state multiple times.
   -- see 'withUpdate'
  updateLock <- atomically $ newTMVar () :: IO (TMVar ())

  -- Mouse event handling variables
  lastMousePressTMVar <- atomically newEmptyTMVar :: IO (TMVar MouseEvent)

  -------------------------------------------------------------------------------
  -- Import GUI from Glade
  -------------------------------------------------------------------------------

  _ <- initGUI

  builder <- builderNew
  builderAddFromString builder builderString

  guiElems <- importGUIElements builder

  -------------------------------------------------------------------------------
  -- Setup computation and rendering
  -------------------------------------------------------------------------------

  (initializeInterpreter, getChart, getMatches, getLevelShifts, getNewModel)
    <- setupInterpreter

  windowSetDefaultSize (controllerWindow guiElems) 1024 640

  (pickFnTVar, requestDraw) <- setupRenderer (controllerWindow guiElems)
                                             (image guiElems)
                                             (controllerWindowBox guiElems)

  -- I am not very famillair with GTK+3, but as far as I know, GTK+3 GUI actions
  -- requiring updates to either the view or the GUI may trigger other such
  -- actions, which can cause multiple redundant canvas redraws. We prevent this
  -- by having the the first GUI action obtain a lock that prevents the actions
  -- it triggers triggered actions from requesting canvas updates.
  let withUpdate' :: Bool -> IO () -> IO ()
      withUpdate' doUpdateGUIParameters action = do
        maybeLock <- atomically $ tryTakeTMVar updateLock
        action
        case maybeLock of
          Nothing -> return ()
          Just () -> do
            (model, guiState) <- atomically $
              (,) <$> readTVar modelTVar <*> readTVar guiStateTVar

            -- We update the GUI before performing a canvas redraw
            when doUpdateGUIParameters $ setGUIParameters guiElems guiState
            setGUISensitivity guiElems model guiState

            atomically $ getChart model guiState >>= requestDraw
            atomically $ putTMVar updateLock ()

      -- Setting the internal state of a GTK+3 GUI element can trigger its
      -- callback, which again writes to its internal state, forming an infinite
      -- loop. I have yet to figure out how to prevent this, so we require the
      -- following workaround, where callbacks originating from GUI elements
      -- holding important internal state are not allowed to make changes to
      -- other GUI elements.

      -- For use by GUI elements that _do not_ hold any state needing
      -- synchronization with the GUIState (e.g. buttons):
      withUpdate = withUpdate' True
      -- For use by GUI elements those that _do_ hold state needing
      -- syncrhonization with the GUI state (e.g. check buttons, radio buttons,
      -- spin buttons):
      withPartialUpdate = withUpdate' False

  -------------------------------------------------------------------------------
  -- Keyboard shortcuts
  -------------------------------------------------------------------------------

  registerKeyboardShortcuts guiElems guiStateTVar

  -------------------------------------------------------------------------------
  -- Generic callbacks
  -------------------------------------------------------------------------------
  -- Callbacks using only basic functionality are defined within a more
  -- restrictive environemnt.

  registerCallbacks
    guiElems modelTVar guiStateTVar withUpdate withPartialUpdate

  -------------------------------------------------------------------------------
  -- Special callbacks
  -------------------------------------------------------------------------------

  -- Quit on closing either the display or controller window, but only after
  -- confirming.
  let quitWithConfirmation :: EventM EAny Bool
      quitWithConfirmation = liftIO $ do
        isResponseConfirm <-
          confirmDialog (controllerWindow guiElems) "Exit the program?"
        if isResponseConfirm
          then mainQuit >> return False
          else return True

  _ <- controllerWindow guiElems `on` deleteEvent $ quitWithConfirmation

  -- Writing files
  fileChooserSaveDialog <-
    fileChooserDialogNew (Just "Save an .ssa file")
                         (Just (controllerWindow guiElems))
                         FileChooserActionSave
                         [   ("Cancel" :: String, ResponseCancel)
                           , ("Save"   :: String, ResponseAccept) ]

  fileChooserSetSelectMultiple          fileChooserSaveDialog False
  fileChooserSetDoOverwriteConfirmation fileChooserSaveDialog True

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
        -- Hey, where is the exception handling?
        mFilePath <- fileChooserGetFilename fileChooserSaveDialog
        case mFilePath of
          Nothing -> return ()
          Just filePath -> guiRunExceptT (controllerWindow guiElems) $ do
            (liftIO $ atomically $ readTVar modelTVar)
            >>= saveSSAFile filePath
            >>= (liftIO . messageDialog (controllerWindow guiElems))
      _ -> return ()

  -- Reading files
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

    return fc

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
              widgetShowAll (controllerWindow guiElems)

              let labels = getLabels model
                  cb = referenceTraceComboBoxText guiElems
              _ <- comboBoxSetModelText cb
              _ <- comboBoxAppendText cb (T.pack "None")
              forM_ labels $ comboBoxAppendText cb . T.pack
              comboBoxSetActive cb 0

      _ -> return ()

  -- Auto page
  _ <- autoButton guiElems `on` buttonActivated $ withUpdate $ do
    matchLevels' <- atomically $ do
      model <- readTVar modelTVar
      guiState <- readTVar guiStateTVar
      matches <- getMatches model guiState

      modifyTVar' guiStateTVar ( set currentPage AutoPage
                               . set levelShiftMatches matches )

      return $ matchLevels matches

    -- Adjust parameters of the match level spin button
    let lvls = fromIntegral matchLevels'
        sqrtLvls = fromIntegral (floor $ sqrt lvls :: Integer)
    adjustment <- adjustmentNew 0 0 lvls 1 sqrtLvls 0
    spinButtonSetAdjustment (matchLevelSpinButton guiElems) adjustment

  -- Applying cropping
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

  -- Applying transforms
  let apply = withUpdate $ atomically $ do
        model <- readTVar modelTVar
        guiState <- readTVar guiStateTVar
        newModel <- getNewModel model guiState
        writeTVar modelTVar newModel
        modifyTVar' guiStateTVar resetGUIPreservingOptions

  _ <- autoApplyButton guiElems     `on` buttonActivated $ apply
  _ <- singleApplyButton guiElems   `on` buttonActivated $ apply
  _ <- multipleApplyButton guiElems `on` buttonActivated $ apply

  -------------------------------------------------------------------------------
  -- Mouse callbacks
  -------------------------------------------------------------------------------
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
            radiiBounds = ((0.02, 2*rx), (0.1, max 200 (2*ry)))
            scaleFactor = case scrollDirection of
              ScrollUp   -> recip $ sqrt 2
              ScrollDown ->         sqrt 2
              _          -> 1
        atomically $ modifyTVar' guiStateTVar $ case modifiers of
          [Control] ->
            over (viewBounds . toViewPort . viewPortRadii . _2)
                 (bound (snd radiiBounds) . (*scaleFactor))
          _ ->
            over (viewBounds . toViewPort . viewPortRadii . _1)
                 (bound (fst radiiBounds) . (*scaleFactor))
      _ -> return ()
    return False

  -- Capturing mouse button presses
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

          -- Right-click-and-drag: ...
          Just (MouseDragRightX (xLeft, xRight)) -> liftIO $ withUpdate $
            case guiState ^. currentPage of
              -- Selecting multiple level-shifts in a time interval
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
  -- Go
  --------------------------------------------------------------------------------

  widgetShowAll (controllerWindow guiElems)
  widgetHide (image guiElems)

  -- We use `insensitizeAll` to limit GUI functionality before any files have
  -- been opened, since otherwise the program as is will try to operate on an
  -- undefined value for the Model.
  insensitizeAll guiElems

  mainGUI
