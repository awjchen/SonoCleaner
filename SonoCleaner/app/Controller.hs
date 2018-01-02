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

import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Trans.Except
import           Data.Default
import qualified Data.Text                   as T
import qualified Data.Vector.Unboxed         as V
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
import qualified Types.IndexInterval         as I
import           Types.IntMap
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

  (initializeInterpreter, getChart, getMatches, getJumps, getNewModel)
    <- setupInterpreter

  windowSetDefaultSize (guiElems ^. controllerWindow) 1024 640

  (pickFnTVar, requestDraw) <- setupRenderer (guiElems ^. controllerWindow)
                                             (guiElems ^. image)
                                             (guiElems ^. controllerWindowBox)

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
          confirmDialog (guiElems ^. controllerWindow) "Exit the program?"
        if isResponseConfirm
          then mainQuit >> return False
          else return True

  _ <- view controllerWindow guiElems `on` deleteEvent $ quitWithConfirmation

  -- Writing files
  fileChooserSaveDialog <-
    fileChooserDialogNew (Just "Save an .ssa file")
                         (Just (guiElems ^. controllerWindow))
                         FileChooserActionSave
                         [   ("Cancel" :: String, ResponseCancel)
                           , ("Save"   :: String, ResponseAccept) ]

  fileChooserSetSelectMultiple          fileChooserSaveDialog False
  fileChooserSetDoOverwriteConfirmation fileChooserSaveDialog True

  _ <- (guiElems ^. saveButton) `on` buttonActivated $ do
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
          Just filePath -> atomically (readTVar modelTVar)
                       >>= saveSSAFile filePath
                       >>= messageDialog (guiElems ^. controllerWindow)
      _ -> return ()

  -- Reading files
  fileChooserOpenDialog <- do
    fc <- fileChooserDialogNew (Just "Open an .ssa file")
                               (Just (guiElems ^. controllerWindow))
                               FileChooserActionOpen
                               [   ("Cancel" :: String, ResponseCancel)
                                 , ("Open"   :: String, ResponseAccept) ]
    fileChooserSetSelectMultiple fc False

    fileFilter <- fileFilterNew
    fileFilterAddPattern fileFilter ("*.ssa" :: String)
    fileChooserSetFilter fc fileFilter

    return fc

  _ <- (guiElems ^. openButton) `on` buttonActivated $ do
    responseID <- dialogRun fileChooserOpenDialog
    widgetHide fileChooserOpenDialog
    case responseID of
      ResponseAccept -> guiRunExceptT (guiElems ^. controllerWindow) $ do
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
              widgetShowAll (guiElems ^. controllerWindow)

              let labels = getLabels model
                  cb = guiElems ^. referenceTraceComboBoxText
              _ <- comboBoxSetModelText cb
              _ <- comboBoxAppendText cb (T.pack "None")
              forM_ labels $ comboBoxAppendText cb . T.pack
              comboBoxSetActive cb 0

      _ -> return ()

  -- Auto page
  _ <- (guiElems ^. autoButton) `on` buttonActivated $ withUpdate $ do
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
    spinButtonSetAdjustment (guiElems ^. matchLevelSpinButton) adjustment

  -- Applying cropping
  _ <- (guiElems ^. applyCropButton) `on` buttonActivated
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

  _ <- (guiElems ^. autoApplyButton)     `on` buttonActivated $ apply
  _ <- (guiElems ^. singleApplyButton)   `on` buttonActivated $ apply
  _ <- (guiElems ^. multipleApplyButton) `on` buttonActivated $ apply

  -------------------------------------------------------------------------------
  -- Mouse callbacks
  -------------------------------------------------------------------------------
  -- See Controller.Mouse for the definition of mouse gestures

  -- Zooming
  _ <- (guiElems ^. imageEventBox) `on` scrollEvent $ do
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
  _ <- (guiElems ^. imageEventBox) `on` buttonPressEvent $ do
    mouseEvent <- captureMouseEvent pickFnTVar
    liftIO $ atomically $ fillTMVar lastMousePressTMVar mouseEvent
    return False

  -- Capturing mouse button releases
  _ <- (guiElems ^. imageEventBox) `on` buttonReleaseEvent $ do
    maybeMouseEvent1 <- liftIO $ atomically $ tryReadTMVar lastMousePressTMVar
    case maybeMouseEvent1 of
      Nothing -> return False
      Just mouseEvent1 -> do
        mouseEvent2 <- captureMouseEvent pickFnTVar
        (model, guiState, jumps) <- liftIO $ atomically $ do
          model' <- readTVar modelTVar
          guiState' <- readTVar guiStateTVar
          jumps' <- getJumps model' guiState'
          return (model', guiState', jumps')

        let (toTime, toIndex) = getIndexTimeConversions model
            dt = getTimeStep model
            s = getCurrentState model ^. series
            lastIndex = V.length s - 1

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
                case findNearestIndex (toIndex (p_x pt)) jumps of
                  Nothing -> return ()
                  Just i -> let t = toTime i + 0.5*dt
                                h = 0.5 * (s V.! i + s V.! (i+1))
                    in  atomically $ modifyTVar' guiStateTVar $
                            set currentPage (SinglePage i)
                          . set (viewBounds.toViewPort.viewPortCenter) (t, h)
              _ -> return ()

          -- Right-click-and-drag: ...
          Just (MouseDragRightX (xLeft, xRight)) -> liftIO $ withUpdate $
            case guiState ^. currentPage of
              -- Selecting multiple level-shifts in a time interval
              MainPage ->  do
                let lowerTarget = toIndex xLeft
                    upperTarget = toIndex xRight
                case findIntermediateIndices (lowerTarget, upperTarget) jumps of
                    Just (is@(_:_:_)) ->
                      let t = 0.5 * (toTime (head is) + toTime (last is))
                      in  atomically $ modifyTVar' guiStateTVar $
                              set currentPage (MultiplePage is)
                            . set (viewBounds.toViewPort.viewPortCenter._1) t
                    _ -> return ()
              -- Selecting crop boundaries
              CropPage _ ->
                let lowerIndex = max 0         $ toIndex xLeft
                    upperIndex = min lastIndex $ toIndex xRight
                    t = 0.5 * (toTime lowerIndex + toTime upperIndex)
                in  atomically $ modifyTVar' guiStateTVar $
                        set currentPage (CropPage $ Just $
                              I.fromEndpoints (lowerIndex, upperIndex))
                      . set (viewBounds . toViewPort . viewPortCenter . _1) t
              _ -> return ()

          -- Right-click-and-drag with a key modifier: interpolation brush
          Just (MouseDragRightXY keyMod (Point x1 y1) (Point x2 y2)) ->
            liftIO $ withUpdate $ case guiState ^. currentPage of
              MainPage ->
                let stratum = case keyMod of
                      ModControl -> LabelLower
                      ModShift   -> LabelUpper
                      _          -> LabelLower
                    operator = interpolateGapsOp stratum
                                (toIndex x1, toIndex x2) (y1, y2)
                in  withUpdate $ atomically $
                      writeTVar modelTVar (applyToModel operator model)
              _ -> return ()

          _ -> return ()
        return False

  --------------------------------------------------------------------------------
  -- Go
  --------------------------------------------------------------------------------

  widgetShowAll (guiElems ^. controllerWindow)
  widgetHide (guiElems ^. image)

  -- We use `insensitizeAll` to limit GUI functionality before any files have
  -- been opened, since otherwise the program as is will try to operate on an
  -- undefined value for the Model.
  insensitizeAll guiElems

  mainGUI
