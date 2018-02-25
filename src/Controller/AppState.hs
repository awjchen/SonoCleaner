module Controller.AppState
  ( AppStateHandle (..)
  , AppState (..)

  , initializeAppState

  , getAppModel
  , getAppGUIState
  , getAppModelGUIState
  , getAppResults

  , modifyAppModel
  , modifyAppGUIState
  , modifyAppModelGUIState
  ) where

import           Control.Arrow                          ((&&&))
import           Control.Concurrent.STM
import           Control.Lens
import           Data.Default
import           Graphics.Rendering.Chart               (LayoutPick, PickFn)
import qualified Graphics.Rendering.Chart.Backend.Cairo as Chart
import           System.FilePath.Posix                  (dropExtension,
                                                         splitFileName, (</>))

import           Model
import qualified View                                   as View

import           Controller.GUIElements
import           Controller.GUIState
import           Controller.GUIStateSync
import           Controller.Interpreter
import           Controller.Sensitivity

--------------------------------------------------------------------------------

data AppState = AppState
  { appModel    :: Model
  , appGUIState :: GUIState
  , appResults  :: Results
  }

data AppStateHandle = AppStateHandle
  { getAppState       :: STM AppState
  , getPickFn         :: STM (PickFn (LayoutPick Double Double Double))
  , updateAppState    :: (AppState -> (Model, GUIState)) -> IO ()
  , requestDraw       :: STM ()
  , requestScreenshot :: (FilePath -> IO (Maybe FilePath)) -> IO ()
  }

--------------------------------------------------------------------------------

initializeAppState :: GUIElements -> IO AppStateHandle
initializeAppState guiElems = do
  viewH <- View.setupRenderer (controllerWindow guiElems) (image guiElems)
                              (controllerWindowBox guiElems)
  interpreterH <- setupInterpreter

  initialResults <- atomically $ getResults interpreterH defaultModel def
  appStateTVar <- newTVarIO $ AppState defaultModel def initialResults

  -- Disclaimer: I am not very familliar with Gtk+ 3, and I don't know what I'm
  -- doing.

  -- For some reason, the triggering of one widget is often accompanied by the
  -- triggering of other widgets, sometimes by design in Gtk+ 3 and sometimes
  -- because of our synchronization of the Gtk+3 widget states with our
  -- `GUIState` (or so I suspect). In any case, it would be expensive and
  -- redundant if each of the widgets were to demand a canvas redraw. We prevent
  -- this by wrapping every callback with a function `withUpdate`, which obtains
  -- a lock that prevents both GUI state synchronization and canvas updates
  -- until all of the callbacks have finished executing.

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
            (model, guiState) <- fmap (appModel &&& appGUIState)
              $ atomically $ readTVar appStateTVar

            updateGUIStateWidgets guiElems guiState
            setGUISensitivity guiElems model guiState

            atomically requestDraw'

      updateAppState' :: (AppState -> (Model, GUIState)) -> IO ()
      updateAppState' f = withUpdate $ atomically $ do
        (newModel, newGUIState) <- f <$> readTVar appStateTVar
        newResults <- getResults interpreterH newModel newGUIState
        writeTVar appStateTVar $ AppState newModel newGUIState newResults

      requestDraw' :: STM ()
      requestDraw' = do
        appState <- readTVar appStateTVar
        let chart = resultChart $ appResults appState
        View.requestDraw viewH chart

      -- `filePathCont` provides a default `FilePath`
      requestScreenshot' :: (FilePath -> IO (Maybe FilePath)) -> IO ()
      requestScreenshot' filePathCont = do
        appState <- atomically $ readTVar appStateTVar
        let model = appModel appState
            guiState = appGUIState appState

        let (originalDirectory, originalFileName) =
              splitFileName $ getFilePath model
            extension = guiState ^. screenshotFileFormat . _2
            extensionString = case extension of
              Chart.PNG -> "png"
              Chart.SVG -> "svg"
              Chart.PS  -> "ps"
              Chart.PDF -> "pdf"
            defaultFileName = concat
              [ "screenshot_"
              , dropExtension originalFileName
              , "_"
              , getLabel model
              , "."
              , extensionString
              ]
            defaultFilePath = originalDirectory </> defaultFileName

        mFilePath <- filePathCont defaultFilePath

        case mFilePath of
          Nothing -> return ()
          Just filePath ->
            View.requestScreenshot viewH
              filePath extension (resultChart $ appResults appState)

  pure $ AppStateHandle
    { getAppState = readTVar appStateTVar
    , getPickFn = View.getPickFn viewH
    , updateAppState = updateAppState'
    , requestDraw = requestDraw'
    , requestScreenshot = requestScreenshot'
    }

--------------------------------------------------------------------------------

getFromApp :: (AppState -> a) -> AppStateHandle -> STM a
getFromApp f appStateH = f <$> getAppState appStateH

getAppModel :: AppStateHandle -> STM Model
getAppModel = getFromApp appModel

getAppGUIState :: AppStateHandle -> STM GUIState
getAppGUIState = getFromApp appGUIState

getAppModelGUIState :: AppStateHandle -> STM (Model, GUIState)
getAppModelGUIState = getFromApp ((,) <$> appModel <*> appGUIState)

getAppResults :: AppStateHandle -> STM Results
getAppResults = getFromApp appResults


modifyAppModel :: AppStateHandle -> (Model -> Model) -> IO ()
modifyAppModel appStateH f =
  updateAppState appStateH $ over _1 f . (appModel &&& appGUIState)

modifyAppGUIState :: AppStateHandle -> (GUIState -> GUIState) -> IO ()
modifyAppGUIState appStateH f =
  updateAppState appStateH $ over _2 f . (appModel &&& appGUIState)

modifyAppModelGUIState
  :: AppStateHandle
  -> ((Model, GUIState) -> (Model, GUIState))
  -> IO ()
modifyAppModelGUIState appStateH f =
  updateAppState appStateH $ f . (appModel &&& appGUIState)

