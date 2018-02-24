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
import           Controller.Interpreter

data AppState = AppState
  { appModel    :: Model
  , appGUIState :: GUIState
  , appResults  :: Results
  }

data AppStateHandle = AppStateHandle
  { getAppState       :: STM AppState
  , getPickFn         :: STM (PickFn (LayoutPick Double Double Double))
  , updateAppState    :: (AppState -> (Model, GUIState)) -> STM ()
  , requestDraw       :: STM ()
  , requestScreenshot :: (FilePath -> IO (Maybe FilePath)) -> IO ()
  }

initializeAppState :: GUIElements -> IO AppStateHandle
initializeAppState guiElems = do
  viewH <- View.setupRenderer (controllerWindow guiElems) (image guiElems)
                              (controllerWindowBox guiElems)
  interpreterH <- setupInterpreter

  initialResults <-
    atomically $ getResults interpreterH defaultModel def
  appStateTVar <- newTVarIO $ AppState defaultModel def initialResults

  let updateAppState' :: (AppState -> (Model, GUIState)) -> STM ()
      updateAppState' f = do
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


modifyAppModel :: AppStateHandle -> (Model -> Model) -> STM ()
modifyAppModel appStateH f =
  updateAppState appStateH $ over _1 f . (appModel &&& appGUIState)

modifyAppGUIState :: AppStateHandle -> (GUIState -> GUIState) -> STM ()
modifyAppGUIState appStateH f =
  updateAppState appStateH $ over _2 f . (appModel &&& appGUIState)

modifyAppModelGUIState
  :: AppStateHandle
  -> ((Model, GUIState) -> (Model, GUIState))
  -> STM ()
modifyAppModelGUIState appStateH f =
  updateAppState appStateH $ f . (appModel &&& appGUIState)

