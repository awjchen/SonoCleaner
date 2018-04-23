{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Controller.AppState
  ( AppHandle (..)
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
import           View

import           Controller.GUIElements
import           Controller.GUIState
import           Controller.GUIStateSync
import           Controller.Interpreter
import           Controller.Sensitivity

--------------------------------------------------------------------------------

data AppState a = AppState
  { appModel    :: Model a
  , appGUIState :: GUIState
  , appResults  :: Results a
  } deriving (Functor)

-- Not a functor. The type parameter of an `AppHandle` cannot be changed.
data AppHandle a = AppHandle
  { getAppState          :: STM (AppState a)
  , getAppPickFn         :: STM (PickFn (LayoutPick Double Double Double))
  , modifyAppState       :: (AppState a -> (Model a, GUIState)) -> IO ()
  , appRequestScreenshot :: (FilePath -> IO (Maybe FilePath)) -> IO ()
  , defaultAnnotation    :: a
  }

--------------------------------------------------------------------------------

initializeAppState :: forall a. GUIElements -> a -> IO (AppHandle a)
initializeAppState guiElems defAnnotation = do
  viewH <- setupRenderer (controllerWindow guiElems) (image guiElems)
                              (controllerWindowBox guiElems)
  interpreterH <- setupInterpreter

  let initialModel = defAnnotation <$ defaultModel
  initialResults <- atomically $ getResults interpreterH initialModel def
  appStateTVar <- newTVarIO $ AppState initialModel def initialResults

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

      requestDraw' :: STM ()
      requestDraw' = do
        appState <- readTVar appStateTVar
        let chart = resultChart $ appResults appState
        requestDraw viewH chart

      modifyAppState' :: (AppState a -> (Model a, GUIState)) -> IO ()
      modifyAppState' f = withUpdate $ atomically $ do
        (newModel, newGUIState) <- f <$> readTVar appStateTVar
        newResults <- getResults interpreterH newModel newGUIState
        writeTVar appStateTVar $ AppState newModel newGUIState newResults

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
            requestScreenshot viewH
              filePath extension (resultChart $ appResults appState)

  pure $ AppHandle
    { getAppState = readTVar appStateTVar
    , getAppPickFn = getPickFn viewH
    , modifyAppState = modifyAppState'
    , appRequestScreenshot = requestScreenshot'
    , defaultAnnotation = defAnnotation
    }

--------------------------------------------------------------------------------

getFromApp :: (AppState a -> r) -> AppHandle a -> STM r
getFromApp f appStateH = f <$> getAppState appStateH

getAppModel :: AppHandle a -> STM (Model a)
getAppModel = getFromApp appModel

getAppGUIState :: AppHandle a -> STM GUIState
getAppGUIState = getFromApp appGUIState

getAppModelGUIState :: AppHandle a -> STM (Model a, GUIState)
getAppModelGUIState = getFromApp ((,) <$> appModel <*> appGUIState)

getAppResults :: AppHandle a -> STM (Results a)
getAppResults = getFromApp appResults

--------------------------------------------------------------------------------

modifyAppModel :: AppHandle a -> (Model a -> Model a) -> IO ()
modifyAppModel appStateH f =
  modifyAppState appStateH $ over _1 f . (appModel &&& appGUIState)

modifyAppGUIState :: AppHandle a -> (GUIState -> GUIState) -> IO ()
modifyAppGUIState appStateH f =
  modifyAppState appStateH $ over _2 f . (appModel &&& appGUIState)

modifyAppModelGUIState
  :: AppHandle a
  -> ((Model a, GUIState) -> (Model a, GUIState))
  -> IO ()
modifyAppModelGUIState appStateH f =
  modifyAppState appStateH $ f . (appModel &&& appGUIState)

