module Controller.DialogCallbacks
  ( registerDialogCallbacks
  ) where

import           Control.Concurrent.STM
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Except
import           Data.Functor                 (void)
import           Graphics.UI.Gtk              hiding (set)
import           System.FilePath              (splitFileName)

import           Controller.AppState
import           Controller.GUIElements
import           Controller.GUIState
import           Controller.Util

import           Model

--------------------------------------------------------------------------------

registerDialogCallbacks :: GUIElements -> AppHandle -> IO ()
registerDialogCallbacks guiElems appH = do
  registerSsaSaveCallback    guiElems appH
  registerSsaOpenCallback    guiElems appH
  registerScreenshotCallback guiElems appH

--------------------------------------------------------------------------------

registerSsaSaveCallback :: GUIElements -> AppHandle -> IO ()
registerSsaSaveCallback guiElems appH = do
  fileChooserSaveDialog <-
    makeSaveDialog "Save an .ssa file" (controllerWindow guiElems)

  void $ saveButton guiElems `on` buttonActivated $ do
    (originalDirectory, originalFileName) <-
      splitFileName . getFilePath <$> atomically (getAppModel appH)
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
                (liftIO $ atomically $ getAppModel appH)
            >>= saveSSAFile filePath
            >>= (liftIO . messageDialog (controllerWindow guiElems))
      _ -> return ()

registerSsaOpenCallback :: GUIElements -> AppHandle -> IO ()
registerSsaOpenCallback guiElems appH = do
  fileChooserOpenDialog <- makeOpenSsaDialog (controllerWindow guiElems)

  void $ openButton guiElems `on` buttonActivated $ do
    responseID <- dialogRun fileChooserOpenDialog
    widgetHide fileChooserOpenDialog

    case responseID of
      ResponseAccept -> guiRunExceptT (controllerWindow guiElems) $ do
        mFilePath <- liftIO $ fileChooserGetFilename fileChooserOpenDialog
        case mFilePath of
          Nothing -> ExceptT $ return $ Left "Could not get any files."
          Just filePath -> do
            newModel <- loadSSAFile filePath
            liftIO $ do
              modifyAppModelGUIState appH $ \(_, guiState) ->
                (newModel, setDefaultViewBounds newModel guiState)

              setComboBoxTextLabels ("None" : getLabels newModel)
                                    (referenceTraceComboBoxText guiElems)

              widgetShowAll (controllerWindow guiElems)

      _ -> return ()

registerScreenshotCallback :: GUIElements -> AppHandle -> IO ()
registerScreenshotCallback guiElems appH = do
  fileChooserScreenshotDialog <-
    makeSaveDialog "Save a screenshot" (controllerWindow guiElems)

  _ <- screenshotSaveButton guiElems `on` buttonActivated $
    appRequestScreenshot appH $ \defaultFilePath -> do
      let (defaultDirectory, defaultFileName) = splitFileName defaultFilePath
      _ <- fileChooserSetCurrentFolder
            fileChooserScreenshotDialog defaultDirectory
      fileChooserSetCurrentName fileChooserScreenshotDialog $ defaultFileName

      responseID <- dialogRun fileChooserScreenshotDialog
      widgetHide fileChooserScreenshotDialog

      case responseID of
        ResponseAccept -> fileChooserGetFilename fileChooserScreenshotDialog
        _              -> pure $ Nothing

  return ()

--------------------------------------------------------------------------------

makeOpenSsaDialog :: Window -> IO FileChooserDialog
makeOpenSsaDialog window = do
  fc <- fileChooserDialogNew
          (Just "Open an .ssa file") (Just window) FileChooserActionOpen
          [ ("Cancel" :: String, ResponseCancel)
          , ("Open"   :: String, ResponseAccept) ]

  fileChooserSetSelectMultiple fc False

  fileFilter <- fileFilterNew
  fileFilterAddPattern fileFilter ("*.ssa" :: String)
  fileChooserSetFilter fc fileFilter

  pure fc

makeSaveDialog :: String -> Window -> IO FileChooserDialog
makeSaveDialog title window = do
  fc <- fileChooserDialogNew (Just title) (Just window) FileChooserActionSave
          [ ("Cancel" :: String, ResponseCancel)
          , ("Save"   :: String, ResponseAccept) ]

  fileChooserSetSelectMultiple          fc False
  fileChooserSetDoOverwriteConfirmation fc True

  pure fc

--------------------------------------------------------------------------------

guiRunExceptT :: Window -> ExceptT String IO () -> IO ()
guiRunExceptT window m = do
  e <- runExceptT m
  case e of
    Left s   -> messageDialog window s
    Right () -> return ()
