-- The 'Controller' of "Model, View, Controller"
--
-- In this program, the Controller mediates all interaction with the user. It
-- collects the user's inputs and transforms them into requests to the Model and
-- View. A part of this is achieved by maintaining synchronization between the
-- state of GTK+ 3 widgets and an instance of a `GUIState` type.
--
-- This module deinfes `controllerMain`, which is basically the entry point of
-- the program.

module Controller
  ( controllerMain
  ) where

import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad.IO.Class       (liftIO)
import           Data.Functor                 (void)
import           Graphics.UI.Gtk              hiding (set)

import           Model

import           Controller.AppState
import           Controller.DialogCallbacks
import           Controller.GenericCallbacks
import           Controller.Glade
import           Controller.GUIElements
import           Controller.GUIState
import           Controller.GUIStateCallbacks
import           Controller.Interpreter
import           Controller.Keybindings
import           Controller.MouseCallbacks
import           Controller.Sensitivity

controllerMain :: IO ()
controllerMain = do

  void initGUI

-- Import GUI from Glade

  builder <- builderNew
  builderAddFromString builder builderString

  guiElems <- importGUIElements builder

  windowSetDefaultSize (controllerWindow guiElems) 1024 640

-- Mutable state

  appH <- initializeAppState guiElems defaultTraceAnnotation

-- Callbacks

  registerKeyboardShortcuts guiElems appH
  registerDialogCallbacks guiElems appH
  registerMouseCallbacks guiElems appH

  -- for widgets holding internal state that needs synchronization with the
  -- GUIState
  registerGUIStateCallbacks guiElems appH

  -- for buttons operating purely on (Model, GUIState)
  registerPureCallbacks guiElems appH

  -- On attempting to close either the display or controller window, quit the
  -- program, but only after asking for confirmation.
  let quitWithConfirmation :: EventM EAny Bool
      quitWithConfirmation = liftIO $ do
        isResponseConfirm <-
          confirmDialog (controllerWindow guiElems) "Exit the program?"
        if isResponseConfirm
          then mainQuit >> return False
          else return True

  void $ controllerWindow guiElems `on` deleteEvent $ quitWithConfirmation

  -- When switching to the Auto page, adjust the step size of the match level
  -- spin button based on the number of match levels
  void $ autoButton guiElems `on` buttonActivated $ do
    matchLevels' <- atomically
      $ matchLevels . resultMatches <$> getAppResults appH

    modifyAppGUIState appH $ set currentPage AutoPage
    let lvls = fromIntegral matchLevels'
        sqrtLvls = fromIntegral (floor $ sqrt lvls :: Integer)
    adjustment <- adjustmentNew 0 0 lvls 1 sqrtLvls 0
    spinButtonSetAdjustment (matchLevelSpinButton guiElems) adjustment

  -- Applying transforms to the data
  let apply = do
        newModel <- atomically $
          resultNewModel . appResults <$> getAppState appH
        modifyAppModelGUIState appH $
          set _1 newModel . over _2 resetGUIPreservingOptions

  void $ autoApplyButton guiElems     `on` buttonActivated $ apply
  void $ singleApplyButton guiElems   `on` buttonActivated $ apply
  void $ multipleApplyButton guiElems `on` buttonActivated $ apply

-- Start

  widgetShowAll (controllerWindow guiElems)
  widgetHide (image guiElems)

  -- Since we initialize the Model undefined, we use `insensitizeAll` to prevent
  -- the use of GUI functionality requiring an actual model before we have one.
  insensitizeAll guiElems

  mainGUI

-------------------------------------------------------------------------------

confirmDialog :: Window -> String -> IO Bool
confirmDialog window msg = do
  msgDialog <- messageDialogNew (Just window) [] MessageInfo ButtonsOkCancel msg
  responseID <- dialogRun msgDialog
  widgetDestroy msgDialog
  return $ case responseID of
    ResponseOk -> True
    _          -> False
