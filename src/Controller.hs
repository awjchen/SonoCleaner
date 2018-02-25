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

import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad.IO.Class       (liftIO)
import           Graphics.UI.Gtk              hiding (set)

import           Controller.AppState          as App
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
import           Controller.Util

import           Model

controllerMain :: IO ()
controllerMain = do

--------------------------------------------------------------------------------
-- Import GUI (from Glade)
--------------------------------------------------------------------------------

  _ <- initGUI

  builder <- builderNew
  builderAddFromString builder builderString

  guiElems <- importGUIElements builder

  windowSetDefaultSize (controllerWindow guiElems) 1024 640

--------------------------------------------------------------------------------
-- Mutable state
--------------------------------------------------------------------------------

  appH <- initializeAppState guiElems

--------------------------------------------------------------------------------
-- Keyboard shortcuts
--------------------------------------------------------------------------------

  registerKeyboardShortcuts guiElems appH

--------------------------------------------------------------------------------
-- Generic callbacks
--------------------------------------------------------------------------------
-- Callbacks requiring only basic functionality are defined within a more
-- structured and restrictive environemnt. In particular, all callbacks that
-- update parameters in `GUIState` are here in `registerGUIStateWidgets`.

  registerGUIStateWidgets guiElems appH
  registerCallbacks guiElems appH

--------------------------------------------------------------------------------
-- Other callbacks
--------------------------------------------------------------------------------

  registerDialogCallbacks guiElems appH
  registerMouseCallbacks guiElems appH

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

  -- Run the automated procedure when switching to the 'Auto' page

  _ <- autoButton guiElems `on` buttonActivated $ do
    matchLevels' <- atomically
      $ matchLevels . resultMatches <$> getAppResults appH

    modifyAppGUIState appH $ set currentPage AutoPage

    -- Increase the step size of the match level spin button with the number of
    -- match levels
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

  _ <- autoApplyButton guiElems     `on` buttonActivated $ apply
  _ <- singleApplyButton guiElems   `on` buttonActivated $ apply
  _ <- multipleApplyButton guiElems `on` buttonActivated $ apply


--------------------------------------------------------------------------------
-- Start the GUI
--------------------------------------------------------------------------------

  widgetShowAll (controllerWindow guiElems)
  widgetHide (image guiElems)

  -- Since we initialize the Model undefined, we use `insensitizeAll` to prevent
  -- the use of GUI functionality requiring an actual model before we have one.
  insensitizeAll guiElems

  mainGUI
