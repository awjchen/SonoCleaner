module Controller.GUIStateCallbacks
  ( registerGUIStateCallbacks
  ) where

import           Control.Lens                           hiding (index)
import           Control.Monad
import           Graphics.UI.Gtk                        hiding (set)

import           Controller.AppState
import           Controller.GUIElements
import           Controller.GUIStateWidgets

registerGUIStateCallbacks ::
     GUIElements
  -> AppStateHandle
  -> IO ()
registerGUIStateCallbacks guiElems appH =
  forM_ syncWidgets $ registerGUIStateCallback guiElems appH

registerGUIStateCallback
  :: GUIElements
  -> AppStateHandle
  -> SyncWidget
  -> IO ()
registerGUIStateCallback guiElems appH syncCallback =
  case syncCallback of
    SyncSpinButtonDouble spinButtonRef stateRef -> do
      let spinButton = spinButtonRef guiElems
      void $ afterValueSpinned spinButton $ do
        val <- spinButtonGetValue spinButton
        modifyAppGUIState appH $ set stateRef val

    SyncSpinButtonInt spinButtonRef stateRef -> do
      let spinButton = spinButtonRef guiElems
      void $ afterValueSpinned spinButton $ do
        val <- spinButtonGetValueAsInt spinButton
        modifyAppGUIState appH $ set stateRef val

    SyncCheckButton checkButtonRef stateRef -> do
      let checkButton = checkButtonRef guiElems
      void $ on checkButton buttonActivated $ do
        active <- toggleButtonGetActive checkButton
        modifyAppGUIState appH $ set stateRef active

    SyncComboBoxText comboBoxRef stateRef reader -> do
      let comboBox = comboBoxRef guiElems
      void $ on comboBox changed $ do
        index <- comboBoxGetActive comboBox
        listStore <- comboBoxGetModelText comboBox
        txt <- listStoreSafeGetValue listStore index
        modifyAppGUIState appH $ set stateRef (index, reader txt)

    SyncRadioButtonGroup stateRef grouping -> do
      forM_ listAll $ \val -> do
        let radioButton = grouping val guiElems
        void $ on radioButton buttonActivated $ do
          active <- toggleButtonGetActive radioButton
          when active $ modifyAppGUIState appH $ set stateRef val

listAll :: (Bounded a, Enum a) => [a]
listAll = [minBound..maxBound]
