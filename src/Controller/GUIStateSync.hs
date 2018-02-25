module Controller.GUIStateSync
  ( updateGUIStateWidgets
  ) where

import           Control.Lens                           hiding (index)
import           Control.Monad
import           Graphics.UI.Gtk                        hiding (set)
import qualified Graphics.UI.Gtk                        as Gtk (set)

import           Controller.GUIStateWidgets
import           Controller.GUIElements
import           Controller.GUIState


updateGUIStateWidgets :: GUIElements -> GUIState -> IO ()
updateGUIStateWidgets guiElems guiState = do
  Gtk.set (notebook guiElems)
    [ notebookPage := pageNumber (guiState ^. currentPage) ]
  forM_ syncWidgets $ updateGUIStateWidget guiElems guiState

updateGUIStateWidget
  :: GUIElements
  -> GUIState
  -> SyncWidget
  -> IO ()
updateGUIStateWidget guiElems guiState syncCallback =
  case syncCallback of
    SyncSpinButtonDouble spinButtonRef stateRef ->
      Gtk.set (spinButtonRef guiElems)
              [ spinButtonValue := (guiState ^. stateRef) ]

    SyncSpinButtonInt spinButtonRef stateRef ->
      Gtk.set (spinButtonRef guiElems)
              [ spinButtonValue := fromIntegral (guiState ^. stateRef) ]

    SyncCheckButton checkButtonRef stateRef ->
      Gtk.set (checkButtonRef guiElems)
              [ toggleButtonActive := (guiState ^. stateRef) ]

    SyncComboBoxText comboBoxRef stateRef _ ->
      comboBoxSetActive (comboBoxRef guiElems) (guiState ^. stateRef . _1)

    SyncRadioButtonGroup stateRef grouping ->
      let val          = guiState ^. stateRef
          radioButton  = grouping val guiElems
      in  Gtk.set radioButton [ toggleButtonActive := True ]
