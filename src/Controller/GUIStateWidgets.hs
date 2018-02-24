-- TODO: summary

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}

module Controller.GUIStateWidgets
  ( registerGUIStateWidgets
  , updateGUIStateWidgets
  ) where

import           Control.Concurrent.STM
import           Control.Lens                           hiding (index)
import           Control.Monad
import qualified Data.Text                              as T
import qualified Graphics.Rendering.Chart.Backend.Cairo as Chart
import           Graphics.UI.Gtk                        hiding (set)
import qualified Graphics.UI.Gtk                        as Gtk (set)

import           Controller.AppState
import           Controller.GUIElements
import           Controller.GUIState
import           Types.LevelShifts

--------------------------------------------------------------------------------

data SyncWidget
  = SyncSpinButtonDouble (GUIElements -> SpinButton)  (Lens' GUIState Double)
  | SyncSpinButtonInt    (GUIElements -> SpinButton)  (Lens' GUIState Int)
  | SyncCheckButton      (GUIElements -> CheckButton) (Lens' GUIState Bool)
  | forall a. SyncComboBoxText
      (GUIElements -> ComboBox) (Lens' GUIState (Int, a)) (Maybe T.Text -> a)
  | forall a. (Bounded a, Enum a) => SyncRadioButtonGroup
      (Lens' GUIState a) (a -> GUIElements -> RadioButton)

--------------------------------------------------------------------------------

syncWidgets :: [SyncWidget]
syncWidgets =
  -- Auto page
  [ SyncSpinButtonInt matchLevelSpinButton matchLevel

  -- Single page
  , SyncComboBoxText singleHoldComboBox singleHold $
    \case Just "Left"  -> HoldLeft
          Just "Right" -> HoldRight
          _            -> HoldLeft
  , SyncSpinButtonDouble singleOffsetSpinButton   singleOffset
  , SyncRadioButtonGroup singleAction $ \case
      SingleIgnore   -> singleIgnoreRadioButton
      SingleZero     -> singleZeroRadioButton
      SingleSlopeFit -> singleSlopeFitRadioButton

  -- Multiple page
  , SyncSpinButtonDouble multipleOffsetSpinButton multipleOffset
  , SyncRadioButtonGroup multipleAction $ \case
      MultipleIgnore -> multipleIgnoreRadioButton
      MultipleLine   -> multipleLineRadioButton
      MultipleCancel -> multipleCancelRadioButton

  -- Label page
  , SyncSpinButtonDouble noiseThresholdSpinButton      noiseThreshold
  , SyncSpinButtonDouble levelShiftThresholdSpinButton levelShiftThreshold

  -- View page
  , SyncCheckButton showReplicateTracesCheckButton showReplicateTraces
  , SyncComboBoxText referenceTraceComboBoxText referenceTraceLabel $
    \case Just "None" -> Nothing
          m           -> m

  -- Screenshot page
  , SyncComboBoxText screenshotFileFormatComboBoxText screenshotFileFormat $
    \case Just "PNG" -> Chart.PNG
          Just "SVG" -> Chart.SVG
          Just "PS"  -> Chart.PS
          Just "PDF" -> Chart.PDF
          _          -> Chart.PNG
  ]

--------------------------------------------------------------------------------

registerGUIStateWidgets ::
     GUIElements
  -> AppStateHandle
  -> (IO () -> IO ())
  -> IO ()
registerGUIStateWidgets guiElems appH withUpdate =
  forM_ syncWidgets $ registerGUIStateWidget guiElems appH withUpdate

registerGUIStateWidget
  :: GUIElements
  -> AppStateHandle
  -> (IO () -> IO ())
  -> SyncWidget
  -> IO ()
registerGUIStateWidget guiElems appH withUpdate syncCallback =
  case syncCallback of
    SyncSpinButtonDouble spinButtonRef stateRef -> do
      let spinButton = spinButtonRef guiElems
      void $ afterValueSpinned spinButton $ withUpdate $ do
        val <- spinButtonGetValue spinButton
        atomically $ modifyAppGUIState appH
                   $ set stateRef val

    SyncSpinButtonInt spinButtonRef stateRef -> do
      let spinButton = spinButtonRef guiElems
      void $ afterValueSpinned spinButton $ withUpdate $ do
        val <- spinButtonGetValueAsInt spinButton
        atomically $ modifyAppGUIState appH
                   $ set stateRef val

    SyncCheckButton checkButtonRef stateRef -> do
      let checkButton = checkButtonRef guiElems
      void $ on checkButton buttonActivated $ withUpdate $ do
        active <- toggleButtonGetActive checkButton
        atomically $ modifyAppGUIState appH
                  $ set stateRef active

    SyncComboBoxText comboBoxRef stateRef reader -> do
      let comboBox = comboBoxRef guiElems
      void $ on comboBox changed $ withUpdate $ do
        index <- comboBoxGetActive comboBox
        listStore <- comboBoxGetModelText comboBox
        txt <- listStoreSafeGetValue listStore index
        atomically $ modifyAppGUIState appH
              $ set stateRef (index, reader txt)

    SyncRadioButtonGroup stateRef grouping -> do
      forM_ listAll $ \val -> do
        let radioButton = grouping val guiElems
        void $ on radioButton buttonActivated $ do
          active <- toggleButtonGetActive radioButton
          when active $ withUpdate $
            atomically $ modifyAppGUIState appH $ set stateRef val

--------------------------------------------------------------------------------

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
updateGUIStateWidget guiElems guiState syncCallback = do
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

-------------------------------------------------------------------------------
-- Utility
-------------------------------------------------------------------------------

listAll :: (Bounded a, Enum a) => [a]
listAll = [minBound..maxBound]
