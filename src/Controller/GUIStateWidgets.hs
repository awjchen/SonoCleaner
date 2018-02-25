-- TODO: summary

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}

module Controller.GUIStateWidgets
  ( SyncWidget (..)
  , syncWidgets
  ) where

import           Control.Lens                           hiding (index)
import qualified Data.Text                              as T
import qualified Graphics.Rendering.Chart.Backend.Cairo as Chart
import           Graphics.UI.Gtk                        hiding (set)

import           Controller.GUIElements
import           Controller.GUIState
import           Types.LevelShifts


data SyncWidget
  = SyncSpinButtonDouble (GUIElements -> SpinButton)  (Lens' GUIState Double)
  | SyncSpinButtonInt    (GUIElements -> SpinButton)  (Lens' GUIState Int)
  | SyncCheckButton      (GUIElements -> CheckButton) (Lens' GUIState Bool)
  | forall a. SyncComboBoxText
      (GUIElements -> ComboBox) (Lens' GUIState (Int, a)) (Maybe T.Text -> a)
  | forall a. (Bounded a, Enum a) => SyncRadioButtonGroup
      (Lens' GUIState a) (a -> GUIElements -> RadioButton)

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
