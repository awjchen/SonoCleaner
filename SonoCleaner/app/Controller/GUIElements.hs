-- Data type that hold references to all relevant elements of the GUI

{-# LANGUAGE RecordWildCards #-}

module Controller.GUIElements where

import           Control.Monad   (void)
import qualified Data.Text       as T
import           Graphics.UI.Gtk hiding (set)

--------------------------------------------------------------------------------
-- GUI elements
--------------------------------------------------------------------------------

data GUIElements = GUIElements
  -- Main window
  { controllerWindow               :: Window
  , image                          :: Image
  , imageEventBox                  :: EventBox
  , controllerWindowBox            :: Box
  , notebook                       :: Notebook

  -- Main page
  , openButton                     :: Button
  , saveButton                     :: Button

  , prevTraceButton                :: Button
  , nextTraceButton                :: Button
  , twinTraceButton                :: Button

  , undoButton                     :: Button
  , redoButton                     :: Button

  , autoButton                     :: Button
  , labellingButton                :: Button

  , viewButton                     :: Button
  , cropButton                     :: Button
  , qualityButton                  :: Button

  , mainFullViewButton             :: Button
  , mainFullViewXButton            :: Button
  , mainFullViewYButton            :: Button

  -- Auto page
  , matchLevelSpinButton           :: SpinButton

  , autoCancelButton               :: Button
  , autoApplyButton                :: Button

  -- Single page
  , singleHoldComboBox             :: ComboBox
  , singleOffsetSpinButton         :: SpinButton

  , singleIgnoreRadioButton        :: RadioButton
  , singleZeroRadioButton          :: RadioButton
  , singleSlopeFitRadioButton      :: RadioButton

  , singleCancelButton             :: Button
  , singleApplyButton              :: Button

  -- Multiple page
  , multipleOffsetSpinButton       :: SpinButton

  , multipleIgnoreRadioButton      :: RadioButton
  , multipleLineRadioButton        :: RadioButton
  , multipleCancelRadioButton      :: RadioButton

  , multipleCancelButton           :: Button
  , multipleApplyButton            :: Button

    -- Label page
  , levelShiftThresholdSpinButton  :: SpinButton
  , noiseThresholdSpinButton       :: SpinButton

  , defaultParametersButton        :: Button

  , labelBackButton                :: Button

    -- View page
  , showReplicateTracesCheckButton :: CheckButton

  , referenceTraceComboBoxText     :: ComboBox

  , viewBackButton                 :: Button

    -- Crop page
  , applyCropButton                :: Button
  , applyUncropButton              :: Button

  , cropBackButton                 :: Button

    -- Crop page
  , qualityGoodButton              :: Button
  , qualityModerateButton          :: Button
  , qualityBadButton               :: Button

  , qualityBackButton              :: Button
  }

--------------------------------------------------------------------------------
-- Import GUI elements from Glade
--------------------------------------------------------------------------------

importGUIElements :: Builder -> IO GUIElements
importGUIElements builder = go where
  go = do
    guiElems <- importGUIElements' builder
    finalize guiElems
    return guiElems

  finalize :: GUIElements -> IO ()
  finalize guiElems = void $ do
    -- Set model of ComboBoxes to Text
    _ <- comboBoxSetModelText (referenceTraceComboBoxText guiElems)

    let shcb = singleHoldComboBox guiElems
    _ <- comboBoxSetModelText shcb
    _ <- comboBoxAppendText shcb (T.pack "Left")
    comboBoxAppendText shcb (T.pack "Right")

importGUIElements' :: Builder -> IO GUIElements
importGUIElements' builder = do
  let getButton      = builderGetObject builder castToButton
      getSpinButton  = builderGetObject builder castToSpinButton
      getCheckButton = builderGetObject builder castToCheckButton
      getRadioButton = builderGetObject builder castToRadioButton

  -- Controller window
  controllerWindow <- builderGetObject builder castToWindow "controllerWindow"
  image <- builderGetObject builder castToImage "image"
  imageEventBox <- builderGetObject builder castToEventBox "imageEventBox"
  controllerWindowBox <- builderGetObject builder castToBox "controllerWindowBox"
  notebook <- builderGetObject builder castToNotebook "notebook"

  -- Main page
  openButton <- getButton "openButton"
  saveButton <- getButton "saveButton"

  prevTraceButton <- getButton "prevTraceButton"
  nextTraceButton <- getButton "nextTraceButton"
  twinTraceButton <- getButton "twinTraceButton"

  undoButton <- getButton "undoButton"
  redoButton <- getButton "redoButton"

  autoButton <- getButton "autoButton"
  labellingButton <- getButton "labellingButton"

  viewButton <- getButton "viewButton"
  cropButton <- getButton "cropButton"
  qualityButton <- getButton "qualityButton"

  mainFullViewButton <- getButton "mainFullViewButton"
  mainFullViewXButton <- getButton "mainFullViewXButton"
  mainFullViewYButton <- getButton "mainFullViewYButton"

  -- Auto page
  matchLevelSpinButton <- getSpinButton "matchLevelSpinButton"

  autoCancelButton <- getButton "autoCancelButton"
  autoApplyButton <- getButton "autoApplyButton"

  -- Single page
  singleHoldComboBox <- builderGetObject builder castToComboBox "singleHoldComboBox"
  singleOffsetSpinButton <- getSpinButton "singleOffsetSpinButton"

  singleIgnoreRadioButton <- getRadioButton "singleIgnoreRadioButton"
  singleZeroRadioButton <- getRadioButton "singleZeroRadioButton"
  singleSlopeFitRadioButton <- getRadioButton "singleSlopeFitRadioButton"

  singleCancelButton <- getButton "singleCancelButton"
  singleApplyButton <- getButton "singleApplyButton"

  -- Multiple page
  multipleOffsetSpinButton <- getSpinButton "multipleOffsetSpinButton"

  multipleIgnoreRadioButton <- getRadioButton "multipleIgnoreRadioButton"
  multipleLineRadioButton <- getRadioButton "multipleLineRadioButton"
  multipleCancelRadioButton <- getRadioButton "multipleCancelRadioButton"

  multipleCancelButton <- getButton "multipleCancelButton"
  multipleApplyButton <- getButton "multipleApplyButton"

  -- Label page
  levelShiftThresholdSpinButton <- getSpinButton "levelShiftThresholdSpinButton"
  noiseThresholdSpinButton <- getSpinButton "noiseThresholdSpinButton"

  defaultParametersButton <- getButton "defaultParametersButton"

  labelBackButton <- getButton "labelBackButton"

  -- View page
  showReplicateTracesCheckButton <- getCheckButton "showReplicateTracesCheckButton"

  referenceTraceComboBoxText <- builderGetObject builder castToComboBox "referenceTraceComboBoxText"

  viewBackButton <- getButton "viewBackButton"

  -- Crop page
  applyCropButton <- getButton "applyCropButton"
  applyUncropButton <- getButton "applyUncropButton"

  cropBackButton <- getButton "cropBackButton"

  -- Quality page
  qualityGoodButton <- getButton "qualityGoodButton"
  qualityModerateButton <- getButton "qualityModerateButton"
  qualityBadButton <- getButton "qualityBadButton"

  qualityBackButton <- getButton "qualityBackButton"

  pure GUIElements{..}
