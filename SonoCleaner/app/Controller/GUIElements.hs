-- Data type that hold references to all relevant elements of the GUI

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Controller.GUIElements where

import           Control.Lens
import           Control.Monad
import qualified Data.Text           as T
import           Graphics.UI.Gtk     hiding (set)

--------------------------------------------------------------------------------
-- GUI elements
--------------------------------------------------------------------------------

data GUIElements = GUIElements
  -- Main window
  { _controllerWindow               :: Window
  , _image                          :: Image
  , _imageEventBox                  :: EventBox
  , _controllerWindowBox            :: Box
  , _notebook                       :: Notebook

  -- Main page
  , _openButton                     :: Button
  , _saveButton                     :: Button

  , _prevTraceButton                :: Button
  , _nextTraceButton                :: Button
  , _twinTraceButton                :: Button

  , _undoButton                     :: Button
  , _redoButton                     :: Button

  , _autoButton                     :: Button
  , _labellingButton                :: Button

  , _viewButton                     :: Button
  , _cropButton                     :: Button
  , _qualityButton                  :: Button

  , _mainFullViewButton             :: Button
  , _mainFullViewXButton            :: Button
  , _mainFullViewYButton            :: Button

  -- Auto page
  , _matchLevelSpinButton           :: SpinButton

  , _autoCancelButton               :: Button
  , _autoApplyButton                :: Button

  -- Single page
  , _singleHoldComboBox             :: ComboBox
  , _singleOffsetSpinButton         :: SpinButton

  , _singleIgnoreRadioButton        :: RadioButton
  , _singleZeroRadioButton          :: RadioButton
  , _singleSlopeFitRadioButton      :: RadioButton

  , _singleCancelButton             :: Button
  , _singleApplyButton              :: Button

  -- Multiple page
  , _multipleOffsetSpinButton       :: SpinButton

  , _multipleIgnoreRadioButton      :: RadioButton
  , _multipleLineRadioButton        :: RadioButton
  , _multipleCancelRadioButton      :: RadioButton

  , _multipleCancelButton           :: Button
  , _multipleApplyButton            :: Button

    -- Label page
  , _levelShiftThresholdSpinButton  :: SpinButton
  , _noiseThresholdSpinButton       :: SpinButton

  , _defaultParametersButton        :: Button

  , _labelBackButton                :: Button

    -- View page
  , _showReplicateTracesCheckButton :: CheckButton

  , _referenceTraceComboBoxText     :: ComboBox

  , _viewBackButton                 :: Button

    -- Crop page
  , _applyCropButton                :: Button
  , _applyUncropButton              :: Button

  , _cropBackButton                 :: Button

    -- Crop page
  , _qualityGoodButton              :: Button
  , _qualityModerateButton          :: Button
  , _qualityBadButton               :: Button

  , _qualityBackButton              :: Button
  }

makeLenses ''GUIElements

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
    _ <- comboBoxSetModelText (guiElems ^. referenceTraceComboBoxText)

    let shcb = guiElems ^. singleHoldComboBox
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
  _controllerWindow <- builderGetObject builder castToWindow "controllerWindow"
  _image <- builderGetObject builder castToImage "image"
  _imageEventBox <- builderGetObject builder castToEventBox "imageEventBox"
  _controllerWindowBox <- builderGetObject builder castToBox "controllerWindowBox"
  _notebook <- builderGetObject builder castToNotebook "notebook"

  -- Main page
  _openButton <- getButton "openButton"
  _saveButton <- getButton "saveButton"

  _prevTraceButton <- getButton "prevTraceButton"
  _nextTraceButton <- getButton "nextTraceButton"
  _twinTraceButton <- getButton "twinTraceButton"

  _undoButton <- getButton "undoButton"
  _redoButton <- getButton "redoButton"

  _autoButton <- getButton "autoButton"
  _labellingButton <- getButton "labellingButton"

  _viewButton <- getButton "viewButton"
  _cropButton <- getButton "cropButton"
  _qualityButton <- getButton "qualityButton"

  _mainFullViewButton <- getButton "mainFullViewButton"
  _mainFullViewXButton <- getButton "mainFullViewXButton"
  _mainFullViewYButton <- getButton "mainFullViewYButton"

  -- Auto page
  _matchLevelSpinButton <- getSpinButton "matchLevelSpinButton"

  _autoCancelButton <- getButton "autoCancelButton"
  _autoApplyButton <- getButton "autoApplyButton"

  -- Single page
  _singleHoldComboBox <- builderGetObject builder castToComboBox "singleHoldComboBox"
  _singleOffsetSpinButton <- getSpinButton "singleOffsetSpinButton"

  _singleIgnoreRadioButton <- getRadioButton "singleIgnoreRadioButton"
  _singleZeroRadioButton <- getRadioButton "singleZeroRadioButton"
  _singleSlopeFitRadioButton <- getRadioButton "singleSlopeFitRadioButton"

  _singleCancelButton <- getButton "singleCancelButton"
  _singleApplyButton <- getButton "singleApplyButton"

  -- Multiple page
  _multipleOffsetSpinButton <- getSpinButton "multipleOffsetSpinButton"

  _multipleIgnoreRadioButton <- getRadioButton "multipleIgnoreRadioButton"
  _multipleLineRadioButton <- getRadioButton "multipleLineRadioButton"
  _multipleCancelRadioButton <- getRadioButton "multipleCancelRadioButton"

  _multipleCancelButton <- getButton "multipleCancelButton"
  _multipleApplyButton <- getButton "multipleApplyButton"

  -- Label page
  _levelShiftThresholdSpinButton <- getSpinButton "levelShiftThresholdSpinButton"
  _noiseThresholdSpinButton <- getSpinButton "noiseThresholdSpinButton"

  _defaultParametersButton <- getButton "defaultParametersButton"

  _labelBackButton <- getButton "labelBackButton"

  -- View page
  _showReplicateTracesCheckButton <- getCheckButton "showReplicateTracesCheckButton"

  _referenceTraceComboBoxText <- builderGetObject builder castToComboBox "referenceTraceComboBoxText"

  _viewBackButton <- getButton "viewBackButton"

  -- Crop page
  _applyCropButton <- getButton "applyCropButton"
  _applyUncropButton <- getButton "applyUncropButton"

  _cropBackButton <- getButton "cropBackButton"

  -- Quality page
  _qualityGoodButton <- getButton "qualityGoodButton"
  _qualityModerateButton <- getButton "qualityModerateButton"
  _qualityBadButton <- getButton "qualityBadButton"

  _qualityBackButton <- getButton "qualityBackButton"

  pure GUIElements{..}
