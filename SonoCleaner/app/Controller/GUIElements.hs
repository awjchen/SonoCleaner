-- Data type that hold references to all relevant elements of the GUI

{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}

module Controller.GUIElements where

import           Control.Applicative
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

    return GUIElements{}
      -- Controller window
      <**> (set controllerWindow    <$> builderGetObject builder castToWindow "controllerWindow")
      <**> (set image               <$> builderGetObject builder castToImage "image")
      <**> (set imageEventBox       <$> builderGetObject builder castToEventBox "imageEventBox")
      <**> (set controllerWindowBox <$> builderGetObject builder castToBox "controllerWindowBox")
      <**> (set notebook            <$> builderGetObject builder castToNotebook "notebook")

      -- Main page
      <**> (set openButton <$> getButton "openButton")
      <**> (set saveButton <$> getButton "saveButton")

      <**> (set prevTraceButton <$> getButton "prevTraceButton")
      <**> (set nextTraceButton <$> getButton "nextTraceButton")
      <**> (set twinTraceButton <$> getButton "twinTraceButton")

      <**> (set undoButton <$> getButton "undoButton")
      <**> (set redoButton <$> getButton "redoButton")

      <**> (set autoButton      <$> getButton "autoButton")
      <**> (set labellingButton <$> getButton "labellingButton")

      <**> (set viewButton      <$> getButton "viewButton")
      <**> (set cropButton      <$> getButton "cropButton")
      <**> (set qualityButton   <$> getButton "qualityButton")

      <**> (set mainFullViewButton  <$> getButton "mainFullViewButton")
      <**> (set mainFullViewXButton <$> getButton "mainFullViewXButton")
      <**> (set mainFullViewYButton <$> getButton "mainFullViewYButton")

      -- Auto page
      <**> (set matchLevelSpinButton          <$> getSpinButton "matchLevelSpinButton")

      <**> (set autoCancelButton <$> getButton "autoCancelButton")
      <**> (set autoApplyButton  <$> getButton "autoApplyButton")

      -- Single page
      <**> (set singleHoldComboBox     <$> builderGetObject builder castToComboBox "singleHoldComboBox")
      <**> (set singleOffsetSpinButton <$> getSpinButton "singleOffsetSpinButton")

      <**> (set singleIgnoreRadioButton        <$> getRadioButton "singleIgnoreRadioButton")
      <**> (set singleZeroRadioButton          <$> getRadioButton "singleZeroRadioButton")
      <**> (set singleSlopeFitRadioButton      <$> getRadioButton "singleSlopeFitRadioButton")

      <**> (set singleCancelButton <$> getButton "singleCancelButton")
      <**> (set singleApplyButton  <$> getButton "singleApplyButton")

      -- Multiple page
      <**> (set multipleOffsetSpinButton <$> getSpinButton "multipleOffsetSpinButton")

      <**> (set multipleIgnoreRadioButton <$> getRadioButton "multipleIgnoreRadioButton")
      <**> (set multipleLineRadioButton   <$> getRadioButton "multipleLineRadioButton")
      <**> (set multipleCancelRadioButton <$> getRadioButton "multipleCancelRadioButton")

      <**> (set multipleCancelButton <$> getButton "multipleCancelButton")
      <**> (set multipleApplyButton  <$> getButton "multipleApplyButton")

      -- Label page
      <**> (set levelShiftThresholdSpinButton <$> getSpinButton "levelShiftThresholdSpinButton")
      <**> (set noiseThresholdSpinButton      <$> getSpinButton "noiseThresholdSpinButton")

      <**> (set defaultParametersButton <$> getButton "defaultParametersButton")

      <**> (set labelBackButton <$> getButton "labelBackButton")

      -- View page
      <**> (set showReplicateTracesCheckButton <$> getCheckButton "showReplicateTracesCheckButton")

      <**> (set referenceTraceComboBoxText <$> builderGetObject builder castToComboBox "referenceTraceComboBoxText")

      <**> (set viewBackButton <$> getButton "viewBackButton")

      -- Crop page
      <**> (set applyCropButton   <$> getButton "applyCropButton")
      <**> (set applyUncropButton <$> getButton "applyUncropButton")

      <**> (set cropBackButton <$> getButton "cropBackButton")

      -- Quality page
      <**> (set qualityGoodButton     <$> getButton "qualityGoodButton")
      <**> (set qualityModerateButton <$> getButton "qualityModerateButton")
      <**> (set qualityBadButton      <$> getButton "qualityBadButton")

      <**> (set qualityBackButton <$> getButton "qualityBackButton")
