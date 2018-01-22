-- Data type that hold references to all relevant elements of the GUI

{-# LANGUAGE RecordWildCards #-}

module Controller.GUIElements
  ( GUIElements (..)
  , importGUIElements
  , setComboBoxTextLabels
  ) where

import qualified Data.Text       as T
import           Graphics.UI.Gtk hiding (set)

--------------------------------------------------------------------------------

data GUIElements = GUIElements
  -- Main window
  { controllerWindow                 :: Window
  , image                            :: Image
  , imageEventBox                    :: EventBox
  , controllerWindowBox              :: Box
  , notebook                         :: Notebook

  -- Main page
  , openButton                       :: Button
  , saveButton                       :: Button

  , prevTraceButton                  :: Button
  , nextTraceButton                  :: Button
  , twinTraceButton                  :: Button

  , undoButton                       :: Button
  , redoButton                       :: Button

  , autoButton                       :: Button
  , labellingButton                  :: Button

  , viewButton                       :: Button
  , cropButton                       :: Button
  , qualityButton                    :: Button

  , screenshotButton                 :: Button

  , mainFullViewButton               :: Button
  , mainFullViewXButton              :: Button
  , mainFullViewYButton              :: Button

  -- Auto page
  , matchLevelSpinButton             :: SpinButton

  , autoCancelButton                 :: Button
  , autoApplyButton                  :: Button

  -- Single page
  , singleHoldComboBox               :: ComboBox
  , singleOffsetSpinButton           :: SpinButton

  , singleIgnoreRadioButton          :: RadioButton
  , singleZeroRadioButton            :: RadioButton
  , singleSlopeFitRadioButton        :: RadioButton

  , singleCancelButton               :: Button
  , singleApplyButton                :: Button

  -- Multiple page
  , multipleOffsetSpinButton         :: SpinButton

  , multipleIgnoreRadioButton        :: RadioButton
  , multipleLineRadioButton          :: RadioButton
  , multipleCancelRadioButton        :: RadioButton

  , multipleCancelButton             :: Button
  , multipleApplyButton              :: Button

    -- Label page
  , levelShiftThresholdSpinButton    :: SpinButton
  , noiseThresholdSpinButton         :: SpinButton

  , defaultParametersButton          :: Button

  , labelBackButton                  :: Button

    -- View page
  , showReplicateTracesCheckButton   :: CheckButton

  , referenceTraceComboBoxText       :: ComboBox

  , viewBackButton                   :: Button

    -- Crop page
  , applyCropButton                  :: Button
  , applyUncropButton                :: Button

  , cropBackButton                   :: Button

    -- Quality page
  , qualityGoodButton                :: Button
  , qualityModerateButton            :: Button
  , qualityBadButton                 :: Button

  , qualityBackButton                :: Button

    -- Screenshot page
  , screenshotFileFormatComboBoxText :: ComboBox
  , screenshotSaveButton             :: Button

  , screenshotBackButton             :: Button
  }

--------------------------------------------------------------------------------

setComboBoxTextLabels :: [String] -> ComboBox -> IO ()
setComboBoxTextLabels labels comboBox = do
  _ <- comboBoxSetModelText comboBox
  mapM_ (comboBoxAppendText comboBox . T.pack) labels
  comboBoxSetActive comboBox 0

(<>>=) :: Monad m => m a -> (a -> m b) -> m a
(<>>=) ma mf = do
  a <- ma
  _ <- mf a
  pure a

importGUIElements :: Builder -> IO GUIElements
importGUIElements builder = do
  let getButton      = builderGetObject builder castToButton
      getSpinButton  = builderGetObject builder castToSpinButton
      getCheckButton = builderGetObject builder castToCheckButton
      getRadioButton = builderGetObject builder castToRadioButton
      getComboBox    = builderGetObject builder castToComboBox

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

  screenshotButton <- getButton "screenshotButton"

  mainFullViewButton <- getButton "mainFullViewButton"
  mainFullViewXButton <- getButton "mainFullViewXButton"
  mainFullViewYButton <- getButton "mainFullViewYButton"

  -- Auto page
  matchLevelSpinButton <- getSpinButton "matchLevelSpinButton"

  autoCancelButton <- getButton "autoCancelButton"
  autoApplyButton <- getButton "autoApplyButton"

  -- Single page
  singleHoldComboBox <- getComboBox "singleHoldComboBox"
                   <>>= setComboBoxTextLabels ["Left", "Right"]

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

  referenceTraceComboBoxText <- getComboBox "referenceTraceComboBoxText"

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

  -- Screenshot page
  screenshotFileFormatComboBoxText <-
         getComboBox "screenshotFileFormatComboBoxText"
    <>>= setComboBoxTextLabels ["PNG", "SVG", "PS", "PDF"]
  screenshotSaveButton <- getButton "screenshotSaveButton"

  screenshotBackButton <- getButton "screenshotBackButton"

  pure GUIElements{..}
