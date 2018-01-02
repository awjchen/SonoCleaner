-- Controlling the sensitivity of GUI elements depending on the GUI state

module Controller.Sensitivity
  ( setGUISensitivity
  , insensitizeAll
  ) where

import           Control.Lens
import           Data.Maybe             (isJust)
import           Graphics.UI.Gtk        hiding (set)

import           Controller.GUIElements
import           Controller.GUIState
import           Model
import           Types.LevelShifts

--------------------------------------------------------------------------------
-- GUI element sensitivity and visibility
--------------------------------------------------------------------------------

setGUISensitivity ::
     GUIElements
  -> Model
  -> GUIState
  -> IO ()
setGUISensitivity guiElems model guiState = case guiState ^. currentPage of
  MainPage -> do
    widgetSetSensitive (openButton guiElems) True
    widgetSetSensitive (saveButton guiElems) True

    widgetSetSensitive (prevTraceButton guiElems) (existsPrevTrace model)
    widgetSetSensitive (nextTraceButton guiElems) (existsNextTrace model)
    widgetSetSensitive (twinTraceButton guiElems)
      $ case getTwinTrace model of
          Nothing -> False
          Just _  -> True

    widgetSetSensitive (undoButton guiElems) (existsPrevHistory model)
    widgetSetSensitive (redoButton guiElems) (existsNextHistory model)

    widgetSetSensitive (autoButton guiElems)      True
    widgetSetSensitive (labellingButton guiElems) True

    widgetSetSensitive (viewButton guiElems)    True
    widgetSetSensitive (cropButton guiElems)    True
    widgetSetSensitive (qualityButton guiElems) True

    widgetSetSensitive (mainFullViewButton guiElems)  True
    widgetSetSensitive (mainFullViewXButton guiElems) True
    widgetSetSensitive (mainFullViewYButton guiElems) True

    widgetSetSensitive (showReplicateTracesCheckButton guiElems) True

  AutoPage -> do
    widgetSetSensitive (autoApplyButton guiElems)
                       (guiState ^. matchLevel /= 0)

  SinglePage _ -> do
    let singleSensitivty = case guiState ^. singleAction of
          SingleIgnore -> False
          _            -> True
    widgetSetSensitive (singleApplyButton guiElems)      singleSensitivty
    widgetSetSensitive (singleOffsetSpinButton guiElems) singleSensitivty
    widgetSetSensitive (singleHoldComboBox guiElems)     singleSensitivty

  MultiplePage _ -> do
    widgetSetSensitive (multipleOffsetSpinButton guiElems)
      $ (guiState ^. multipleAction) == MultipleCancel

    widgetSetSensitive (multipleApplyButton guiElems) $
      case guiState ^. multipleAction of
            MultipleIgnore -> False
            _              -> True

  CropPage mCropInterval -> do
    widgetSetSensitive (applyCropButton guiElems)
      $ isJust mCropInterval
    widgetSetSensitive (applyUncropButton guiElems)
      $ case getCurrentState model ^. context of
          RootContext      -> False
          CroppedContext _ -> True

  _ -> return ()

--------------------------------------------------------------------------------
-- Disable functions before a file is opened
--------------------------------------------------------------------------------

insensitizeAll :: GUIElements -> IO ()
insensitizeAll guiElems = do
  widgetSetSensitive (openButton guiElems)      True
  widgetSetSensitive (prevTraceButton guiElems) False
  widgetSetSensitive (nextTraceButton guiElems) False
  widgetSetSensitive (twinTraceButton guiElems) False
  widgetSetSensitive (saveButton guiElems)      False
  widgetSetSensitive (undoButton guiElems)      False
  widgetSetSensitive (redoButton guiElems)      False
  widgetSetSensitive (autoButton guiElems)      False
  widgetSetSensitive (labellingButton guiElems) False
  widgetSetSensitive (viewButton guiElems)      False
  widgetSetSensitive (cropButton guiElems)      False
  widgetSetSensitive (qualityButton guiElems)   False
  widgetSetSensitive (mainFullViewButton guiElems)  False
  widgetSetSensitive (mainFullViewXButton guiElems) False
  widgetSetSensitive (mainFullViewYButton guiElems) False
  widgetSetSensitive (showReplicateTracesCheckButton guiElems) False
