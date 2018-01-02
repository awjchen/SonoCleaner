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
    widgetSetSensitive (guiElems ^. openButton) True
    widgetSetSensitive (guiElems ^. saveButton) True

    widgetSetSensitive (guiElems ^. prevTraceButton) (existsPrevTrace model)
    widgetSetSensitive (guiElems ^. nextTraceButton) (existsNextTrace model)
    widgetSetSensitive (guiElems ^. twinTraceButton)
      $ case getTwinTrace model of
          Nothing -> False
          Just _  -> True

    widgetSetSensitive (guiElems ^. undoButton) (existsPrevHistory model)
    widgetSetSensitive (guiElems ^. redoButton) (existsNextHistory model)

    widgetSetSensitive (guiElems ^. autoButton)      True
    widgetSetSensitive (guiElems ^. labellingButton) True

    widgetSetSensitive (guiElems ^. viewButton)    True
    widgetSetSensitive (guiElems ^. cropButton)    True
    widgetSetSensitive (guiElems ^. qualityButton) True

    widgetSetSensitive (guiElems ^. mainFullViewButton)  True
    widgetSetSensitive (guiElems ^. mainFullViewXButton) True
    widgetSetSensitive (guiElems ^. mainFullViewYButton) True

    widgetSetSensitive (guiElems ^. showReplicateTracesCheckButton) True

  AutoPage -> do
    widgetSetSensitive (guiElems ^. autoApplyButton)
                       (guiState ^. matchLevel /= 0)

  SinglePage _ -> do
    let singleSensitivty = case guiState ^. singleAction of
          SingleIgnore -> False
          _            -> True
    widgetSetSensitive (guiElems ^. singleApplyButton)      singleSensitivty
    widgetSetSensitive (guiElems ^. singleOffsetSpinButton) singleSensitivty
    widgetSetSensitive (guiElems ^. singleHoldComboBox)     singleSensitivty

  MultiplePage _ -> do
    widgetSetSensitive (guiElems ^. multipleOffsetSpinButton)
      $ (guiState ^. multipleAction) == MultipleCancel

    widgetSetSensitive (guiElems ^. multipleApplyButton) $
      case guiState ^. multipleAction of
            MultipleIgnore -> False
            _              -> True

  CropPage mCropInterval -> do
    widgetSetSensitive (guiElems ^. applyCropButton)
      $ isJust mCropInterval
    widgetSetSensitive (guiElems ^. applyUncropButton)
      $ case getCurrentState model ^. context of
          RootContext      -> False
          CroppedContext _ -> True

  _ -> return ()

--------------------------------------------------------------------------------
-- Disable functions before a file is opened
--------------------------------------------------------------------------------

insensitizeAll :: GUIElements -> IO ()
insensitizeAll guiElems = do
  widgetSetSensitive (guiElems ^. openButton)      True
  widgetSetSensitive (guiElems ^. prevTraceButton) False
  widgetSetSensitive (guiElems ^. nextTraceButton) False
  widgetSetSensitive (guiElems ^. twinTraceButton) False
  widgetSetSensitive (guiElems ^. saveButton)      False
  widgetSetSensitive (guiElems ^. undoButton)      False
  widgetSetSensitive (guiElems ^. redoButton)      False
  widgetSetSensitive (guiElems ^. autoButton)      False
  widgetSetSensitive (guiElems ^. labellingButton) False
  widgetSetSensitive (guiElems ^. viewButton)      False
  widgetSetSensitive (guiElems ^. cropButton)      False
  widgetSetSensitive (guiElems ^. qualityButton)   False
  widgetSetSensitive (guiElems ^. mainFullViewButton)  False
  widgetSetSensitive (guiElems ^. mainFullViewXButton) False
  widgetSetSensitive (guiElems ^. mainFullViewYButton) False
  widgetSetSensitive (guiElems ^. showReplicateTracesCheckButton) False
