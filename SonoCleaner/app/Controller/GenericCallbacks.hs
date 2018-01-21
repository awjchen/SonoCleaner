-- Generic GUI actions (callbacks) are defined here in a uniform and restricted
-- manner

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}

module Controller.GenericCallbacks
  ( registerCallbacks
  , setGUIParameters
  ) where

import           Control.Concurrent.STM
import           Control.Lens                           hiding (index)
import           Control.Monad
import           Data.Default
import qualified Data.Text                              as T
import qualified Graphics.Rendering.Chart.Backend.Cairo as Chart
import           Graphics.UI.Gtk                        hiding (set)
import qualified Graphics.UI.Gtk                        as Gtk (set)

import           Controller.GUIElements
import           Controller.GUIState
import           Controller.Util
import           Model
import           Types.LevelShifts

--------------------------------------------------------------------------------
-- Callback specifications (see below for type definitions)
--------------------------------------------------------------------------------

buttons :: [ButtonCB]
buttons =
  -- Main page
  [ ButtonCB prevTraceButton gotoPrevTrace resetGUIForNewTrace
  , ButtonCB nextTraceButton gotoNextTrace resetGUIForNewTrace
  , ButtonCB twinTraceButton gotoTwinTrace resetGUIPreservingOptions'

  , ButtonCB undoButton undo noop
  , ButtonCB redoButton redo noop

  , ButtonCB labellingButton  id (setNotebookPage LabelPage)
  , ButtonCB viewButton       id (setNotebookPage ViewPage)
  , ButtonCB cropButton       id (setNotebookPage (CropPage Nothing))
  , ButtonCB qualityButton    id (setNotebookPage QualityPage)
  , ButtonCB screenshotButton id (setNotebookPage ScreenshotPage)

  , ButtonCB mainFullViewButton  id (const setDefaultViewBounds)
  , ButtonCB mainFullViewXButton id (const setDefaultViewBoundsX)
  , ButtonCB mainFullViewYButton id (const setDefaultViewBoundsY)

  , ButtonCB autoCancelButton     id resetGUIPreservingOptions'
  , ButtonCB singleCancelButton   id resetGUIPreservingOptions'
  , ButtonCB multipleCancelButton id resetGUIPreservingOptions'

  -- Label page
  , ButtonCB defaultParametersButton id
      (\_ _ -> set levelShiftThreshold (view levelShiftThreshold def)
             . set noiseThreshold      (view noiseThreshold def))
  , ButtonCB labelBackButton id resetGUIPreservingOptions'

  -- View page
  , ButtonCB viewBackButton id resetGUIPreservingOptions'

  -- Crop page
  , ButtonCB applyUncropButton uncrop resetGUIPreservingOptions'

  , ButtonCB cropBackButton id resetGUIPreservingOptions'

  -- Quality page
  , ButtonCB qualityGoodButton     (setQuality Good)
      resetGUIPreservingOptions'
  , ButtonCB qualityModerateButton (setQuality Moderate)
      resetGUIPreservingOptions'
  , ButtonCB qualityBadButton      (setQuality Bad)
      resetGUIPreservingOptions'

  , ButtonCB qualityBackButton id resetGUIPreservingOptions'

  -- Screenshot page
  , ButtonCB screenshotBackButton id resetGUIPreservingOptions'
  ]
  where
    noop _ _ = id
    resetGUIPreservingOptions' _ _ = resetGUIPreservingOptions
    resetGUIForNewTrace _ model =
      setDefaultViewBounds model . resetGUIPreservingOptions

    setNotebookPage ::
      NotebookPage -> GUIElements -> Model -> GUIState -> GUIState
    setNotebookPage nbPage _ _ = set currentPage nbPage

spinButtons :: [SpinButtonCB Double]
spinButtons =
  -- Single page
  [ SpinButtonCB singleOffsetSpinButton   singleOffset
  -- Multiple page
  , SpinButtonCB multipleOffsetSpinButton multipleOffset
  -- Auto page
  , SpinButtonCB noiseThresholdSpinButton      noiseThreshold
  , SpinButtonCB levelShiftThresholdSpinButton levelShiftThreshold
  ]

intSpinButtons :: [SpinButtonCB Int]
intSpinButtons =
  -- Auto page
  [ SpinButtonCB matchLevelSpinButton matchLevel
  ]

checkButtons :: [CheckButtonCB]
checkButtons =
  -- View page
  [ CheckButtonCB showReplicateTracesCheckButton  showReplicateTraces
  ]

radioButtons :: [RadioButtonGroup]
radioButtons =
  [ RadioButtonGroup singleAction (\a -> case a of
      SingleIgnore   -> singleIgnoreRadioButton
      SingleZero     -> singleZeroRadioButton
      SingleSlopeFit -> singleSlopeFitRadioButton)
  , RadioButtonGroup multipleAction (\a -> case a of
      MultipleIgnore -> multipleIgnoreRadioButton
      MultipleLine   -> multipleLineRadioButton
      MultipleCancel -> multipleCancelRadioButton)
  ]

comboBoxTexts :: [ComboBoxTextCB]
comboBoxTexts =
  [ ComboBoxTextCB singleHoldComboBox singleHold $
    \mt -> case mt of
      Just "Left"  -> HoldLeft
      Just "Right" -> HoldRight
      _            -> HoldLeft
  , ComboBoxTextCB referenceTraceComboBoxText referenceTraceLabel $
    \mt -> case mt of
      Just "None" -> Nothing
      m           -> m
  , ComboBoxTextCB screenshotFileFormatComboBoxText screenshotFileFormat $
    \mt -> case mt of
      Just "PNG" -> Chart.PNG
      Just "SVG" -> Chart.SVG
      Just "PS"  -> Chart.PS
      Just "PDF" -> Chart.PDF
      _          -> Chart.PNG
  ]

--------------------------------------------------------------------------------
-- Registering the callbacks
--------------------------------------------------------------------------------

registerCallbacks ::
     GUIElements
  -> TVar Model
  -> TVar GUIState
  -> (IO () -> IO ())
  -> (IO () -> IO ())
  -> IO ()
registerCallbacks guiElems modelTVar guiStateTVar
                  withUpdate withPartialUpdate = do
  forM_ buttons
    $ registerButtonCB guiElems modelTVar guiStateTVar withUpdate
  forM_ intSpinButtons
    $ registerSpinButtonIntCB guiElems guiStateTVar withPartialUpdate
  forM_ spinButtons
    $ registerSpinButtonCB guiElems guiStateTVar withPartialUpdate
  forM_ checkButtons
    $ registerCheckButtonCB guiElems guiStateTVar withPartialUpdate
  forM_ comboBoxTexts
    $ registerComboBoxTextCB guiElems guiStateTVar withPartialUpdate
  forM_ radioButtons
    $ registerRadioButtonGroupCB guiElems guiStateTVar withPartialUpdate

--------------------------------------------------------------------------------
-- Synchronizing the Gtk+ 3 state with the `GUIState`
--------------------------------------------------------------------------------

setGUIParameters :: GUIElements -> GUIState -> IO ()
setGUIParameters guiElems guiState = do
  Gtk.set (notebook guiElems)
    [ notebookPage := pageNumber (guiState ^. currentPage) ]

  forM_ spinButtons    $ setSpinButton       guiElems guiState
  forM_ intSpinButtons $ setIntSpinButton    guiElems guiState
  forM_ checkButtons   $ setCheckButton      guiElems guiState
  forM_ comboBoxTexts  $ setComboBoxText     guiElems guiState
  forM_ radioButtons   $ setRadioButtonGroup guiElems guiState

--------------------------------------------------------------------------------
-- Buttons
--------------------------------------------------------------------------------

data ButtonCB = ButtonCB
  { buttonRef            :: GUIElements -> Button
  , buttonModelAction    :: Model -> Model
  , buttonGUIStateAction :: GUIElements -> Model -> GUIState -> GUIState
  }

registerButtonCB ::
     GUIElements
  -> TVar Model
  -> TVar GUIState
  -> (IO () -> IO ())
  -> ButtonCB
  -> IO ()
registerButtonCB guiElems modelTVar
  guiStateTVar withUpdate callback =
  let button = buttonRef callback guiElems
  in  void $ on button buttonActivated $ withUpdate $ atomically $ do
    model <- readTVar modelTVar
    let newModel = buttonModelAction callback model
    writeTVar modelTVar newModel
    modifyTVar' guiStateTVar (buttonGUIStateAction callback guiElems newModel)

--------------------------------------------------------------------------------
-- Spin buttons
--------------------------------------------------------------------------------

data SpinButtonCB a = SpinButtonCB
  { spinButtonRef    :: GUIElements -> SpinButton
  , spinButtonTarget :: Lens' GUIState a
  }

registerSpinButtonIntCB ::
     GUIElements
  -> TVar GUIState
  -> (IO () -> IO ())
  -> SpinButtonCB Int
  -> IO ()
registerSpinButtonIntCB guiElems guiStateTVar withPartialUpdate callback =
  let spinButton = spinButtonRef callback guiElems
  in  void $ afterValueSpinned spinButton $ withPartialUpdate $ do
        val <- spinButtonGetValueAsInt spinButton
        atomically $ modifyTVar' guiStateTVar
                   $ set (spinButtonTarget callback) val

registerSpinButtonCB ::
     GUIElements
  -> TVar GUIState
  -> (IO () -> IO ())
  -> SpinButtonCB Double
  -> IO ()
registerSpinButtonCB guiElems guiStateTVar withPartialUpdate callback =
  let spinButton = spinButtonRef callback guiElems
  in  void $ afterValueSpinned spinButton $ withPartialUpdate $ do
        val <- spinButtonGetValue spinButton
        atomically $ modifyTVar' guiStateTVar
                   $ set (spinButtonTarget callback) val

setSpinButton :: GUIElements -> GUIState -> SpinButtonCB Double -> IO ()
setSpinButton guiElems guiState spinButtonSpec =
  let spinButton = spinButtonRef    spinButtonSpec
      target     = spinButtonTarget spinButtonSpec
  in  Gtk.set (spinButton guiElems)
        [ spinButtonValue := (guiState ^. target) ]

setIntSpinButton :: GUIElements -> GUIState -> SpinButtonCB Int -> IO ()
setIntSpinButton guiElems guiState spinButtonSpec =
  let spinButton = spinButtonRef    spinButtonSpec
      target     = spinButtonTarget spinButtonSpec
  in  Gtk.set (spinButton guiElems)
        [ spinButtonValue := fromIntegral (guiState ^. target) ]

--------------------------------------------------------------------------------
-- Check buttons
--------------------------------------------------------------------------------

data CheckButtonCB = CheckButtonCB
  { checkButtonRef    :: GUIElements -> CheckButton
  , checkButtonTarget :: Lens' GUIState Bool
  }

registerCheckButtonCB ::
     GUIElements
  -> TVar GUIState
  -> (IO () -> IO ())
  -> CheckButtonCB
  -> IO ()
registerCheckButtonCB guiElems guiStateTVar withPartialUpdate callback =
  let checkButton = (checkButtonRef callback) guiElems
  in  void $ on checkButton buttonActivated $ withPartialUpdate $ do
        active <- toggleButtonGetActive checkButton
        atomically $ modifyTVar' guiStateTVar
                   $ set (checkButtonTarget callback) active

setCheckButton :: GUIElements -> GUIState -> CheckButtonCB -> IO ()
setCheckButton guiElems guiState checkButtonSpec =
  let checkButton = checkButtonRef    checkButtonSpec
      target      = checkButtonTarget checkButtonSpec
  in  Gtk.set (checkButton guiElems)
              [ toggleButtonActive := (guiState ^. target) ]

--------------------------------------------------------------------------------
-- Combo Boxes
--------------------------------------------------------------------------------

data ComboBoxTextCB = forall a. ComboBoxTextCB
  (GUIElements -> ComboBox)    -- comboBoxRef
  (Lens' GUIState (Int, a))    -- comboBoxTarget
  (Maybe T.Text -> a)          -- comboBoxReader

registerComboBoxTextCB ::
     GUIElements
  -> TVar GUIState
  -> (IO () -> IO ())
  -> ComboBoxTextCB
  -> IO ()
registerComboBoxTextCB guiElems guiStateTVar withPartialUpdate
  (ComboBoxTextCB ref target reader) =
  let comboBox = ref guiElems
  in  void $ on comboBox changed $ withPartialUpdate $ do
    index <- comboBoxGetActive comboBox
    listStore <- comboBoxGetModelText comboBox
    txt <- listStoreSafeGetValue listStore index
    atomically $ modifyTVar' guiStateTVar
          $ set target (index, reader txt)

setComboBoxText :: GUIElements -> GUIState -> ComboBoxTextCB -> IO ()
setComboBoxText guiElems guiState (ComboBoxTextCB ref target _) =
  let comboBox = ref guiElems
      index = guiState ^. target . _1
  in  comboBoxSetActive comboBox index

--------------------------------------------------------------------------------
-- Radio buttons
--------------------------------------------------------------------------------

data RadioButtonCB = forall a. RadioButtonCB
  (GUIElements -> RadioButton)    -- radioButtonRef
  (Lens' GUIState a)              -- radioButtonTarget
  a                               -- radioButtonValue

data RadioButtonGroup = forall a. (Bounded a, Enum a) => RadioButtonGroup
  (Lens' GUIState a)                -- radioButtonGroupTarget
  (a -> GUIElements -> RadioButton) -- radioButtonGrouping

registerRadioButtonCB ::
     GUIElements
  -> TVar GUIState
  -> (IO () -> IO ())
  -> RadioButtonCB
  -> IO ()
registerRadioButtonCB guiElems guiStateTVar withPartialUpdate
  (RadioButtonCB ref target value) =
  let radioButton = ref guiElems
  in  void $ on radioButton buttonActivated $ withPartialUpdate $ do
        active <- toggleButtonGetActive radioButton
        when active $ atomically $ modifyTVar' guiStateTVar $ set target value

registerRadioButtonGroupCB ::
     GUIElements
  -> TVar GUIState
  -> (IO () -> IO ())
  -> RadioButtonGroup
  -> IO ()
registerRadioButtonGroupCB guiElems guiStateTVar withPartialUpdate
  (RadioButtonGroup target grouping) =
  let register = registerRadioButtonCB guiElems guiStateTVar withPartialUpdate
  in  forM_ listAll $ \val -> register
         $ RadioButtonCB (grouping val) target val

setRadioButtonGroup :: GUIElements -> GUIState -> RadioButtonGroup -> IO ()
setRadioButtonGroup guiElems guiState (RadioButtonGroup target grouping) =
  let val          = guiState ^. target
      radioButton  = grouping val guiElems
  in  Gtk.set radioButton [ toggleButtonActive := True ]
