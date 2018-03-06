-- Keyboard shortcuts for the GUI

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE StandaloneDeriving        #-}

module Controller.Keybindings
  ( registerKeyboardShortcuts
  ) where

import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Strict        as M
import qualified Data.Text              as T
import           Graphics.UI.Gtk        hiding (set)

import           Controller.AppState
import           Controller.GUIElements
import           Controller.GUIState

-------------------------------------------------------------------------------

-- In order to maintain consistency between the behaviour keyboard shortcuts
-- and GUI elements, keyboard shortcuts should act purely through methods
-- acting on GUI elements, for example, by clicking or toggling buttons.

data GUIAction =
  forall a. WidgetClass a => GUIAction (GUIElements -> a) (a -> IO ())

type KeyPress = (T.Text, [Modifier])

deriving instance Ord Modifier

generalKeybindings :: M.Map KeyPress GUIAction
generalKeybindings = M.fromList
  [ (("r", [Control]), GUIAction mainFullViewButton  buttonClicked)
  , (("x", [Control]), GUIAction mainFullViewXButton buttonClicked)
  , (("y", [Control]), GUIAction mainFullViewYButton buttonClicked)
  ]

mainPageKeybindings :: M.Map KeyPress GUIAction
mainPageKeybindings = generalKeybindings `M.union` M.fromList
  [ (("o", [Control]),        GUIAction openButton buttonClicked)
  , (("s", [Control]),        GUIAction saveButton     buttonClicked)

  , (("b", [Control]),        GUIAction prevTraceButton buttonClicked)
  , (("f", [Control]),        GUIAction nextTraceButton buttonClicked)
  , (("t", [Control]),        GUIAction twinTraceButton buttonClicked)

  , (("z", [Control]),        GUIAction undoButton buttonClicked)
  , (("Z", [Shift, Control]), GUIAction redoButton buttonClicked)

  , (("a", []), GUIAction autoButton       buttonClicked)
  , (("b", []), GUIAction labellingButton  buttonClicked)
  , (("v", []), GUIAction viewButton       buttonClicked)
  , (("c", []), GUIAction cropButton       buttonClicked)
  , (("q", []), GUIAction qualityButton    buttonClicked)
  , (("s", []), GUIAction screenshotButton buttonClicked)
  ]

autoPageKeybindings :: M.Map KeyPress GUIAction
autoPageKeybindings = generalKeybindings `M.union` M.fromList
  [ (("g", []),      GUIAction matchLevelSpinButton $ spinButtonDo SpinHome)
  , (("G", [Shift]), GUIAction matchLevelSpinButton $ spinButtonDo SpinEnd)
  , (("f", []),      GUIAction matchLevelSpinButton $ spinButtonDo SpinStepForward)
  , (("F", [Shift]), GUIAction matchLevelSpinButton $ spinButtonDo SpinPageForward)
  , (("b", []),      GUIAction matchLevelSpinButton $ spinButtonDo SpinStepBackward)
  , (("B", [Shift]), GUIAction matchLevelSpinButton $ spinButtonDo SpinPageBackward)

  , (("Escape", []), GUIAction autoCancelButton buttonClicked)
  , (("Tab",    []), GUIAction autoCancelButton buttonClicked)
  , (("space",  []), GUIAction autoApplyButton buttonClicked)
  ]

singlePageKeybindings :: M.Map KeyPress GUIAction
singlePageKeybindings = generalKeybindings `M.union` M.fromList
  [ (("q", []), GUIAction singleHoldComboBox $ flip comboBoxSetActive 0)
  , (("w", []), GUIAction singleHoldComboBox $ flip comboBoxSetActive 1)

  , (("b", []), GUIAction singleOffsetSpinButton $ spinButtonDo SpinPageBackward)
  , (("f", []), GUIAction singleOffsetSpinButton $ spinButtonDo SpinPageForward)
  , (("B", [Shift]), GUIAction singleOffsetSpinButton $ spinButtonDo SpinStepBackward)
  , (("F", [Shift]), GUIAction singleOffsetSpinButton $ spinButtonDo SpinStepForward)

  , (("g", []), GUIAction singleIgnoreRadioButton        $ flip toggleButtonSetActive True)
  , (("z", []), GUIAction singleZeroRadioButton          $ flip toggleButtonSetActive True)
  , (("s", []), GUIAction singleSlopeFitRadioButton      $ flip toggleButtonSetActive True)

  , (("Escape", []), GUIAction singleCancelButton buttonClicked)
  , (("Tab",    []), GUIAction singleCancelButton buttonClicked)
  , (("space",  []), GUIAction singleApplyButton buttonClicked)
  ]

multiplePageKeybindings :: M.Map KeyPress GUIAction
multiplePageKeybindings = generalKeybindings `M.union` M.fromList
  [ (("b", []), GUIAction multipleOffsetSpinButton $ spinButtonDo SpinPageBackward)
  , (("f", []), GUIAction multipleOffsetSpinButton $ spinButtonDo SpinPageForward)
  , (("B", [Shift]), GUIAction multipleOffsetSpinButton $ spinButtonDo SpinStepBackward)
  , (("F", [Shift]), GUIAction multipleOffsetSpinButton $ spinButtonDo SpinStepForward)


  , (("g", []), GUIAction multipleIgnoreRadioButton $ flip toggleButtonSetActive True)
  , (("r", []), GUIAction multipleLineRadioButton   $ flip toggleButtonSetActive True)
  , (("s", []), GUIAction multipleCancelRadioButton $ flip toggleButtonSetActive True)

  , (("Escape", []), GUIAction multipleCancelButton buttonClicked)
  , (("Tab",    []), GUIAction multipleCancelButton buttonClicked)
  , (("space",  []), GUIAction multipleApplyButton buttonClicked)
  ]

labelPageKeybindings :: M.Map KeyPress GUIAction
labelPageKeybindings = generalKeybindings `M.union` M.fromList
  [ (("Down" , [Shift]), GUIAction levelShiftThresholdSpinButton $ spinButtonDo SpinStepBackward)
  , (("Up"   , [Shift]), GUIAction levelShiftThresholdSpinButton $ spinButtonDo SpinStepForward)
  , (("J", [Shift]), GUIAction levelShiftThresholdSpinButton $ spinButtonDo SpinStepBackward)
  , (("K", [Shift]), GUIAction levelShiftThresholdSpinButton $ spinButtonDo SpinStepForward)

  , (("Down", []), GUIAction noiseThresholdSpinButton $ spinButtonDo SpinStepBackward)
  , (("Up",   []), GUIAction noiseThresholdSpinButton $ spinButtonDo SpinStepForward)
  , (("j",    []), GUIAction noiseThresholdSpinButton $ spinButtonDo SpinStepBackward)
  , (("k",    []), GUIAction noiseThresholdSpinButton $ spinButtonDo SpinStepForward)

  , (("r", []), GUIAction defaultParametersButton buttonClicked)

  , (("Escape",  []), GUIAction labelBackButton buttonClicked)
  , (("Tab",     []), GUIAction labelBackButton buttonClicked)
  ]

viewPageKeybindings :: M.Map KeyPress GUIAction
viewPageKeybindings = generalKeybindings `M.union` M.fromList
  [ (("r" , []), GUIAction showReplicateTracesCheckButton toggleToggleButton)

  , (("Escape",  []), GUIAction viewBackButton buttonClicked)
  , (("Tab",     []), GUIAction viewBackButton buttonClicked)
  ]

cropPageKeybindings :: M.Map KeyPress GUIAction
cropPageKeybindings = generalKeybindings `M.union` M.fromList
  [ (("c", []),       GUIAction applyCropButton   buttonClicked)
  , (("b", []),       GUIAction applyUncropButton buttonClicked)

  , (("Escape",  []), GUIAction cropBackButton    buttonClicked)
  , (("Tab",     []), GUIAction cropBackButton    buttonClicked)
  ]

qualityPageKeybindings :: M.Map KeyPress GUIAction
qualityPageKeybindings = generalKeybindings `M.union` M.fromList
  [ (("g", []),       GUIAction qualityGoodButton     buttonClicked)
  , (("d", []),       GUIAction qualityModerateButton buttonClicked)
  , (("b", []),       GUIAction qualityBadButton      buttonClicked)

  , (("Escape",  []), GUIAction qualityBackButton buttonClicked)
  , (("Tab",     []), GUIAction qualityBackButton buttonClicked)
  ]

screenshotPageKeybindings :: M.Map KeyPress GUIAction
screenshotPageKeybindings = generalKeybindings `M.union` M.fromList
  [ (("s", []),       GUIAction screenshotSaveButton buttonClicked)

  , (("Escape",  []), GUIAction screenshotBackButton buttonClicked)
  , (("Tab",     []), GUIAction screenshotBackButton buttonClicked)
  ]

registerKeyboardShortcuts :: GUIElements -> AppHandle -> IO ()
registerKeyboardShortcuts guiElems appH = do
  let interpretKeyPress :: EventM EKey Bool
      interpretKeyPress = do
        guiState <- liftIO $ atomically $ getAppGUIState appH
        keyCombination <- (,) <$> eventKeyName <*> eventModifier

        let bindings = case guiState ^. currentPage of
              MainPage       -> mainPageKeybindings
              AutoPage       -> autoPageKeybindings
              SinglePage   _ -> singlePageKeybindings
              MultiplePage _ -> multiplePageKeybindings
              LabelPage      -> labelPageKeybindings
              ViewPage       -> viewPageKeybindings
              CropPage     _ -> cropPageKeybindings
              QualityPage    -> qualityPageKeybindings
              ScreenshotPage -> screenshotPageKeybindings

        case M.lookup keyCombination bindings of
          Just (GUIAction ref action) ->
            let guiElement = ref guiElems
            in  liftIO $ do
              isSensitive <- widgetIsSensitive guiElement
              when isSensitive $ action guiElement
              return True
          Nothing     -> return False

  _ <- (controllerWindow guiElems) `on` keyPressEvent $ interpretKeyPress

  return ()

-------------------------------------------------------------------------------
-- Utility
-------------------------------------------------------------------------------

spinButtonDo :: SpinButtonClass s => SpinType -> s -> IO ()
spinButtonDo spinType sb = do
  val <- spinButtonGetValue sb
  adj <- spinButtonGetAdjustment sb
  newVal <- case spinType of
    SpinStepForward  -> (val+) <$> adjustmentGetStepIncrement adj
    SpinStepBackward -> (val-) <$> adjustmentGetStepIncrement adj
    SpinPageForward  -> (val+) <$> adjustmentGetPageIncrement adj
    SpinPageBackward -> (val-) <$> adjustmentGetPageIncrement adj
    SpinHome         ->            adjustmentGetLower adj
    SpinEnd          ->            adjustmentGetUpper adj
    SpinUserDefined  -> return val
  spinButtonSetValue sb newVal

toggleToggleButton :: ToggleButtonClass s => s -> IO ()
toggleToggleButton toggleButton = do
  active <- toggleButtonGetActive toggleButton
  toggleButtonSetActive toggleButton (not active)
