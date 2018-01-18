-- Definition of the keyboard shortcuts of the GUI

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes                #-}

module Controller.Keybindings
  ( registerKeyboardShortcuts
  ) where

import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class (liftIO)
import           Data.Text              (unpack)
import           Graphics.UI.Gtk        hiding (set)

import           Controller.GUIElements
import           Controller.GUIState

-------------------------------------------------------------------------------
-- Keyboard shortcuts
-------------------------------------------------------------------------------

-- In order to maintain consistency between the behaviour keyboard shortcuts
-- and GUI elements, keyboard shortcuts should act purely through methods
-- acting on GUI elements, for example, by clicking or toggling buttons.

data GUIAction =
  forall a. WidgetClass a => GUIAction (GUIElements -> a) (a -> IO ())

type Keybinding = ((String, [Modifier]), GUIAction)

generalKeybindings :: [Keybinding]
generalKeybindings =
  [ (("r", [Control]), GUIAction mainFullViewButton  buttonClicked)
  , (("x", [Control]), GUIAction mainFullViewXButton buttonClicked)
  , (("y", [Control]), GUIAction mainFullViewYButton buttonClicked)
  ]

mainPageKeybindings :: [Keybinding]
mainPageKeybindings =
  [ (("o", [Control]),        GUIAction openButton buttonClicked)
  , (("s", [Control]),        GUIAction saveButton     buttonClicked)

  , (("b", [Control]),        GUIAction prevTraceButton buttonClicked)
  , (("f", [Control]),        GUIAction nextTraceButton buttonClicked)
  , (("t", [Control]),        GUIAction twinTraceButton buttonClicked)

  , (("z", [Control]),        GUIAction undoButton buttonClicked)
  , (("Z", [Shift, Control]), GUIAction redoButton buttonClicked)

  , (("a", []), GUIAction autoButton      buttonClicked)
  , (("b", []), GUIAction labellingButton buttonClicked)
  , (("v", []), GUIAction viewButton      buttonClicked)
  , (("c", []), GUIAction cropButton      buttonClicked)
  , (("q", []), GUIAction qualityButton   buttonClicked)
  ]

autoPageKeybindings :: [Keybinding]
autoPageKeybindings =
  [ (("g", []),      GUIAction matchLevelSpinButton $ spinButtonDo SpinHome)
  , (("G", [Shift]), GUIAction matchLevelSpinButton $ spinButtonDo SpinEnd)
  , (("f", []),      GUIAction matchLevelSpinButton $ spinButtonDo SpinStepForward)
  , (("F", [Shift]), GUIAction matchLevelSpinButton $ spinButtonDo SpinPageForward)
  , (("b", []),      GUIAction matchLevelSpinButton $ spinButtonDo SpinStepBackward)
  , (("B", [Shift]), GUIAction matchLevelSpinButton $ spinButtonDo SpinPageBackward)

  , (("Escape", []), GUIAction autoCancelButton buttonClicked)
  , (("space",  []), GUIAction autoApplyButton buttonClicked)
  ]

singlePageKeybindings :: [Keybinding]
singlePageKeybindings =
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
  , (("space",  []), GUIAction singleApplyButton buttonClicked)
  ]

multiplePageKeybindings :: [Keybinding]
multiplePageKeybindings =
  [ (("b", []), GUIAction multipleOffsetSpinButton $ spinButtonDo SpinPageBackward)
  , (("f", []), GUIAction multipleOffsetSpinButton $ spinButtonDo SpinPageForward)
  , (("B", [Shift]), GUIAction multipleOffsetSpinButton $ spinButtonDo SpinStepBackward)
  , (("F", [Shift]), GUIAction multipleOffsetSpinButton $ spinButtonDo SpinStepForward)


  , (("g", []), GUIAction multipleIgnoreRadioButton $ flip toggleButtonSetActive True)
  , (("r", []), GUIAction multipleLineRadioButton   $ flip toggleButtonSetActive True)
  , (("s", []), GUIAction multipleCancelRadioButton $ flip toggleButtonSetActive True)

  , (("Escape", []), GUIAction multipleCancelButton buttonClicked)
  , (("space",  []), GUIAction multipleApplyButton buttonClicked)
  ]

labelPageKeybindings :: [Keybinding]
labelPageKeybindings =
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
  ]

viewPageKeybindings :: [Keybinding]
viewPageKeybindings =
  [ (("r" , []), GUIAction showReplicateTracesCheckButton toggleToggleButton)

  , (("Escape",  []), GUIAction viewBackButton buttonClicked)
  ]

cropPageKeybindings :: [Keybinding]
cropPageKeybindings =
  [ (("c", []),       GUIAction applyCropButton   buttonClicked)
  , (("b", []),       GUIAction applyUncropButton buttonClicked)

  , (("Escape",  []), GUIAction cropBackButton    buttonClicked)
  ]

qualityPageKeybindings :: [Keybinding]
qualityPageKeybindings =
  [ (("g", []),       GUIAction qualityGoodButton     buttonClicked)
  , (("d", []),       GUIAction qualityModerateButton buttonClicked)
  , (("b", []),       GUIAction qualityBadButton      buttonClicked)

  , (("Escape",  []), GUIAction qualityBackButton buttonClicked)
  ]

registerKeyboardShortcuts ::
     GUIElements
  -> TVar GUIState
  -> IO ()
registerKeyboardShortcuts guiElems guiStateMVar = do
  let interpretKeyPress :: EventM EKey Bool
      interpretKeyPress = do
        guiState <- liftIO $ readTVarIO guiStateMVar
        keyCombination <- (,) <$> fmap unpack eventKeyName <*> eventModifier

        let bindings = case guiState ^. currentPage of
              MainPage       -> generalKeybindings ++ mainPageKeybindings
              AutoPage       -> generalKeybindings ++ autoPageKeybindings
              SinglePage   _ -> generalKeybindings ++ singlePageKeybindings
              MultiplePage _ -> generalKeybindings ++ multiplePageKeybindings
              LabelPage      -> generalKeybindings ++ labelPageKeybindings
              ViewPage       -> generalKeybindings ++ viewPageKeybindings
              CropPage     _ -> generalKeybindings ++ cropPageKeybindings
              QualityPage    -> generalKeybindings ++ qualityPageKeybindings
              ScreenshotPage -> generalKeybindings

        case lookup keyCombination bindings of
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
