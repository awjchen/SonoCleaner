-- TODO: summary

module Controller.GenericCallbacks
  ( registerCallbacks
  ) where

import           Control.Concurrent.STM
import           Control.Lens                           hiding (index)
import           Control.Monad
import           Data.Default
import           Graphics.UI.Gtk                        hiding (set)

import           Controller.GUIElements
import           Controller.GUIState
import           Model

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

--------------------------------------------------------------------------------
-- Registering the callbacks
--------------------------------------------------------------------------------

registerCallbacks ::
     GUIElements
  -> TVar Model
  -> TVar GUIState
  -> (IO () -> IO ())
  -> IO ()
registerCallbacks guiElems modelTVar guiStateTVar withUpdate =
  forM_ buttons
    $ registerButtonCB guiElems modelTVar guiStateTVar withUpdate

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
