-- TODO: summary

module Controller.GenericCallbacks
  ( registerCallbacks
  ) where

import           Control.Concurrent.STM
import           Control.Lens           hiding (index)
import           Control.Monad
import           Data.Default
import           Graphics.UI.Gtk        hiding (set)

import           Model

import           Controller.GUIElements
import           Controller.GUIState

--------------------------------------------------------------------------------

type PureAction = Model -> GUIState -> (Model, GUIState)

data PureCallback = PureCallback (GUIElements -> Button) PureAction

m_ :: (Model -> Model) -> PureAction
m_ f = \model guiState -> (f model, guiState)

_g :: (GUIState -> GUIState) -> PureAction
_g f = \model guiState -> (model, f guiState)

_mg :: (Model -> GUIState -> GUIState) -> PureAction
_mg f = \model guiState -> (model, f model guiState)

m_g :: (Model -> Model) -> (GUIState -> GUIState) -> PureAction
m_g f g = \model guiState -> (f model, g guiState)

-- Use new model in the GUIState function
m_mg :: (Model -> Model) -> (Model -> GUIState -> GUIState) -> PureAction
m_mg f g = \model guiState ->
  let model' = f model in (model', g model' guiState)

--------------------------------------------------------------------------------

pureCallbacks :: [PureCallback]
pureCallbacks =
  -- Main page
  [ PureCallback prevTraceButton $ m_mg
      gotoPrevTrace
      (\model -> setDefaultViewBounds model . resetGUIPreservingOptions)
  , PureCallback nextTraceButton $ m_mg
      gotoNextTrace
      (\model -> setDefaultViewBounds model . resetGUIPreservingOptions)
  , PureCallback twinTraceButton
      $ m_g gotoTwinTrace resetGUIPreservingOptions

  , PureCallback undoButton $ m_ undo
  , PureCallback redoButton $ m_ redo

  , PureCallback labellingButton  $ _g (set currentPage LabelPage)
  , PureCallback viewButton       $ _g (set currentPage ViewPage)
  , PureCallback cropButton       $ _g (set currentPage (CropPage Nothing))
  , PureCallback qualityButton    $ _g (set currentPage QualityPage)
  , PureCallback screenshotButton $ _g (set currentPage ScreenshotPage)

  , PureCallback mainFullViewButton  $ _mg setDefaultViewBounds
  , PureCallback mainFullViewXButton $ _mg setDefaultViewBoundsX
  , PureCallback mainFullViewYButton $ _mg setDefaultViewBoundsY

  , PureCallback autoCancelButton     $ _g resetGUIPreservingOptions
  , PureCallback singleCancelButton   $ _g resetGUIPreservingOptions
  , PureCallback multipleCancelButton $ _g resetGUIPreservingOptions

  -- Label page
  , PureCallback defaultParametersButton
      $ _g $ set levelShiftThreshold (view levelShiftThreshold def)
            . set noiseThreshold      (view noiseThreshold def)
  , PureCallback labelBackButton $ _g resetGUIPreservingOptions

  -- View page
  , PureCallback viewBackButton $ _g resetGUIPreservingOptions

  -- Crop page
  , PureCallback applyCropButton $ \model guiState ->
      case guiState ^. currentPage of
        (CropPage (Just indexInterval)) ->
          (crop indexInterval model, resetGUIPreservingOptions guiState)
        _ -> (model, guiState)
  , PureCallback applyUncropButton $ m_g uncrop resetGUIPreservingOptions

  , PureCallback cropBackButton $ _g resetGUIPreservingOptions

  -- Quality page
  , PureCallback qualityGoodButton
      $ m_g (setQuality Good) resetGUIPreservingOptions
  , PureCallback qualityModerateButton
      $ m_g (setQuality Moderate) resetGUIPreservingOptions
  , PureCallback qualityBadButton
      $ m_g (setQuality Bad) resetGUIPreservingOptions

  , PureCallback qualityBackButton $ _g resetGUIPreservingOptions

  -- Screenshot page
  , PureCallback screenshotBackButton $ _g resetGUIPreservingOptions
  ]

--------------------------------------------------------------------------------

registerPureCallback
  :: GUIElements
  -> TVar Model
  -> TVar GUIState
  -> (IO () -> IO ())
  -> PureCallback
  -> IO ()
registerPureCallback guiElems modelTVar guiStateTVar withUpdate
  (PureCallback buttonRef action) =
  let button = buttonRef guiElems
  in  void $ on button buttonActivated $ withUpdate $ atomically $ do
    (model, guiState) <- (,) <$> readTVar modelTVar <*> readTVar guiStateTVar
    let (model', guiState') = action model guiState
    writeTVar modelTVar model'
    writeTVar guiStateTVar guiState'

registerCallbacks ::
     GUIElements
  -> TVar Model
  -> TVar GUIState
  -> (IO () -> IO ())
  -> IO ()
registerCallbacks guiElems modelTVar guiStateTVar withUpdate =
  forM_ pureCallbacks
    $ registerPureCallback guiElems modelTVar guiStateTVar withUpdate

