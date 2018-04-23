-- TODO: summary

module Controller.GenericCallbacks
  ( registerPureCallbacks
  ) where

import           Control.Lens           hiding (index)
import           Control.Monad
import           Data.Default
import           Graphics.UI.Gtk        hiding (set)

import           Model

import           Controller.AppState
import           Controller.GUIElements
import           Controller.GUIState

--------------------------------------------------------------------------------

type PureAction a = Model a -> GUIState -> (Model a, GUIState)

data PureCallback a = PureCallback (GUIElements -> Button) (PureAction a)

m_ :: (Model a -> Model a) -> PureAction a
m_ f = \model guiState -> (f model, guiState)

_g :: (GUIState -> GUIState) -> PureAction a
_g f = \model guiState -> (model, f guiState)

_mg :: (Model a -> GUIState -> GUIState) -> PureAction a
_mg f = \model guiState -> (model, f model guiState)

m_g :: (Model a -> Model a) -> (GUIState -> GUIState) -> PureAction a
m_g f g = \model guiState -> (f model, g guiState)

-- Use new model in the GUIState function
m_mg
  :: (Model a -> Model a) -> (Model a -> GUIState -> GUIState) -> PureAction a
m_mg f g = \model guiState ->
  let model' = f model in (model', g model' guiState)

--------------------------------------------------------------------------------

pureCallbacks :: [PureCallback a]
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

registerPureCallback :: GUIElements -> AppHandle a -> PureCallback a -> IO ()
registerPureCallback guiElems appH (PureCallback buttonRef action) =
  let button = buttonRef guiElems
  in  void $ on button buttonActivated $
        modifyAppModelGUIState appH $ uncurry action

registerPureCallbacks :: GUIElements -> AppHandle a -> IO ()
registerPureCallbacks guiElems appH =
  forM_ pureCallbacks $ registerPureCallback guiElems appH

