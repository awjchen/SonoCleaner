-- Definition of the information extracted from the controller window GUI
--
-- This excludes the mouse inputs on the display window.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Controller.GUIState where

import           Control.Arrow                          ((>>>))
import           Control.Lens
import           Data.Default
import qualified Data.Text                              as T
import qualified Graphics.Rendering.Chart.Backend.Cairo as Chart

import           Model
import           Types.Bounds
import           Types.Indices
import           Types.LevelShifts

--------------------------------------------------------------------------------
-- GUI state
--------------------------------------------------------------------------------

data NotebookPage
  = MainPage
  | AutoPage
  | SinglePage Index1
  | MultiplePage [Index1]
  | LabelPage
  | ViewPage
  | CropPage (Maybe (IndexInterval Index0))
  | QualityPage
  | ScreenshotPage
  deriving (Eq)

-- We must ensure that the page numbers defined here match those defined the
-- Glade file.
pageNumber :: NotebookPage -> Int
pageNumber page = case page of
  MainPage       -> 0
  AutoPage       -> 1
  SinglePage _   -> 2
  MultiplePage _ -> 3
  LabelPage      -> 4
  ViewPage       -> 5
  CropPage _     -> 6
  QualityPage    -> 7
  ScreenshotPage -> 8

data GUIState = GUIState
  { _currentPage          :: NotebookPage
  , _viewBounds           :: ViewBounds

  -- Auto options
  , _matchLevel           :: Int
  , _levelShiftThreshold  :: Double
  , _noiseThreshold       :: Double
  , _levelShiftMatches    :: LevelShiftMatches

  , _showAdvancedOptions  :: Bool

  -- Single options
  , _singleHold           :: (Int, Hold)
  , _singleOffset         :: Double
  , _singleAction         :: SingleAction

  -- Multiple options
  , _multipleOffset       :: Double
  , _multipleAction       :: MultipleAction

  -- View options
  , _showReplicateTraces  :: Bool
  , _referenceTraceLabel  :: (Int, Maybe T.Text)

  -- Screenshot options
  , _screenshotFileFormat :: (Int, Chart.FileFormat)
  }
makeLenses ''GUIState

instance Default GUIState where
  def = GUIState
    { _currentPage          = MainPage
    , _viewBounds           = ViewBounds (0, 1) (0, 1)

    -- Auto options
    , _matchLevel           = 0
    , _levelShiftThreshold  = 0.72
    , _noiseThreshold       = 0.12
    , _levelShiftMatches    = def

    , _showAdvancedOptions  = False

    -- Single options
    , _singleHold           = (0, HoldLeft)
    , _singleOffset         = 0.0
    , _singleAction         = SingleIgnore

    -- Multiple options
    , _multipleOffset       = 0.0
    , _multipleAction       = MultipleIgnore

    -- View Options
    , _showReplicateTraces  = False
    , _referenceTraceLabel  = (0, Nothing)

    -- Screenshot options
    , _screenshotFileFormat = (0, Chart.PNG)
    }

resetGUIPreservingOptions :: GUIState -> GUIState
resetGUIPreservingOptions guiState =
  def & viewBounds           .~ (guiState ^. viewBounds)
      & levelShiftThreshold  .~ (guiState ^. levelShiftThreshold)
      & noiseThreshold       .~ (guiState ^. noiseThreshold)
      -- & singleHold           .~ (guiState ^. singleHold)
      & showReplicateTraces  .~ (guiState ^. showReplicateTraces)
      & referenceTraceLabel  .~ (guiState ^. referenceTraceLabel)
      & screenshotFileFormat .~ (guiState ^. screenshotFileFormat)

setDefaultViewBoundsX :: Model -> GUIState -> GUIState
setDefaultViewBoundsX model =
  let defaultBounds = getTraceBounds model in
  set (viewBounds . viewBoundsX) (defaultBounds ^. viewBoundsX)

setDefaultViewBoundsY :: Model -> GUIState -> GUIState
setDefaultViewBoundsY model =
  let defaultBounds = getTraceBounds model in
      set (viewBounds . viewBoundsY) (defaultBounds ^. viewBoundsY)
  >>> over (viewBounds . viewBoundsY) (bimap pred succ)

setDefaultViewBounds :: Model -> GUIState -> GUIState
setDefaultViewBounds = (.) <$> setDefaultViewBoundsX
                           <*> setDefaultViewBoundsY
