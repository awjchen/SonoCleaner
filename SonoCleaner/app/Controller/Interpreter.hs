-- This module contains the logic that acts on the sonomicrometry trace data. It
-- gathers and combines trace data, operations on that data, and parameters for
-- those operations in order to compute and display (generating a ChartSpec) the
-- results of those operations. Intermediate results are cached in an attempt to
-- avoid recomputation.

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Controller.Interpreter
  ( setupInterpreter
  ) where

import           Control.Concurrent.STM
import           Control.Lens           hiding (index, indices, transform)
import           Control.Monad          (when)
import           Data.Colour            (blend, opaque)
import           Data.Colour.Names
import           Data.Default
import qualified Data.IntMap.Strict     as M
import qualified Data.IntSet            as S
import qualified Data.Text              as T
import           Data.Text.Lens
import qualified Data.Vector.Unboxed    as V
import           System.FilePath        (splitFileName)
import           Text.Printf

import           Controller.GUIState
import           Model
import           Types.Bounds
import           Types.LevelShifts
import           View.Types

--------------------------------------------------------------------------------
-- Data flow
--------------------------------------------------------------------------------

-- A. Model (raw data)
--      |
--      | 1. readIdAnnotation (using DataParams)
--      v
-- B. AnnotatedTraceState IdDataDependencies (raw data, with precomputations)
--      |
--      | 2. applyOperation (using TraceOperation)
--      v
-- C. AnnotatedTraceState OpDataDependencies (transformed data, with precomputations)
--      |
--      | 3. makeAnnotatedChartSpec (using ViewParams)
--      v
-- D. AnnotatedChartSpec (specification of the display)

--------------------------------------------------------------------------------
-- The parameters which determine the transformation and display of a trace
--------------------------------------------------------------------------------

-- 1
data DataParams = DataParams
  { _dpLevelShiftThreshold :: Double
  , _dpNoiseThreshold      :: Double
  } deriving (Eq)

-- 2
type LevelShiftIndex = Int
type MatchLevel = Int
type Offset = Double

data TraceOperation =
    IdentityOp
  | AutoOp MatchLevel
  | ManualSingleOp   SingleAction    LevelShiftIndex  Offset (Int, Hold)
  | ManualMultipleOp MultipleAction [LevelShiftIndex] Offset
  deriving (Eq)

-- 3
data ViewParams = ViewParams
  { _vpViewBounds          :: ViewBounds
  , _vpShowReplicateTraces :: Bool
  , _vpReferenceTraceLabel :: (Int, Maybe T.Text)
  , _vpCurrentPage         :: NotebookPage
  , _vpCropBounds          :: Maybe (Int, Int)
  } deriving (Eq)
makeLenses ''ViewParams

--------------------------------------------------------------------------------
-- Reading the parameters from the GUIState
--------------------------------------------------------------------------------

-- 1
readDataParams :: GUIState -> DataParams
readDataParams GUIState{..} = DataParams
  { _dpLevelShiftThreshold = _levelShiftThreshold
  , _dpNoiseThreshold      = _noiseThreshold
  }

-- 2
readTraceOperation :: GUIState -> TraceOperation
readTraceOperation GUIState{..} = case _currentPage of
  AutoPage     -> AutoOp _matchLevel
  SinglePage   -> case _levelShiftSelection of
    [levelShiftIndex] ->
      ManualSingleOp _singleAction
                     levelShiftIndex
                     _singleOffset
                     _singleHold
    _ -> IdentityOp
  MultiplePage -> case _levelShiftSelection of
    (_:_:_) -> ManualMultipleOp _multipleAction
                                _levelShiftSelection
                                _multipleOffset
    _ -> IdentityOp
  _            -> IdentityOp

-- 3
readViewParams :: GUIState -> ViewParams
readViewParams GUIState{..} = ViewParams
  { _vpViewBounds          = _viewBounds
  , _vpShowReplicateTraces = _showReplicateTraces
  , _vpReferenceTraceLabel = _referenceTraceLabel
  , _vpCurrentPage         = _currentPage
  , _vpCropBounds          = cropBounds }
  where
    cropBounds = case _currentPage of
      CropPage -> case _levelShiftSelection of
        [lb, ub] -> Just (lb, ub)
        _        -> Nothing
      _ -> Nothing

--------------------------------------------------------------------------------
-- Parameter dependencies for validating the intermediate cached computations
--------------------------------------------------------------------------------

-- B
data IdDataDependencies = IdDataDependencies
  { _iddDataVersion :: Integer
  , _iddDataParams  :: DataParams
  } deriving (Eq)

-- C
data OpDataDependencies = OpDataDependencies
  { _nddDataVersion :: Integer
  , _nddDataParams  :: DataParams
  , _nddOperation   :: TraceOperation
  } deriving (Eq)

-- D
data DisplayDependencies = DisplayDependencies
  { _ddDataVersion :: Integer
  , _ddDataParams  :: DataParams
  , _ddOperation   :: TraceOperation
  , _ddViewParams  :: ViewParams
  } deriving (Eq)

makeLenses ''OpDataDependencies

--------------------------------------------------------------------------------
-- The 'objects/vertices' of the data flow
-- (Cached data annotated with precomputations and parameter dependencies)
--------------------------------------------------------------------------------

-- B, C
data AnnotatedTraceState a = AnnotatedTraceState
  { _atsDependencies      :: a
  , _atsTraceState        :: TraceState
  , _atsJumps             :: M.IntMap Double
  , _atsLevelShiftMatches :: LevelShiftMatches }

-- D
data AnnotatedChartSpec = AnnotatedChartSpec
  { _acsDependencies :: DisplayDependencies
  , _acsChartSpec    :: ChartSpec  }

makeLenses ''AnnotatedTraceState
makeLenses ''AnnotatedChartSpec

--------------------------------------------------------------------------------
-- The 'morphisms/edges' of the data flow
-- (Functions between the 'objects/vertices')
--------------------------------------------------------------------------------

-- 1
readIdAnnotation
  :: DataParams
  -> Model
  -> AnnotatedTraceState IdDataDependencies
readIdAnnotation dataParams model =
  let nt  = _dpNoiseThreshold      dataParams
      lst = _dpLevelShiftThreshold dataParams

      dataVersion = getTraceDataVersion model
      idTraceState = getCurrentState model
      idJumps = labelTraceStateJumps nt lst idTraceState
      idMatches = matchJumps nt idJumps idTraceState

  in  AnnotatedTraceState
        { _atsDependencies      = IdDataDependencies
                                    { _iddDataVersion = dataVersion
                                    , _iddDataParams  = dataParams }
        , _atsTraceState        = idTraceState
        , _atsJumps             = idJumps
        , _atsLevelShiftMatches = idMatches }


-- 2
applyOperation
  :: TraceOperation
  -> AnnotatedTraceState IdDataDependencies
  -> AnnotatedTraceState OpDataDependencies
applyOperation traceOp ats =
  let dataParams = _iddDataParams (_atsDependencies ats)
      transform = getTraceStateTransform traceOp ats
      newDependencies = OpDataDependencies
        { _nddDataVersion = _iddDataVersion (_atsDependencies ats)
        , _nddDataParams  = dataParams
        , _nddOperation   = traceOp }
  in  case transform of
    IdOperator -> ats & set atsDependencies newDependencies
    TraceStateOperator op ->
      let newTraceState = op (_atsTraceState ats)
          newJumps = labelTraceStateJumps
                       (_dpNoiseThreshold dataParams)
                       (_dpLevelShiftThreshold dataParams)
                       newTraceState
          newMatches = matchJumps
                     (_dpNoiseThreshold dataParams) newJumps newTraceState
      in  AnnotatedTraceState
            { _atsDependencies      = newDependencies
            , _atsTraceState        = newTraceState
            , _atsJumps             = newJumps
            , _atsLevelShiftMatches = newMatches }

-- helper for 2
getTraceStateTransform
  :: TraceOperation
  -> AnnotatedTraceState IdDataDependencies
  -> TraceStateOperator
getTraceStateTransform traceOp ats = case traceOp of
  IdentityOp -> idOperator
  AutoOp matchLevel' ->
    matchJumpsTrace (_atsLevelShiftMatches ats) matchLevel'
  ManualSingleOp action index offset holdPair -> case action of
    SingleIgnore        -> idOperator
    SingleZero          -> apply zeroJump
    SingleSlopeFit      -> apply estimateSlopeBoth
    where apply f = f (snd holdPair) offset index (_atsJumps ats)
  ManualMultipleOp action indices offset -> case action of
    MultipleIgnore -> idOperator
    MultipleLine   -> apply interpolateBetweenJumps
    MultipleCancel -> apply matchGroup
    where apply f = f offset indices (_atsJumps ats)

-- 3
makeAnnotatedChartSpec
  :: Model
  -> ViewParams
  -> AnnotatedTraceState OpDataDependencies
  -> AnnotatedChartSpec
makeAnnotatedChartSpec model viewParams ats =
  let atsDeps = _atsDependencies ats
      displayDependencies = DisplayDependencies
        { _ddDataVersion = _nddDataVersion (_atsDependencies ats)
        , _ddDataParams  = _nddDataParams atsDeps
        , _ddOperation   = _nddOperation  atsDeps
        , _ddViewParams  = viewParams }

      chartSpec = specifyChart model viewParams ats

  in  AnnotatedChartSpec
        { _acsDependencies = displayDependencies
        , _acsChartSpec    = chartSpec }

-- helper for 3
specifyChart
  :: Model
  -> ViewParams
  -> AnnotatedTraceState OpDataDependencies
  -> ChartSpec
specifyChart model viewParams ats =
  ChartSpec
    { plotTitle            = title
    , plotTitleColour      = titleColour
    , plotSeries           = newSeries
    , plotJumpIndices      = newJumpIndices
    , plotModifiedIndices  = newModifiedIndices
    , plotOriginalSeries   = originalSeries
    , plotTwinSeries       = twinSeries
    , plotCustomSeries     = customSeries
    , plotHighlightRegion  = highlightRegion
    , plotXRange           = viewParams ^. vpViewBounds . viewBoundsX
    , plotYRange           = viewParams ^. vpViewBounds . viewBoundsY
    , plotBackgroundColour = bgColour
    , plotAnnotation       = annotation
    , plotTimeStep         = getTimeStep model
    , plotToTime           = toTime
    , plotToIndex          = toIndex
    }
  where
    (toTime, toIndex)  = getIndexTimeConversions model

    traceSet = if viewParams ^. vpShowReplicateTraces
      then TraceSet { showOriginal = True
                    , showTwin     = True }
      else def

    ----------------------------------------------------------------------------
    -- ChartSpec fields

    title = prefix ++ fileName ++ " (" ++ label ++ ")" where
      label = getLabel model
      fileName = snd $ splitFileName $ getFilePath model
      prefix = case viewParams ^. vpCurrentPage of
        MainPage     -> "Main view -- "
        AutoPage     -> "Previewing automatic correction -- "
        SinglePage   -> "Previewing manual correction (single) -- "
        MultiplePage -> "Previewing manual correction (group) -- "
        LabelPage    -> "Adjusting labelling settings -- "
        ViewPage     -> "Selecting comparison traces -- "
        CropPage     -> "Cropping -- "
        QualityPage  -> "Setting trace quality -- "

    titleColour = case viewParams ^. vpCurrentPage of
      MainPage     -> opaque black
      AutoPage     -> opaque greenyellow
      SinglePage   -> opaque yellow
      MultiplePage -> opaque magenta
      LabelPage    -> opaque plum
      ViewPage     -> opaque white
      CropPage     -> opaque orange
      QualityPage  -> opaque cyan

    bgColour = case getQuality model of
      Good     -> opaque grey
      Moderate -> opaque (blend 0.85 grey blue)
      Bad      -> opaque (blend 0.85 grey red)

    newSeries = ats ^. atsTraceState . series

    newJumpIndices = ats ^. atsJumps

    newModifiedIndices = ats ^. atsTraceState . modifiedJumps

    originalSeries =
      if not $ showOriginal traceSet
      then Nothing
      else Just $ getInputState model ^. series

    twinSeries =
      if not $ showTwin traceSet
      then Nothing
      else fmap (view series) (getTwinTrace model)

    customSeries =
      let mTraceState = viewParams ^? vpReferenceTraceLabel . _2 . _Just . unpacked
                    >>= findTraceByLabel model
      in  case mTraceState of
            Nothing -> Nothing
            Just ts ->
              let vp = viewParams ^. vpViewBounds . toViewPort
                  c2 = vp ^. viewPortCenter . _2
                  r2 = vp ^. viewPortRadii  . _2
                  (y0, y1) = ts ^. seriesBounds
                  c1 = (y1+y0)/2
                  r1 = (y1-y0)/2
              in  Just $ V.map (\y -> (y-c1)*r2/r1 + c2) $ ts ^. series

    highlightRegion = case ats ^. atsDependencies . nddOperation of
      IdentityOp -> fmap (over both toTime) (viewParams ^. vpCropBounds)
      AutoOp _ -> Nothing
      ManualSingleOp _ i _ _ ->
        Just $ over both toTime (i, succ i)
      ManualMultipleOp _ is _ ->
        Just $ over both toTime (head is, succ (last is))

    annotation = case ats ^. atsDependencies . nddOperation of
      ManualSingleOp _ i _ _ ->
        let idSeries = getCurrentState model ^. series
            x2 = toTime (succ i)
            y1 = idSeries V.! i
            y2 = idSeries V.! succ i
        in  Just (x2, (y1+y2)/2, printf "%.2f" (y2-y1))
      _ -> Nothing

--------------------------------------------------------------------------------
-- Initialization of the interpreter
--------------------------------------------------------------------------------

setupInterpreter :: IO ( Model -> GUIState -> STM ()
                       , Model -> GUIState -> STM ChartSpec
                       , Model -> GUIState -> STM LevelShiftMatches
                       , Model -> GUIState -> STM (M.IntMap Double)
                       , Model -> GUIState -> STM Model )
setupInterpreter = do
  idAnnotationTVar       <- newTVarIO undefined
  opAnnotationTVar       <- newTVarIO undefined
  annotatedChartSpecTVar <- newTVarIO undefined

  let initializeInterpreter :: Model -> GUIState -> STM ()
      initializeInterpreter model guiState = do
        let dataParams = readDataParams guiState
            traceOp    = readTraceOperation guiState
            viewParams = readViewParams guiState

            idAnnotation = readIdAnnotation dataParams model
            opAnnotation = applyOperation traceOp idAnnotation
            annotatedChartSpec =
              makeAnnotatedChartSpec model viewParams opAnnotation

        writeTVar idAnnotationTVar idAnnotation
        writeTVar opAnnotationTVar opAnnotation
        writeTVar annotatedChartSpecTVar annotatedChartSpec

  let updateData :: Model -> GUIState -> STM ()
      updateData model guiState = do
        let dataParams  = readDataParams guiState
            traceOp     = readTraceOperation guiState
            viewParams  = readViewParams guiState
            dataVersion = getTraceDataVersion model

        idAnnotationOld <- readTVar idAnnotationTVar
        idAnnotationNew <- do
          let newDeps = IdDataDependencies dataVersion dataParams
              oldDeps = idAnnotationOld ^. atsDependencies
          if  newDeps == oldDeps
            then return idAnnotationOld
            else writeReturnTVar idAnnotationTVar
                   $ readIdAnnotation dataParams model

        opAnnotationOld <- readTVar opAnnotationTVar
        opAnnotationNew <- do
          let newDeps = OpDataDependencies dataVersion dataParams traceOp
              oldDeps = opAnnotationOld ^. atsDependencies
          if  newDeps == oldDeps
            then return opAnnotationOld
            else writeReturnTVar opAnnotationTVar
                   $ applyOperation traceOp idAnnotationNew

        annotatedChartSpecOld <- readTVar annotatedChartSpecTVar
        let newDeps =
              DisplayDependencies dataVersion dataParams traceOp viewParams
            oldDeps = annotatedChartSpecOld ^. acsDependencies
        when (newDeps /= oldDeps) $
          writeTVar annotatedChartSpecTVar
            $ makeAnnotatedChartSpec model viewParams opAnnotationNew

  let getChart :: Model -> GUIState -> STM ChartSpec
      getChart model guiState = do
        updateData model guiState
        view acsChartSpec <$> readTVar annotatedChartSpecTVar

  let getMatches :: Model -> GUIState -> STM LevelShiftMatches
      getMatches model guiState = do
        updateData model guiState
        view atsLevelShiftMatches <$> readTVar idAnnotationTVar

  let getJumps :: Model -> GUIState -> STM (M.IntMap Double)
      getJumps model guiState = do
        updateData model guiState
        view atsJumps <$> readTVar idAnnotationTVar

  let getNewModel :: Model -> GUIState -> STM Model
      getNewModel model guiState = do
        updateData model guiState
        idAnnotation <- readTVar idAnnotationTVar
        let traceOp  = readTraceOperation guiState
            traceStateOp = getTraceStateTransform traceOp idAnnotation
            newModel = applyToModel traceStateOp model
        return newModel

  return (initializeInterpreter, getChart, getMatches, getJumps, getNewModel)

--------------------------------------------------------------------------------
-- Utility
--------------------------------------------------------------------------------

writeReturnTVar :: TVar a -> a -> STM a
writeReturnTVar tvar a = writeTVar tvar a >> return a
