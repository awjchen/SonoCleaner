-- This module contains the logic that acts on the sonomicrometry trace data. It
-- gathers and combines trace data, operations on that data, and parameters for
-- those operations in order to compute and display (generating a ChartSpec) the
-- results of those operations. Intermediate results are cached in an attempt to
-- avoid recomputation.

{-# LANGUAGE TemplateHaskell #-}

module Controller.Interpreter
  ( setupInterpreter
  ) where

import           Control.Arrow          ((&&&))
import           Control.Concurrent.STM
import           Control.Lens           hiding (index, indices, transform)
import           Control.Monad          (when)
import           Data.Colour            (blend, opaque)
import           Data.Colour.Names
import           Data.Default
import qualified Data.Text              as T
import           Data.Text.Lens
import           System.FilePath        (splitFileName)
import           Text.Printf

import           Controller.GUIState
import           Model
import           Types.Bounds
import           Types.Indices
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
  { dpLevelShiftThreshold :: Double
  , dpNoiseThreshold      :: Double
  } deriving (Eq)

-- 2
type LevelShiftIndex = Index1
type MatchLevel = Int
type Offset = Double

data TraceOperation =
    IdentityOp
  | AutoOp MatchLevel
  | ManualSingleOp   SingleAction   LevelShiftIndex   Offset (Int, Hold)
  | ManualMultipleOp MultipleAction [LevelShiftIndex] Offset
  | CropOp (Maybe (IndexInterval Index0))
  deriving (Eq)

-- 3
data ViewParams = ViewParams
  { vpViewBounds          :: ViewBounds
  , vpShowReplicateTraces :: Bool
  , vpReferenceTraceLabel :: (Int, Maybe T.Text)
  , vpCurrentPage         :: NotebookPage
  } deriving (Eq)

--------------------------------------------------------------------------------
-- Reading the parameters from the GUIState
--------------------------------------------------------------------------------

-- 1
readDataParams :: GUIState -> DataParams
readDataParams gst = DataParams
  { dpLevelShiftThreshold = gst ^. levelShiftThreshold
  , dpNoiseThreshold      = gst ^. noiseThreshold
  }

-- 2
readTraceOperation :: GUIState -> TraceOperation
readTraceOperation gst = case gst ^. currentPage of
  AutoPage ->
    AutoOp $ gst ^. matchLevel
  SinglePage levelShiftIndex ->
    ManualSingleOp (gst ^. singleAction)
                   levelShiftIndex
                   (gst ^. singleOffset)
                   (gst ^. singleHold)
  MultiplePage levelShiftGroup ->
    ManualMultipleOp (gst ^. multipleAction)
                     levelShiftGroup
                     (gst ^. multipleOffset)
  CropPage mCropInterval ->
    CropOp mCropInterval
  _ ->
    IdentityOp

-- 3
readViewParams :: GUIState -> ViewParams
readViewParams gst = ViewParams
  { vpViewBounds          = gst ^. viewBounds
  , vpShowReplicateTraces = gst ^. showReplicateTraces
  , vpReferenceTraceLabel = gst ^. referenceTraceLabel
  , vpCurrentPage         = gst ^. currentPage
  }

--------------------------------------------------------------------------------
-- Parameter dependencies for validating the intermediate cached computations
--------------------------------------------------------------------------------

-- B
data IdDataDependencies = IdDataDependencies
  { iddDataVersion :: Integer
  , iddDataParams  :: DataParams
  } deriving (Eq)

-- C
data OpDataDependencies = OpDataDependencies
  { nddDataVersion :: Integer
  , nddDataParams  :: DataParams
  , nddOperation   :: TraceOperation
  } deriving (Eq)

-- D
data DisplayDependencies = DisplayDependencies
  { ddDataVersion :: Integer
  , ddDataParams  :: DataParams
  , ddOperation   :: TraceOperation
  , ddViewParams  :: ViewParams
  } deriving (Eq)

--------------------------------------------------------------------------------
-- The 'objects/vertices' of the data flow
-- (Cached data annotated with precomputations and parameter dependencies)
--------------------------------------------------------------------------------

-- B, C
data AnnotatedTraceState a = AnnotatedTraceState
  { atsDependencies      :: a
  , atsTraceState        :: TraceState
  , atsJumps             :: IIntMap Index1 Double
  , atsLevelShiftMatches :: LevelShiftMatches }

-- D
data AnnotatedChartSpec = AnnotatedChartSpec
  { acsDependencies :: DisplayDependencies
  , acsChartSpec    :: ChartSpec  }

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
  let nt  = dpNoiseThreshold      dataParams
      lst = dpLevelShiftThreshold dataParams

      dataVersion = getTraceDataVersion model
      idTraceState = getCurrentState model
      idJumps = labelTraceStateJumps nt lst idTraceState
      idMatches = matchJumps nt idJumps idTraceState

  in  AnnotatedTraceState
        { atsDependencies      = IdDataDependencies
                                   { iddDataVersion = dataVersion
                                   , iddDataParams  = dataParams }
        , atsTraceState        = idTraceState
        , atsJumps             = idJumps
        , atsLevelShiftMatches = idMatches }


-- 2
applyOperation
  :: TraceOperation
  -> AnnotatedTraceState IdDataDependencies
  -> AnnotatedTraceState OpDataDependencies
applyOperation traceOp ats =
  let dataParams = iddDataParams (atsDependencies ats)
      transform = getTraceStateTransform traceOp ats
      newDependencies = OpDataDependencies
        { nddDataVersion = iddDataVersion (atsDependencies ats)
        , nddDataParams  = dataParams
        , nddOperation   = traceOp }
  in  if isIdentityOp transform
      then ats{ atsDependencies = newDependencies }
      else  let newTraceState = getOp transform (atsTraceState ats)
                newJumps = labelTraceStateJumps
                            (dpNoiseThreshold dataParams)
                            (dpLevelShiftThreshold dataParams)
                            newTraceState
                newMatches = matchJumps
                          (dpNoiseThreshold dataParams) newJumps newTraceState
            in  AnnotatedTraceState
                  { atsDependencies      = newDependencies
                  , atsTraceState        = newTraceState
                  , atsJumps             = newJumps
                  , atsLevelShiftMatches = newMatches }

-- helper for 2
getTraceStateTransform
  :: TraceOperation
  -> AnnotatedTraceState IdDataDependencies
  -> TraceOperator
getTraceStateTransform traceOp ats = case traceOp of
  IdentityOp -> IdOperator
  AutoOp matchLevel' ->
    applyMatchesOp (atsLevelShiftMatches ats) matchLevel'
  ManualSingleOp action index offset holdPair -> case action of
    SingleIgnore   -> IdOperator
    SingleZero     -> apply setZeroOp
    SingleSlopeFit -> apply setMedianOp
    where apply f = f (snd holdPair) offset index (atsJumps ats)
  ManualMultipleOp action indices offset -> case action of
    MultipleIgnore -> IdOperator
    MultipleLine   -> apply interpolateGroupOp
    MultipleCancel -> apply matchGroupOp
    where apply f = f offset indices (atsJumps ats)
  CropOp _ -> IdOperator

-- 3
makeAnnotatedChartSpec
  :: Model
  -> ViewParams
  -> AnnotatedTraceState OpDataDependencies
  -> AnnotatedChartSpec
makeAnnotatedChartSpec model viewParams ats =
  let atsDeps = atsDependencies ats
      displayDependencies = DisplayDependencies
        { ddDataVersion = nddDataVersion (atsDependencies ats)
        , ddDataParams  = nddDataParams atsDeps
        , ddOperation   = nddOperation  atsDeps
        , ddViewParams  = viewParams }

      chartSpec = specifyChart model viewParams ats

  in  AnnotatedChartSpec
        { acsDependencies = displayDependencies
        , acsChartSpec    = chartSpec }

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
    , plotXRange           = viewParams ^. to vpViewBounds . viewBoundsX
    , plotYRange           = viewParams ^. to vpViewBounds . viewBoundsY
    , plotBackgroundColour = bgColour
    , plotAnnotation       = annotation
    , plotTimes            = getTimes model
    , plotTimeStep         = getTimeStep model
    , plotToTime           = toTime
    , plotToIndex          = toIndex
    }
  where
    toTime  = timeAtPoint model
    toIndex = nearestPoint model

    traceSet = if vpShowReplicateTraces viewParams
      then TraceSet { showOriginal = True
                    , showTwin     = True }
      else def

    ----------------------------------------------------------------------------
    -- ChartSpec fields

    title = prefix ++ fileName ++ " (" ++ label ++ ")" where
      label = getLabel model
      fileName = snd $ splitFileName $ getFilePath model
      prefix = case vpCurrentPage viewParams of
        MainPage       -> "Main view -- "
        AutoPage       -> "Previewing automatic correction -- "
        SinglePage   _ -> "Previewing manual correction (single) -- "
        MultiplePage _ -> "Previewing manual correction (group) -- "
        LabelPage      -> "Adjusting labelling settings -- "
        ViewPage       -> "Selecting comparison traces -- "
        CropPage     _ -> "Cropping -- "
        QualityPage    -> "Setting trace quality -- "

    titleColour = case vpCurrentPage viewParams of
      MainPage       -> opaque black
      AutoPage       -> opaque greenyellow
      SinglePage   _ -> opaque yellow
      MultiplePage _ -> opaque magenta
      LabelPage      -> opaque plum
      ViewPage       -> opaque white
      CropPage     _ -> opaque orange
      QualityPage    -> opaque cyan

    bgColour = case getQuality model of
      Good     -> opaque grey
      Moderate -> opaque (blend 0.85 grey blue)
      Bad      -> opaque (blend 0.85 grey red)

    newSeries = ats ^. to atsTraceState . series

    newJumpIndices = atsJumps ats

    newModifiedIndices = ats ^. to atsTraceState . modifiedJumps

    originalSeries =
      if not $ showOriginal traceSet
      then Nothing
      else Just $ getInputState model ^. series

    twinSeries =
      if not $ showTwin traceSet
      then Nothing
      else fmap (view series) (getTwinTrace model)

    customSeries =
      let mTraceState =
            viewParams ^? to vpReferenceTraceLabel . _2 . _Just . unpacked
            >>= findTraceByLabel model
      in  case mTraceState of
            Nothing -> Nothing
            Just ts ->
              let vp = viewParams ^. to vpViewBounds . toViewPort
                  c2 = vp ^. viewPortCenter . _2
                  r2 = vp ^. viewPortRadii  . _2
                  (y0, y1) = ts ^. seriesBounds
                  c1 = (y1+y0)/2
                  r1 = (y1-y0)/2
              in  Just $ ivMap (\y -> (y-c1)*r2/r1 + c2) $ ts ^. series

    highlightRegion = case nddOperation (atsDependencies ats) of
      IdentityOp -> Nothing
      AutoOp _ -> Nothing
      ManualSingleOp _ j _ _ ->
        Just $ over both toTime $ runIndexInterval $ jumpEndpoints j
      ManualMultipleOp _ js _ ->
        Just $ over both toTime $ runIndexInterval
             $ iiUndiff $ IndexInterval $ (head &&& last) js
      CropOp cropBounds ->
        fmap (over both toTime . runIndexInterval) cropBounds

    annotation = case nddOperation (atsDependencies ats) of
      ManualSingleOp _ j _ _ ->
        let idSeries = getCurrentState model ^. series
            (i0, i1) = runIndexInterval $ jumpEndpoints j
            x1 = toTime i1
            y0 = ivIndex idSeries i0
            y1 = ivIndex idSeries i1
        in  Just (x1, (y0+y1)/2, printf "%.2f" (y1-y0))
      _ -> Nothing

--------------------------------------------------------------------------------
-- Initialization of the interpreter
--------------------------------------------------------------------------------

setupInterpreter :: IO ( Model -> GUIState -> STM ()
                       , Model -> GUIState -> STM ChartSpec
                       , Model -> GUIState -> STM LevelShiftMatches
                       , Model -> GUIState -> STM (IIntMap Index1 Double)
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
              oldDeps = atsDependencies idAnnotationOld
          if  newDeps == oldDeps
            then return idAnnotationOld
            else writeReturnTVar idAnnotationTVar
                   $ readIdAnnotation dataParams model

        opAnnotationOld <- readTVar opAnnotationTVar
        opAnnotationNew <- do
          let newDeps = OpDataDependencies dataVersion dataParams traceOp
              oldDeps = atsDependencies opAnnotationOld
          if  newDeps == oldDeps
            then return opAnnotationOld
            else writeReturnTVar opAnnotationTVar
                   $ applyOperation traceOp idAnnotationNew

        annotatedChartSpecOld <- readTVar annotatedChartSpecTVar
        let newDeps =
              DisplayDependencies dataVersion dataParams traceOp viewParams
            oldDeps = acsDependencies annotatedChartSpecOld
        when (newDeps /= oldDeps) $
          writeTVar annotatedChartSpecTVar
            $ makeAnnotatedChartSpec model viewParams opAnnotationNew

  let getChart :: Model -> GUIState -> STM ChartSpec
      getChart model guiState = do
        updateData model guiState
        acsChartSpec <$> readTVar annotatedChartSpecTVar

  let getMatches :: Model -> GUIState -> STM LevelShiftMatches
      getMatches model guiState = do
        updateData model guiState
        atsLevelShiftMatches <$> readTVar idAnnotationTVar

  let getJumps :: Model -> GUIState -> STM (IIntMap Index1 Double)
      getJumps model guiState = do
        updateData model guiState
        atsJumps <$> readTVar idAnnotationTVar

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
