-- This module is where the data, parameters, and operators come together; where
-- the actual computation takes place. The main result is a specification of
-- what to display to the user (a `ChartSpec`). The intention of this module is
-- optimization: intermediate steps in the computation are cached.

{-# LANGUAGE LambdaCase #-}

module Controller.Interpreter
  ( Handle (..)
  , Results (..)
  , setupInterpreter
  ) where

import           Control.Arrow          ((&&&))
import           Control.Concurrent.STM
import           Control.Lens           hiding (index, indices, transform)
import           Control.Monad          (when)
import           Data.Colour            (blend, opaque)
import           Data.Colour.Names
import           Data.Default
import qualified Data.Text              as T
import           Data.Text.Lens         (unpacked)
import           System.FilePath        (splitFileName)
import           Text.Printf            (printf)

import           Types.Bounds
import           Types.Indices
import           Types.LevelShifts

import           Model
import qualified View                   as View

import           Controller.GUIState

--------------------------------------------------------------------------------
-- Data flow
--------------------------------------------------------------------------------

-- A. Model (the "raw" data)
--      |
--      | 1. annotateRawData (parametrized by DataParams)
--      v
-- B. AnnotatedTraceState RawDataDependencies (the raw data, but with metadata)
--      |
--      | 2. applyTraceOperation (parametrized by Command)
--      v
-- C. AnnotatedTraceState NewDataDependencies (transformed data)
--      |
--      | 3. makeAnnotatedChartSpec (parametrized by ViewParams)
--      v
-- D. AnnotatedChartSpec (specification of the display (the main result))

--------------------------------------------------------------------------------
-- The parameters which determine the transformation and display of a trace
--------------------------------------------------------------------------------

-- 1
data DataParams = DataParams
  { dpLevelShiftThreshold :: Double
  , dpNoiseThreshold      :: Double
  } deriving (Eq)

-- 2
type MatchLevel = Int
type Offset = Double

data Command =
    IdentityOp
  | AutoOp MatchLevel
  | ManualSingleOp   SingleAction   Index1   Offset (Int, Hold)
  | ManualMultipleOp MultipleAction [Index1] Offset
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
readCommand :: GUIState -> Command
readCommand gst = case gst ^. currentPage of
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
-- Parameter dependencies. Used to check whether the cached intermediate
-- computations are still valid.
--------------------------------------------------------------------------------

-- B
data RawDataDependencies = RawDataDependencies
  { iddDataVersion :: Integer
  , iddDataParams  :: DataParams
  } deriving (Eq)

-- C
data NewDataDependencies = NewDataDependencies
  { nddDataVersion :: Integer
  , nddDataParams  :: DataParams
  , nddOperation   :: Command
  } deriving (Eq)

-- D
data DisplayDependencies = DisplayDependencies
  { ddDataVersion :: Integer
  , ddDataParams  :: DataParams
  , ddOperation   :: Command
  , ddViewParams  :: ViewParams
  } deriving (Eq)

--------------------------------------------------------------------------------
-- The 'objects' of the data flow (cached intermediate data, annotated with
-- their dependencies)
--------------------------------------------------------------------------------

-- B, C
data AnnotatedTraceState a = AnnotatedTraceState
  { atsDependencies      :: a
  , atsTraceState        :: TraceState
  , atsLevelShifts       :: IIntSet Index1
  , atsLevelShiftMatches :: LevelShiftMatches }

-- D
data AnnotatedChartSpec = AnnotatedChartSpec
  { acsDependencies :: DisplayDependencies
  , acsChartSpec    :: View.ChartSpec  }

--------------------------------------------------------------------------------
-- The functions between the 'objects'
--------------------------------------------------------------------------------

-- 1
annotateRawData
  :: DataParams
  -> Model
  -> AnnotatedTraceState RawDataDependencies
annotateRawData dataParams model =
  let nt  = dpNoiseThreshold      dataParams
      lst = dpLevelShiftThreshold dataParams

      dataVersion = getTraceDataVersion model
      idTraceState = getCurrentState model
      idLevelShifts = labelLevelShifts nt lst idTraceState
      idMatches = matchLevelShifts nt idLevelShifts idTraceState

  in  AnnotatedTraceState
        { atsDependencies      = RawDataDependencies
                                   { iddDataVersion = dataVersion
                                   , iddDataParams  = dataParams }
        , atsTraceState        = idTraceState
        , atsLevelShifts       = idLevelShifts
        , atsLevelShiftMatches = idMatches }


-- 2
applyTraceOperation
  :: Command
  -> AnnotatedTraceState RawDataDependencies
  -> AnnotatedTraceState NewDataDependencies
applyTraceOperation traceOp ats =
  let dataParams = iddDataParams (atsDependencies ats)
      transform = getTraceStateTransform traceOp ats
      newDependencies = NewDataDependencies
        { nddDataVersion = iddDataVersion (atsDependencies ats)
        , nddDataParams  = dataParams
        , nddOperation   = traceOp }
  in  if isIdentityOp transform
      then ats{ atsDependencies = newDependencies }
      else  let newTraceState = getOp transform (atsTraceState ats)
                newLevelShifts = labelLevelShifts
                                  (dpNoiseThreshold dataParams)
                                  (dpLevelShiftThreshold dataParams)
                                  newTraceState
                newMatches = matchLevelShifts
                               (dpNoiseThreshold dataParams)
                               newLevelShifts
                               newTraceState
            in  AnnotatedTraceState
                  { atsDependencies      = newDependencies
                  , atsTraceState        = newTraceState
                  , atsLevelShifts       = newLevelShifts
                  , atsLevelShiftMatches = newMatches }

-- helper for 2
getTraceStateTransform
  :: Command
  -> AnnotatedTraceState RawDataDependencies
  -> TraceOperator
getTraceStateTransform traceOp ats = case traceOp of
  IdentityOp -> IdOperator
  AutoOp matchLevel' ->
    applyMatchesOp (atsLevelShiftMatches ats) matchLevel'
  ManualSingleOp action index offset holdPair -> case action of
    SingleIgnore   -> IdOperator
    SingleZero     -> apply setZeroOp
    SingleSlopeFit -> apply setMedianOp
    where apply f = f (snd holdPair) offset index (atsLevelShifts ats)
  ManualMultipleOp action indices offset -> case action of
    MultipleIgnore -> IdOperator
    MultipleLine   -> apply interpolateGroupOp
    MultipleCancel -> apply matchGroupOp
    where apply f = f offset indices (atsLevelShifts ats)
  CropOp _ -> IdOperator

-- 3
makeAnnotatedChartSpec
  :: Model
  -> ViewParams
  -> AnnotatedTraceState NewDataDependencies
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
  -> AnnotatedTraceState NewDataDependencies
  -> View.ChartSpec
specifyChart model viewParams ats = View.ChartSpec
  { View.plotTitle =
      let label = getLabel model
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
            ScreenshotPage -> "Taking a screenshot -- "
      in  prefix ++ fileName ++ " (" ++ label ++ ")"
  , View.plotTitleColour =
      case vpCurrentPage viewParams of
        MainPage       -> opaque black
        AutoPage       -> opaque greenyellow
        SinglePage   _ -> opaque yellow
        MultiplePage _ -> opaque magenta
        LabelPage      -> opaque plum
        ViewPage       -> opaque white
        CropPage     _ -> opaque orange
        QualityPage    -> opaque cyan
        ScreenshotPage -> opaque beige
  , View.plotSeries           = ats ^. to atsTraceState . series
  , View.plotLevelShifts      = atsLevelShifts ats
  , View.plotModifiedSegments = ats ^. to atsTraceState . modifiedSegments
  , View.plotOriginalSeries   = if vpShowReplicateTraces viewParams
                            then Just $ getInputState model ^. series
                            else Nothing
  , View.plotTwinSeries       = if vpShowReplicateTraces viewParams
                            then fmap (view series) (getTwinTrace model)
                            else Nothing
  , View.plotCustomSeries =
        viewParams
      & preview (to vpReferenceTraceLabel . _2 . _Just . unpacked)
      & (>>= findTraceByLabel model)
      & \case
          Nothing -> Nothing
          Just ts -> Just $ ivMap rescale $ ts ^. series
            where
              vp = viewParams ^. to vpViewBounds . toViewPort
              c2 = vp ^. viewPortCenter . _2
              r2 = vp ^. viewPortRadii  . _2
              (y0, y1) = ts ^. seriesBounds
              c1 = (y1+y0)/2
              r1 = (y1-y0)/2
              rescale y = (y-c1)*r2/r1 + c2
  , View.plotHighlightRegion  =
      case nddOperation (atsDependencies ats) of
        IdentityOp -> Nothing
        AutoOp _ -> Nothing
        ManualSingleOp _ j _ _ ->
          Just $ runIndexInterval $ levelShiftEndpoints j
        ManualMultipleOp _ js _ ->
          Just $ runIndexInterval
              $ iiUndiff $ IndexInterval $ (head &&& last) js
        CropOp cropBounds ->
          fmap runIndexInterval cropBounds
  , View.plotXRange           = viewParams ^. to vpViewBounds . viewBoundsX
  , View.plotYRange           = viewParams ^. to vpViewBounds . viewBoundsY
  , View.plotBackgroundColour =
      case getQuality model of
        -- darkgrey (169) is lighter than grey (128) ...
        Good     -> opaque darkgrey
        Moderate -> opaque (blend 0.85 darkgrey blue)
        Bad      -> opaque (blend 0.85 darkgrey red)
  , View.plotAnnotation =
      case nddOperation (atsDependencies ats) of
        ManualSingleOp _ j _ _ ->
          let idSeries = getCurrentState model ^. series
              (i0, i1) = runIndexInterval $ levelShiftEndpoints j
              y0 = ivIndex idSeries i0
              y1 = ivIndex idSeries i1
          in  Just (i1, (y0+y1)/2, printf "%.2f" (y1-y0))
        _ -> Nothing
  , View.plotTimes            = getTimes model
  , View.plotTimeStep         = getTimeStep model
  , View.plotToTime           = timeAtPoint model
  , View.plotToIndex          = nearestPoint model
  }

--------------------------------------------------------------------------------
-- Interface of the interpreter
--------------------------------------------------------------------------------

-- Is there a better name for `Results`?

data Handle = Handle
  { getResults :: Model -> GUIState -> STM Results
  }

data Results = Results
  { resultChart       :: View.ChartSpec
  , resultMatches     :: LevelShiftMatches
  , resultLevelShifts :: IIntSet Index1
  , resultNewModel    :: Model
  }

setupInterpreter :: IO Handle
setupInterpreter = do
  idAnnotationTVar       <- newTVarIO undefined -- B
  opAnnotationTVar       <- newTVarIO undefined -- C
  annotatedChartSpecTVar <- newTVarIO undefined -- D

    -- Use default values for initialization
  atomically $ do
    let dataParams = readDataParams def
        traceOp    = readCommand def
        viewParams = readViewParams def

        idAnnotation = annotateRawData dataParams defaultModel
        opAnnotation = applyTraceOperation traceOp idAnnotation
        annotatedChartSpec =
          makeAnnotatedChartSpec defaultModel viewParams opAnnotation

    writeTVar idAnnotationTVar idAnnotation
    writeTVar opAnnotationTVar opAnnotation
    writeTVar annotatedChartSpecTVar annotatedChartSpec

  let updateData :: Model -> GUIState -> STM ()
      updateData model guiState = do
        let dataParams  = readDataParams guiState
            traceOp     = readCommand guiState
            viewParams  = readViewParams guiState
            dataVersion = getTraceDataVersion model

        idAnnotationOld <- readTVar idAnnotationTVar
        idAnnotationNew <- do
          let newDeps = RawDataDependencies dataVersion dataParams
              oldDeps = atsDependencies idAnnotationOld
          if  newDeps == oldDeps
            then return idAnnotationOld
            else writeReturnTVar idAnnotationTVar
                   $ annotateRawData dataParams model

        opAnnotationOld <- readTVar opAnnotationTVar
        opAnnotationNew <- do
          let newDeps = NewDataDependencies dataVersion dataParams traceOp
              oldDeps = atsDependencies opAnnotationOld
          if  newDeps == oldDeps
            then return opAnnotationOld
            else writeReturnTVar opAnnotationTVar
                   $ applyTraceOperation traceOp idAnnotationNew

        annotatedChartSpecOld <- readTVar annotatedChartSpecTVar
        let newDeps =
              DisplayDependencies dataVersion dataParams traceOp viewParams
            oldDeps = acsDependencies annotatedChartSpecOld
        when (newDeps /= oldDeps) $
          writeTVar annotatedChartSpecTVar
            $ makeAnnotatedChartSpec model viewParams opAnnotationNew

  let getResults' :: Model -> GUIState -> STM Results
      getResults' model guiState = do
        updateData model guiState

        chart <- acsChartSpec <$> readTVar annotatedChartSpecTVar
        idAnnotation <- readTVar idAnnotationTVar

        let matches = atsLevelShiftMatches idAnnotation
            levelShifts = atsLevelShifts idAnnotation

            -- We must manually extract `TraceStateOperator` and use `applyToModel`,
            -- since this is the only way to transform the Model.
            traceOp  = readCommand guiState
            traceStateOp = getTraceStateTransform traceOp idAnnotation
            newModel = applyToModel traceStateOp model

        pure $ Results chart matches levelShifts newModel

  pure $ Handle getResults'

--------------------------------------------------------------------------------
-- Utility
--------------------------------------------------------------------------------

writeReturnTVar :: TVar a -> a -> STM a
writeReturnTVar tvar a = writeTVar tvar a >> return a
