-- `Model` is the type that represents all of the data the program is intended
-- to process. Conceputally, the program should have at most one instance of
-- this type at a time.

{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeApplications  #-}

module Model.Model
  -- Types
  ( Model
  , TraceQuality (..)

  -- Data versioning
  , getTraceDataVersion

  -- Cropping
  , isCropped
  , crop
  , uncrop

  -- Trace annotations
  , getTraceAnnotation

  -- Selecting traces
  , existsPrevTrace
  , existsNextTrace
  , gotoNextTrace
  , gotoPrevTrace

  , findTraceByLabel

  , getTwinTrace
  , gotoTwinTrace

  -- Trace history
  , existsPrevHistory
  , existsNextHistory
  , undo
  , redo

  -- Trace quality
  , setQuality
  , getQuality

  -- Modifying data
  , applyToModel

  -- Querying
  , getFilePath
  , getTimeStep
  , getLabels

  , getInputState
  , getLabel

  , getCurrentState
  , getTraceBounds

  , getTimes
  , timeAtPoint
  , timeAtSlope
  , nearestPoint
  , nearestSlope

  -- Model initialization
  , loadSSAFile
  , initModel

  -- Model output
  , saveSSAFile
  ) where

import           Control.Applicative              ((<|>))
import           Control.Exception
import           Control.Lens
import           Control.Monad                    (mzero, when)
import           Control.Monad.IO.Class           (liftIO)
import           Control.Monad.Trans.Except       (ExceptT (..))
import           Control.Monad.Trans.State.Strict
import qualified Data.ByteString.Lazy             as BL
import           Data.Csv
import           Data.Foldable                    (find, foldl')
import           Data.List                        (findIndex, stripPrefix)
import qualified Data.Map.Strict                  as M
import           Data.Maybe                       (fromMaybe, mapMaybe)
import           Data.Time.Clock.POSIX            (getPOSIXTime)
import           Data.Tuple                       (swap)
import qualified Data.Vector                      as V
import qualified Data.Vector.Unboxed              as VU
import           GHC.Generics                     hiding (to)
import           System.Directory                 (createDirectoryIfMissing)
import           System.FilePath                  (splitFileName, (</>))
import           System.IO.Error                  (isDoesNotExistError)
import           Text.Printf                      (printf)

import           Types.Bounds
import           Types.Indices
import qualified Types.Zipper                     as Z

import           Model.Ssa
import           Model.TraceOperators
import           Model.TraceState

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

data Model a = Model
  { _filePath         :: FilePath
  , _ssaFile          :: SSA
  -- We store the (simplified) timestamps here, in `fakeTimes` merely to avoid
  -- their recomputation
  , _fakeTimes        :: IVector Index0 Double
  , _traces           :: Z.Zipper (TraceInfo a)
  , _timeStep         :: Double
  , _traceDataVersion :: Integer
  } deriving (Functor)

data TraceInfo a = TraceInfo
  { _history    :: Z.Zipper TraceState
  , _label      :: String
  , _quality    :: TraceQuality
  , _annotation :: a
  } deriving (Functor)

data TraceQuality = Bad
                  | Moderate
                  | Good
  deriving (Eq, Enum, Ord, Show, Generic)

instance ToField TraceQuality where
  toField Good     = "Good"
  toField Moderate = "Moderate"
  toField Bad      = "Bad"

instance FromField TraceQuality where
  parseField s
    | s == "Good"     = pure Good
    | s == "Moderate" = pure Moderate
    | s == "Bad"      = pure Bad
    | otherwise       = mzero

makeLenses ''Model
makeLenses ''TraceInfo

currentTrace :: Lens' (Model a) (TraceInfo a)
currentTrace = traces . Z.extract

currentTraceState :: Lens' (Model a) TraceState
currentTraceState = currentTrace . history . Z.extract

-------------------------------------------------------------------------------
-- Data versioning
-------------------------------------------------------------------------------

-- We track when the current `TraceState` changes in order to avoid
-- recomputation. Changes are signaled by `incrementVersion`.

incrementVersion :: Model a -> Model a
incrementVersion = over traceDataVersion succ

getTraceDataVersion :: Model a -> Integer
getTraceDataVersion = view traceDataVersion

-------------------------------------------------------------------------------
-- Cropping
-------------------------------------------------------------------------------

-- All functions relying on the Model should not need to care about whether or
-- not the traced are cropped; that is, it is the Model's responsibility to
-- handle cropping in a way that is invisible beyond its interface. For example,
-- the model provides conversion functions between times and indices which
-- depend on the cropping state of the data. Of course, these functions must
-- only be applied to data from the Model which generated them, and so there is
-- still room for error.

-- Cropping invariant: all `TraceState`s (i.e. those traversed by
-- `allTraceStates`) must be cropped in the same way (i.e. by the same series of
-- intervals).

allTraceStates :: Traversal' (Model a) TraceState
allTraceStates = traces . traverse . history . traverse

-- By the cropping invariant, we need only inspect a single trace to determine
-- the cropping context.
croppingContext :: Lens' (Model a) TraceContext
croppingContext = currentTraceState . context

isCropped :: Model a -> Bool
isCropped model =
  case model ^. croppingContext of
    RootContext _        -> False
    CroppedContext _ _ _ -> True

getCropBounds :: Model a -> IndexInterval Index0
getCropBounds model =
  case model ^. croppingContext of
    RootContext bounds -> bounds
    CroppedContext totalOffset cropInterval _ ->
      let interval@(l, _) = runIndexInterval cropInterval
          shift = index0 totalOffset `iMinus` l
      in  IndexInterval $ over both (iTranslate shift) interval

crop :: IndexInterval Index0 -> Model a -> Model a
crop cropInterval =
    incrementVersion
  . over allTraceStates (cropTraceState cropInterval)

uncrop :: Model a -> Model a
uncrop =
    incrementVersion
  . over allTraceStates uncropTraceState

-------------------------------------------------------------------------------
-- Trace annotations
-------------------------------------------------------------------------------

getTraceAnnotation :: Model a -> a
getTraceAnnotation = view (currentTrace . annotation)

-- The intention of this function is to couple the saving of the trace
-- annotation to trace selection by decorating e.g. `gotoNextTrace`.
swapOutTraceAnnotations :: (Model a -> Model a) -> a -> Model a -> Model a
swapOutTraceAnnotations f newAnnotation =
  f . set (currentTrace . annotation) newAnnotation

-------------------------------------------------------------------------------
-- Selecting traces
-------------------------------------------------------------------------------

-- Walking along the zipper

existsPrevTrace :: Model a -> Bool
existsPrevTrace model =
  not $ null $ model ^. traces . Z.lefts

existsNextTrace :: Model a -> Bool
existsNextTrace model =
  not $ null $ model ^. traces . Z.rights

gotoNextTrace :: a -> Model a -> Model a
gotoNextTrace = swapOutTraceAnnotations gotoNextTrace'

gotoNextTrace' :: Model a -> Model a
gotoNextTrace' = incrementVersion
  . over traces Z.tugRight

gotoPrevTrace :: a -> Model a -> Model a
gotoPrevTrace = swapOutTraceAnnotations gotoPrevTrace'

gotoPrevTrace' :: Model a -> Model a
gotoPrevTrace' = incrementVersion
  . over traces Z.tugLeft

-- By label

findTraceByLabel :: Model a -> String -> Maybe TraceState
findTraceByLabel model label' =
  view (history . Z.extract)
    <$> find ((== label') . view label) (model ^. traces)

-- Twin traces

getTwinTrace :: Model a -> Maybe TraceState
getTwinTrace model = do
  let label'  = model ^. currentTrace . label
  twinLabel' <- twinLabel label'
  findTraceByLabel model twinLabel'

parseTrxLabel :: String -> Maybe (String, String)
parseTrxLabel str | Just suffix <- stripPrefix "TRX" str
                  , [t1, t2, ':', r1, r2] <- suffix
                  = Just ([t1, t2], [r1, r2])
                  | otherwise = Nothing

makeTrxLabel :: (String, String) -> String
makeTrxLabel (t, r) = "TRX" ++ t ++ ":" ++ r

twinLabel :: String -> Maybe String
twinLabel = fmap (makeTrxLabel . swap) . parseTrxLabel

twinLabelQuotient :: String -> Maybe String
twinLabelQuotient str = makeTrxLabel . sort2 <$> parseTrxLabel str
  where
    sort2 (s1, s2) = (min s1 s2, max s1 s2)

gotoTwinTrace :: a -> Model a -> Model a
gotoTwinTrace = swapOutTraceAnnotations gotoTwinTrace'

gotoTwinTrace' :: Model a -> Model a
gotoTwinTrace' model = case getTwinTrace model of
  Nothing -> model
  Just _  ->
    -- Linear search for the position of the twin trace
    let label'  = model ^. currentTrace . label
        traces' = model ^. traces
        mResult = do
          twinLabel' <- twinLabel label'
          let mRight = fmap ('r',) $ findIndex ((== twinLabel') . view label)
                     $ traces' ^. Z.rights
              mLeft  = fmap ('l',) $ findIndex ((== twinLabel') . view label)
                     $ traces' ^. Z.lefts
          mLeft <|> mRight
    -- Walk along the zipper
    in case mResult of
        Just ('r', n) -> foldl' (&) model (replicate (n+1) gotoNextTrace')
        Just ('l', n) -> foldl' (&) model (replicate (n+1) gotoPrevTrace')
        _             -> model

-------------------------------------------------------------------------------
-- Trace history
-------------------------------------------------------------------------------

existsPrevHistory :: Model a -> Bool
existsPrevHistory model =
  not $ null $ model ^. currentTrace . history . Z.lefts

existsNextHistory :: Model a -> Bool
existsNextHistory model =
  not $ null $ model ^. currentTrace . history . Z.rights

undo :: Model a -> Model a
undo = incrementVersion
  . over (currentTrace . history) Z.tugLeft

redo :: Model a -> Model a
redo = incrementVersion
  . over (currentTrace . history) Z.tugRight

-------------------------------------------------------------------------------
-- Trace quality
-------------------------------------------------------------------------------

setQuality :: TraceQuality -> Model a -> Model a
setQuality = set (currentTrace . quality)

getQuality :: Model a -> TraceQuality
getQuality = view $ currentTrace . quality

qualityFileSuffix :: String
qualityFileSuffix = ".quality"

-------------------------------------------------------------------------------
-- Modifying data
-------------------------------------------------------------------------------

-- Lifting a TraceOperator via `applyToModel` is the only way to manipulate the
-- data contained in a `Model` from outside the module (other than by cropping,
-- which doesn't count because it only masks the data).

applyToModel :: TraceOperator -> Model a -> Model a
applyToModel traceOp = incrementVersion
  . over (currentTrace . history)
      (\hist -> Z.clobberRight (getOp traceOp (hist ^. Z.extract)) hist)

-------------------------------------------------------------------------------
-- Querying
-------------------------------------------------------------------------------

-- Ssa file

getFilePath :: Model a -> String
getFilePath = view filePath

getTimeStep :: Model a -> Double
getTimeStep = view timeStep

getLabels :: Model a -> [String]
getLabels model = model ^.. traces . folded . label

-- Current trace

getInputState :: Model a -> TraceState
getInputState = view $ currentTrace . history . Z.head

getLabel :: Model a -> String
getLabel = view $ currentTrace . label

-- Current `TraceState`

getCurrentState :: Model a -> TraceState
getCurrentState = view currentTraceState

getTraceBounds :: Model a -> ViewBounds
getTraceBounds model =
  let traceState = getCurrentState model
      s = traceState ^. series
      time = timeAtPoint model
      boundsX = over both time $ runIndexInterval $ iiGetIVectorBounds s
      boundsY = traceState ^. seriesBounds
  in  ViewBounds boundsX boundsY

-- Times (depends on cropping state)

getTimes :: Model a -> IVector Index0 Double
getTimes model = unsafeIvSlice (getCropBounds model) $ model ^. fakeTimes

timeAtPoint :: Model a -> Index0 -> Double
timeAtPoint model i = getTimes model `ivIndex` i

timeAtSlope :: Model a -> Index1 -> Double
timeAtSlope model j = let mid (x, y) = 0.5*(x+y) in
  mid $ over both (timeAtPoint model) $ runIndexInterval $ levelShiftEndpoints j

nearestPoint :: Model a -> Double -> Index0
nearestPoint model t =
  let offset = unsafeRunIndex0 $ iiLeft $ getCropBounds model
      dt = getTimeStep model
      bounds = iiGetIVectorBounds (getTimes model)
  in  iiBound bounds $ index0 $ subtract offset $ round $ t/dt

nearestSlope :: Model a -> Double -> Index1
nearestSlope model t =
  let offset = unsafeRunIndex0 $ iiLeft $ getCropBounds model
      dt = getTimeStep model
      bounds = iiDiff $ iiGetIVectorBounds (getTimes model)
  in  iiBound bounds $ index1 $ subtract offset $ round $ t/dt - 0.5

-------------------------------------------------------------------------------
-- Model initialization
-------------------------------------------------------------------------------

loadSSAFile :: FilePath -> ExceptT String IO (Model ())
loadSSAFile filePath' = do
  ssa <- loadSSA filePath'
  traceQualities <- readQualityFile filePath'

  -- We only require that the `traceDataVersion` is not the same as the that of
  -- an existing model. The following implementation should suffice, since both
  -- the data version and time can only increase.
  newDataVersion <- fmap (negate . round) $ liftIO $ getPOSIXTime

  ExceptT $ pure $ initModel filePath' ssa traceQualities newDataVersion

initModel
  :: FilePath
  -> SSA
  -> [(String, TraceQuality)]
  -> Integer
  -> Either String (Model ())
initModel filePath' ssa traceQualities newDataVersion = do
  let dt = ssa ^. ssaSampleTimeInterval
      readQuality ti = ti & set quality
        (fromMaybe Good $ Prelude.lookup (ti ^. label) traceQualities)
      dataLength = VU.length (ssa ^. ssaIndexTrace . traceSeries)

      -- We assume that the data is a time series, allowing us to pretend that
      -- the measurements are evenly spaced in time. Using these fake timestamps
      -- simplifies conversions between times and indices.
      fakeTimes' = ivector $ VU.generate dataLength ((*dt) . fromIntegral)
      traces' = Z.unsafeFromList
              $ over (partsOf (traverse . label)) renameLabels
              $ map (readQuality . initTraceInfo)
              $ ssa ^. ssaDataTraces

  when (dataLength < 3) $ Left $ concat
    [ "Unacceptable .ssa file '"
    , filePath'
    , "': traces must have at least 3 data points." ]

  pure Model { _filePath  = filePath'
               , _ssaFile   = ssa
               , _fakeTimes = fakeTimes'
               , _traces    = traces'
               , _timeStep  = dt
               , _traceDataVersion = newDataVersion }

initTraceInfo :: Trace -> TraceInfo ()
initTraceInfo tr =
  let series' = ivector $ tr ^. traceSeries
  in TraceInfo
    { _label      = tr ^. traceLabel
    , _history    = Z.fromNonEmpty $ pure $ initTraceState series'
    , _quality    = Good
    , _annotation = ()
    }

readQualityFile :: FilePath -> ExceptT String IO [(String, TraceQuality)]
readQualityFile ssaFilePath' =
  let qualityFilePath = ssaFilePath' ++ qualityFileSuffix
      reader :: IO (Either String [(String, TraceQuality)])
      reader = (fmap V.toList . decode NoHeader <$> BL.readFile qualityFilePath)
      handler :: IOException -> IO (Either String [(String, TraceQuality)])
      handler e
        | isDoesNotExistError e = pure $ Right []
        | otherwise = pure $ Left $ show e
  in  ExceptT $ catch reader handler

-- For uniqueness
renameLabels :: [String] -> [String]
renameLabels labels =
  let counts = M.fromListWith (+) $ fmap (flip (,) (1 :: Int)) labels
      rename :: String -> State (M.Map String Int) String
      rename lbl = case M.lookup lbl counts of
        Nothing -> pure lbl
        Just 1  -> pure lbl
        Just _  -> do
          suffixInt <- at lbl . non 0 <+= 1
          pure $ lbl ++ " (" ++ show suffixInt ++ ")"
  in  evalState (traverse rename labels) M.empty

-------------------------------------------------------------------------------
-- Model output
-------------------------------------------------------------------------------

toSSA :: Model a -> SSA
toSSA model =
  let newSeries = model & toListOf
        (traces . folded . history . Z.extract . series . to unsafeRunIVector)
      oldTraces = model ^. ssaFile . ssaDataTraces
      newTraces = zipWith (set traceSeries) newSeries oldTraces
      traceLength = VU.length $ head newSeries
      offset = unsafeRunIndex0 $ iiLeft $ getCropBounds model
  in  model ^. ssaFile & ssaDataTraces .~ newTraces
                       & ssaIndexTrace . traceSeries %~
                           VU.slice offset traceLength

saveSSAFile :: FilePath -> Model a -> ExceptT String IO String
saveSSAFile filePath' model =
  let sanitize = fmap (\x -> if x == '/' then '_' else x)
      (outputDir, fileName) = splitFileName filePath'
      outputFileName = sanitize fileName
      outputQualityFileName = outputFileName ++ qualityFileSuffix
      outputQualityFilePath  = outputDir </> outputQualityFileName
      outputFileMessage = printf "Wrote file: %s." outputFileName
  in do
    liftIO $ createDirectoryIfMissing True outputDir
    writeSSA filePath' $ toSSA model
    qualityStatements <- writeQualityFile outputQualityFilePath model
    return $ unlines $ [outputFileMessage, ""] ++ qualityStatements

writeQualityFile :: FilePath -> Model a -> ExceptT String IO [String]
writeQualityFile filePath' model = do
  let qualityOutput = zip (model ^.. traces . folded . label)
                          (model ^.. traces . folded . quality)
  ExceptT $ catch (fmap Right $ BL.writeFile filePath' $ encode qualityOutput)
                  (fmap Left . pure . show @IOException)
  return $ traceQualitySummary qualityOutput

traceQualitySummary :: [(String, TraceQuality)] -> [String]
traceQualitySummary qualities =
  let bestTrxQualities = M.toList $ M.fromListWith max
                       $ mapMaybe (traverseOf _1 twinLabelQuotient) qualities
      counts = M.toList $ M.fromListWith (+)
             $ flip zip (repeat (1::Int)) $ fmap snd bestTrxQualities
      summary qual count =
        printf "%d %s TRX channels." count (show qual)
  in  fmap (uncurry summary) counts
