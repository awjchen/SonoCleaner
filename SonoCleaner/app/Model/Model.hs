-- The type that holds all the data on which the program operates

{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}

module Model.Model
  -- Types
  ( Model
  , TraceQuality (..)

  , initUndefinedModel

  -- Model operations
  , applyToModel

  , crop
  , uncrop

  , setQuality

  , undo
  , redo

  , gotoNextTrace
  , gotoPrevTrace
  , gotoTwinTrace

  -- Model accessors
  , getTraceDataVersion

  , getFilePath

  , getCurrentState
  , getInputState
  , getTimeStep
  , getLabel
  , getLabels
  , getTraceBounds

  , getTimes
  , getIndexTimeConversions
  , getIndexToTime
  , getTimeToIndex

  , getQuality

  , findTraceByLabel
  , getTwinTrace

  , existsPrevTrace
  , existsNextTrace
  , existsPrevHistory
  , existsNextHistory

  -- File input
  , loadSSAFile

  -- File output
  , saveSSAFile
  ) where

import           Control.Applicative        ((<|>))
import           Control.Exception
import           Control.Lens
import           Control.Monad              (mzero)
import           Control.Monad.Trans.Except (ExceptT (..))
import qualified Data.ByteString.Lazy       as BL
import           Data.Csv
import           Data.Foldable              (find, foldl')
import           Data.List                  (findIndex, stripPrefix)
import qualified Data.List.NonEmpty         as NE
import qualified Data.Map.Strict            as M
import           Data.Maybe                 (fromMaybe, mapMaybe)
import           Data.Tuple                 (swap)
import qualified Data.Vector                as V
import qualified Data.Vector.Unboxed        as VU
import           GHC.Generics
import           System.Directory           (createDirectoryIfMissing)
import           System.FilePath            (splitFileName, (</>))
import           Text.Printf                (printf)

import           SonoSsa.Ssa
import           Types.Bounds
import qualified Types.IndexInterval        as I
import qualified Types.Zipper               as Z

import           Model.TraceState

-- TODO : Model initialization (default model?)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

data Model = Model
  { _filePath         :: FilePath
  , _ssaFile          :: SSA
  -- We store the (fake) timestamps here merely to avoid their recomputation
  , _fakeTimes        :: VU.Vector Double
  , _traces           :: Z.Zipper TraceInfo
  , _timeStep         :: Double
  , _cropHistory      :: NE.NonEmpty I.IndexInterval
  , _traceDataVersion :: Integer }

initUndefinedModel :: Model
initUndefinedModel = Model
  { _filePath         = undefined
  , _ssaFile          = undefined
  , _fakeTimes        = undefined
  , _traces           = undefined
  , _timeStep         = undefined
  , _cropHistory      = undefined
  , _traceDataVersion = 0
  }

data TraceInfo = TraceInfo
  { _history :: Z.Zipper TraceState
  , _label   :: String
  , _quality :: TraceQuality
  }

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

-------------------------------------------------------------------------------
-- Lenses
-------------------------------------------------------------------------------

makeLenses ''Model
makeLenses ''TraceInfo

currentTrace :: Lens' Model TraceInfo
currentTrace = traces . Z.extract

-------------------------------------------------------------------------------
-- Model operations
-------------------------------------------------------------------------------

-- We track versions of the data to avoid recomputations
incrementVersion :: Model -> Model
incrementVersion = over traceDataVersion succ

-- Lifting a TraceStateOperator via 'applyToModel' is the only way to manipulate
-- the data contained in a 'Model' from outside the module (other than by
-- cropping, which doesn't really affect the data).
applyToModel :: TraceStateOperator -> Model -> Model
applyToModel tsOp = incrementVersion
  . over (currentTrace . history)
      (\hist -> Z.clobberRight (getOperator tsOp (hist ^. Z.extract)) hist)

-- Cropping

allTraceStates :: Traversal' Model TraceState
allTraceStates = traces . traverse . history . traverse

-- Relative, inclusive bounds
crop :: I.IndexInterval -> Model -> Model
crop indexInterval = incrementVersion
  . over allTraceStates (cropTraceState indexInterval)
  . over cropHistory (indexInterval NE.<|)

uncrop :: Model -> Model
uncrop model =
  let (indexInterval NE.:| bounds) = model ^. cropHistory
  in  case NE.nonEmpty bounds of
        Nothing -> model
        Just bounds' -> model
          & incrementVersion
          & allTraceStates %~ uncropTraceState indexInterval
          & cropHistory .~ bounds'

-- Quality

setQuality :: TraceQuality -> Model -> Model
setQuality = set (currentTrace . quality)

-- History

undo :: Model -> Model
undo = incrementVersion
  . over (currentTrace . history) Z.tugLeft

redo :: Model -> Model
redo = incrementVersion
  . over (currentTrace . history) Z.tugRight

-- Navigating traces

gotoNextTrace :: Model -> Model
gotoNextTrace = incrementVersion
  . over traces Z.tugRight

gotoPrevTrace :: Model -> Model
gotoPrevTrace = incrementVersion
  . over traces Z.tugLeft

gotoTwinTrace :: Model -> Model
gotoTwinTrace model = case getTwinTrace model of
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
        Just ('r', n) -> foldl' (&) model (replicate (n+1) gotoNextTrace)
        Just ('l', n) -> foldl' (&) model (replicate (n+1) gotoPrevTrace)
        _             -> model

-------------------------------------------------------------------------------
-- Model accessors
-------------------------------------------------------------------------------

-- Trace data version

getTraceDataVersion :: Model -> Integer
getTraceDataVersion = view traceDataVersion

-- Current file information

getFilePath :: Model -> String
getFilePath = view filePath

getCropBounds :: Model -> I.IndexInterval
getCropBounds model =
  let (mostRecentCrop NE.:| previousCrops) = model ^. cropHistory
      previousOffset = sum $ fmap (fst . I.getEndpoints) previousCrops
  in  I.translate previousOffset mostRecentCrop

-- Current trace information

getCurrentState :: Model -> TraceState
getCurrentState = view $ currentTrace . history . Z.extract

getInputState :: Model -> TraceState
getInputState = view $ currentTrace . history . Z.head

getTimeStep :: Model -> Double
getTimeStep = view timeStep

getLabel :: Model -> String
getLabel = view $ currentTrace . label

getLabels :: Model -> [String]
getLabels model = model ^.. traces . folded . label

getTraceBounds :: Model -> ViewBounds
getTraceBounds model =
  let traceState = getCurrentState model
      s = traceState ^. series
      toTime = getIndexToTime model
      boundsX = (toTime 0, toTime (VU.length s - 1))
      boundsY = traceState ^. seriesBounds
  in  ViewBounds boundsX boundsY

getTimes :: Model -> VU.Vector Double
getTimes model = I.slice (getCropBounds model) $ model ^. fakeTimes

getIndexTimeConversions :: Model -> (Int -> Double, Double -> Int)
getIndexTimeConversions model =
  let offset = I.leftEndpoint $ getCropBounds model
      dt = getTimeStep model
  in  ( \i -> dt * fromIntegral (i+offset)
      , \x -> subtract offset $ floor (x/dt) )

getIndexToTime :: Model -> Int -> Double
getIndexToTime = fst . getIndexTimeConversions

getTimeToIndex :: Model -> Double -> Int
getTimeToIndex = snd . getIndexTimeConversions

getQuality :: Model -> TraceQuality
getQuality = view $ currentTrace . quality

-- Related traces

findTraceByLabel :: Model -> String -> Maybe TraceState
findTraceByLabel model label' =
  view (history . Z.extract)
    <$> find ((== label') . view label) (model ^. traces)

getTwinTrace :: Model -> Maybe TraceState
getTwinTrace model = do
  let label'  = model ^. currentTrace . label
  twinLabel' <- twinLabel label'
  findTraceByLabel model twinLabel'

-- Zipper positions for files and traces and trace history

existsPrevTrace :: Model -> Bool
existsPrevTrace model =
  not $ null $ model ^. traces . Z.lefts

existsNextTrace :: Model -> Bool
existsNextTrace model =
  not $ null $ model ^. traces . Z.rights

existsPrevHistory :: Model -> Bool
existsPrevHistory model =
  not $ null $ model ^. currentTrace . history . Z.lefts

existsNextHistory :: Model -> Bool
existsNextHistory model =
  not $ null $ model ^. currentTrace . history . Z.rights

-------------------------------------------------------------------------------
-- Twin traces
-------------------------------------------------------------------------------

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

-------------------------------------------------------------------------------
-- File initialization
-------------------------------------------------------------------------------

loadSSAFile :: FilePath -> Model -> ExceptT String IO Model
loadSSAFile filePath' model = do
  ssa <- loadSSA filePath'
  traceQualities <- readQualityFile filePath'
  let dt = ssa ^. ssaSampleTimeInterval
      readQuality ti = ti & set quality
        (fromMaybe Good $ Prelude.lookup (ti ^. label) traceQualities)
      dataLength = VU.length (ssa ^. ssaIndexTrace . traceSeries)
      bounds = I.fromEndpoints (0, dataLength - 1)
      -- We assume that the data is a time series, allowing us to pretend that
      -- the measurements are evenly spaced in time. Using these fake timestamps
      -- makes conversions between times and indices much simpler.
      fakeTimes' = VU.generate dataLength ((*dt) . fromIntegral)
      traces' = Z.unsafeFromList
              $ map (readQuality . initTraceInfo)
              $ ssa ^. ssaDataTraces
  return Model { _filePath  = filePath'
               , _ssaFile   = ssa
               , _fakeTimes = fakeTimes'
               , _traces    = traces'
               , _timeStep  = dt
               , _cropHistory = bounds NE.:| []
               , _traceDataVersion = succ (_traceDataVersion model) }

initTraceInfo :: Trace -> TraceInfo
initTraceInfo tr =
  let s = tr ^. traceSeries
  in TraceInfo
    { _label   = tr ^. traceLabel
    , _history = Z.fromNonEmpty $ pure $ initTraceState s
    , _quality = Good
    }

readQualityFile :: FilePath -> ExceptT String IO [(String, TraceQuality)]
readQualityFile ssaFilePath' =
  let qualityFilePath = ssaFilePath' ++ qualityFileSuffix
      handler :: IOException -> IO (Either String [(String, TraceQuality)])
      handler _ = return $ Right []
      reader :: IO (Either String [(String, TraceQuality)])
      reader = fmap V.toList . decode NoHeader <$> BL.readFile qualityFilePath
  in  ExceptT $ catch reader handler

-------------------------------------------------------------------------------
-- File output
-------------------------------------------------------------------------------

toSSA :: Model -> SSA
toSSA model =
  let newSeries = model ^.. traces . folded . history . Z.extract . series
      oldTraces = model ^.  ssaFile . ssaDataTraces
      newTraces = zipWith (set traceSeries) newSeries oldTraces
      traceLength = VU.length $ head newSeries
      offset = I.leftEndpoint $ getCropBounds model
  in  model ^. ssaFile & ssaDataTraces .~ newTraces
                       & ssaIndexTrace . traceSeries %~
                           VU.slice offset traceLength

saveSSAFile :: FilePath -> Model -> IO String
saveSSAFile filePath' model =
  let sanitize = fmap (\x -> if x == '/' then '_' else x)
      (outputDir, fileName) = splitFileName filePath'
      outputFileName = sanitize fileName
      outputQualityFileName = outputFileName ++ qualityFileSuffix
      outputQualityFilePath  = outputDir </> outputQualityFileName
      outputFileMessage = printf "Wrote file: %s." outputFileName
  in do
    createDirectoryIfMissing True outputDir
    writeSSA filePath' $ toSSA model
    qualityStatements <- writeQualityFile outputQualityFilePath model
    return $ unlines $ [outputFileMessage, ""] ++ qualityStatements

writeQualityFile :: FilePath -> Model -> IO [String]
writeQualityFile filePath' model = do
  let qualityOutput = zip (model ^.. traces . folded . label)
                          (model ^.. traces . folded . quality)
  BL.writeFile filePath' $ encode qualityOutput
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

-------------------------------------------------------------------------------
-- File handling constants
-------------------------------------------------------------------------------

qualityFileSuffix :: String
qualityFileSuffix = ".quality"
