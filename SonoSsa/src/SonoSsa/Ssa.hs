{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}

module SonoSsa.Ssa
  ( SSA
  , ssaFilePath
  , ssaTitle
  , ssaVersion
  , ssaCreationDate
  , ssaCreationTime
  , ssaParentFile
  , ssaSampleTimeInterval
  , ssaUnitTimeInterval
  , ssaNumberRows
  , ssaNumberColumns
  , ssaIndexColumn
  , ssaIndexTrace
  , ssaDataTraces

  , Trace
  , traceLabel
  , traceUnit
  , traceSeries

  , parseSSA
  , loadSSA

  , printSSA
  , writeSSA
  ) where

import           Prelude                     hiding (readFile, writeFile)

import           Control.Exception           hiding (try)
import           Control.Lens                hiding (noneOf)
import           Control.Monad
import           Control.Monad.ST
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Data.Char                   (isDigit, ord)
import           Data.Double.Conversion.Text (toFixed)
import           Data.Foldable
import           Data.List                   (intercalate, intersperse,
                                              transpose)
import qualified Data.List.NonEmpty          as NE
import           Data.Monoid
import qualified Data.Set                    as S
import qualified Data.Text                   as T
import qualified Data.Text.IO                as TIO
import qualified Data.Text.Lazy              as TL
import qualified Data.Text.Lazy.Builder      as B
import qualified Data.Text.Lazy.IO           as TLIO
import qualified Data.Vector.Unboxed         as V
import qualified Data.Vector.Unboxed.Mutable as VM
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer

--------------------------------------------------------------------------------
-- .ssa file representation
--------------------------------------------------------------------------------

-- This is a 'minimal' representation of an .ssa file where fields of no use to
-- us are not parsed but stored as raw strings. These fields are recorded only
-- so that we are able to write changes back into a new .ssa file.

data SSA = SSA
  { _ssaFilePath           :: String
  , _ssaTitle              :: String
  , _ssaVersion            :: String
  , _ssaCreationDate       :: String
  , _ssaCreationTime       :: String
  , _ssaParentFile         :: String
  , _ssaSampleTimeInterval :: Double
  , _ssaUnitTimeInterval   :: String
  , _ssaNumberRows         :: Integer
  , _ssaNumberColumns      :: Integer
  , _ssaIndexColumn        :: String
  , _ssaIndexTrace         :: Trace
  , _ssaDataTraces         :: [Trace]
  } deriving (Show)

data Trace = Trace
  { _traceLabel  :: String
  , _traceUnit   :: String
  , _traceSeries :: V.Vector Double
  } deriving (Show)

makeLenses ''SSA
makeLenses ''Trace

--------------------------------------------------------------------------------
-- .ssa file specifics
--------------------------------------------------------------------------------

titleLabel              = "TITLE:"
versionLabel            = "VERSION:"
creationDateLabel       = "CREATION DATE:"
creationTimeLabel       = "CREATION TIME:"
parentFileLabel         = "PARENT FILE:"
sampleTimeIntervalLabel = "SAMPLE TIME INTERVAL:"
unitTimeIntervalLabel   = "UNIT OF TIME INTERVAL:"
numberRowsLabel         = "# OF ROW:"
numberColumnsLabel      = "# OF COLUMN:"
indexColumnLabel        = "INDEX COLUMN #:"
beginDataLabel          = "BEGIN DATA:"
endDataLabel            = "END DATA"

supportedSsaVersions :: [String]
supportedSsaVersions = ["3.00", "3.10"]

--------------------------------------------------------------------------------
-- Reading .ssa files
--------------------------------------------------------------------------------

loadSSA :: FilePath -> ExceptT String IO SSA
loadSSA filePath = do
  ssaText <- ExceptT $ catch (Right <$> TIO.readFile filePath)
                             (fmap Left . pure . show @IOException)
  withExceptT simplifyParseError
    $ ExceptT $ return $ parseSSA filePath ssaText

parseSSA :: FilePath -> T.Text -> Either (ParseError Char Void) SSA
parseSSA filePath ssaText =
  runST $ runParserT (parseSSA' filePath) filePath ssaText

--------------------------------------------------------------------------------
-- .ssa parser
--------------------------------------------------------------------------------

type ParserST s a = ParsecT Void T.Text (ST s) a

parseSSA' :: String -> ParserST s SSA
parseSSA' filePath = do
  -- Header
  title              <- headerString  titleLabel
  version            <- headerString  versionLabel
  when (not $ version `elem` supportedSsaVersions)
    $ fail $ versionErrorMessage version
  creationDate       <- headerString  creationDateLabel
  creationTime       <- headerString  creationTimeLabel
  parentFile         <- headerString  parentFileLabel
  sampleTimeInterval <- headerFloat   sampleTimeIntervalLabel
  unitTimeInterval   <- headerString  unitTimeIntervalLabel
  numberRows         <- headerInteger numberRowsLabel
  numberColumns      <- headerInteger numberColumnsLabel
  indexColumn        <- headerString  indexColumnLabel
  let rows = fromIntegral numberRows
      cols = if version == "3.00" then cols'+1 else cols'
        where cols' = fromIntegral numberColumns

  -- Trace data
  string beginDataLabel >> eol
  allLabels  <- line
  allUnits   <- line
  dataRows   <- dataBlock (rows, cols)
  extraRows  <- manyTill float6Line (string endDataLabel *> eol)
  eof

  let (timeTrace, dataTraces) = makeTraces allLabels allUnits dataRows
  return SSA { _ssaFilePath           = filePath
             , _ssaTitle              = title
             , _ssaVersion            = version
             , _ssaCreationDate       = creationDate
             , _ssaCreationTime       = creationTime
             , _ssaParentFile         = parentFile
             , _ssaSampleTimeInterval = sampleTimeInterval
             , _ssaUnitTimeInterval   = unitTimeInterval
             , _ssaNumberRows         = numberRows
             , _ssaNumberColumns      = numberColumns
             , _ssaIndexColumn        = indexColumn
             , _ssaIndexTrace         = timeTrace
             , _ssaDataTraces         = dataTraces
             }

makeTraces :: [String] -> [String] -> [V.Vector Double] -> (Trace, [Trace])
makeTraces allLabels allUnits dataRows =
  let (timeLabel': dataLabels) = allLabels
      (timeUnit' : dataUnits)  = allUnits
      (timeData  : traceData)  = dataRows
      timeTrace = Trace timeLabel' timeUnit' timeData
      dataTraces = zipWith3 Trace dataLabels dataUnits traceData
  in  (timeTrace, dataTraces)

headerField :: T.Text -> ParserST s a -> ParserST s a
headerField name p = string name *> tab *> p

headerString :: T.Text -> ParserST s String
headerString name = headerField name (manyTill anyChar eol)

headerInteger :: T.Text -> ParserST s Integer
headerInteger name = headerField name decimal <* eol

headerFloat :: T.Text -> ParserST s Double
headerFloat name = headerField name float <* eol

line :: ParserST s [String]
line = sepEndBy (many (noneOf ['\t', '\n', '\r'])) tab <* eol

dataBlock :: (Int, Int) -> ParserST s [V.Vector Double]
dataBlock (rows, cols) = do
  vs <- lift $ replicateM cols (VM.new rows)
  forM_ [0..rows-1] $ \i -> do
    xs <- float6Line
    lift $ zipWithM_ (`VM.write` i) vs xs
  lift $ mapM V.unsafeFreeze vs

float6Line :: ParserST s [Double]
float6Line = endBy float6 tab <* eol

-- This float parser is sufficient for the fixed-precision numbers found in .ssa
-- files.
float6 :: ParserST s Double
float6 = do
  sign <- optional (char '-')
  wholePart <- decimals
  _ <- char '.'
  fracPart <- decmial6
  let unsigned = 1e-6 * (fromIntegral (wholePart * 1000000 + fracPart))
  return $ maybe id (const negate) sign unsigned

decimals :: ParserST s Int
decimals = go 0 where
  go :: Int -> ParserST s Int
  go acc = ((+ 10*acc) . digitToInt <$> digitChar >>= go)
       <|> pure acc

decmial6 :: ParserST s Int
decmial6 =  do
  i1 <- digitToInt <$> digitChar
  i2 <- digitToInt <$> digitChar
  i3 <- digitToInt <$> digitChar
  i4 <- digitToInt <$> digitChar
  i5 <- digitToInt <$> digitChar
  i6 <- digitToInt <$> digitChar
  pure $ i1*100000 + i2*10000 + i3*1000 + i4*100 + i5*10 + i6

digitToInt :: Char -> Int
digitToInt c = ord c - ord '0'

--------------------------------------------------------------------------------
-- Printing .ssa files
--------------------------------------------------------------------------------

writeSSA :: FilePath -> SSA -> ExceptT String IO ()
writeSSA filePath ssa = ExceptT $ catch
  (fmap Right $ TLIO.writeFile filePath $ printSSA ssa)
  (fmap Left . pure . show @IOException)

printSSA :: SSA -> TL.Text
printSSA ssa =
  let version = ssa ^. ssaVersion
      labels  = ssa ^.  ssaIndexTrace . traceLabel
              : ssa ^.. ssaDataTraces . traverse . traceLabel
      units   = ssa ^.  ssaIndexTrace . traceUnit
              : ssa ^.. ssaDataTraces . traverse . traceUnit
      columnData =     V.toList (ssa ^.  ssaIndexTrace . traceSeries)
                 : map V.toList (ssa ^.. ssaDataTraces . traverse . traceSeries)
      numberColumns = if version == "3.00" then cols-1 else cols
        where cols = length columnData
      numberRows = length $ head columnData

      fileHeader =
        [ singleField' titleLabel              (ssa ^. ssaTitle)
        , singleField' versionLabel            (ssa ^. ssaVersion)
        , singleField' creationDateLabel       (ssa ^. ssaCreationDate)
        , singleField' creationTimeLabel       (ssa ^. ssaCreationTime)
        , singleField' parentFileLabel         (ssa ^. ssaParentFile)
        , singleField' sampleTimeIntervalLabel
            ( TL.unpack $ B.toLazyText $ printDouble
            $ ssa ^. ssaSampleTimeInterval )
        , singleField' unitTimeIntervalLabel   (ssa ^. ssaUnitTimeInterval)
        , singleField' numberRowsLabel         (show numberRows)
        , singleField' numberColumnsLabel      (show numberColumns)
        , singleField' indexColumnLabel        (ssa ^. ssaIndexColumn)
        ]

      traceDataHeader =
        [ B.fromText beginDataLabel
        , sepEndWith '\t' labels
        , sepEndWith '\t' units <> delineators
        ] where delineators = if version == "3.00"
                              then mempty
                              else B.fromString "\tDelineators"

      dataBlock = map (mconcat . map (appendTab . printDouble))
                      (transpose columnData)

      -- the extra mempty adds an extra newline
      end = [B.fromText endDataLabel, mempty]

      -- .ssa files are generated on windows so use "\r\n" for eol
      fileBuilder = mconcat $ intersperse (B.fromString "\r\n") $ concat
        [fileHeader, traceDataHeader, dataBlock, end]

  in  B.toLazyText fileBuilder

singleField' :: T.Text -> String -> B.Builder
singleField' name content =
  B.fromText name <> B.singleton '\t' <> B.fromString content

appendTab :: B.Builder -> B.Builder
appendTab b = b <> B.singleton '\t'

printDouble :: Double -> B.Builder
printDouble = B.fromText . toFixed 6

sepEndWith :: Char -> [String] -> B.Builder
sepEndWith c strs =
  mconcat $ strs >>= \s -> [B.fromString s, B.singleton c]

--------------------------------------------------------------------------------
-- Simplifying parse errors
--------------------------------------------------------------------------------

simplifyParseError :: ParseError Char Void -> String
simplifyParseError (TrivialError sourcePositions unexpected expecteds) =
  customUnlines
    [ headerMsg
    , positionMsg sourcePositions
    , unexpectedMsg unexpected
    , expectedMsg expecteds
    ]
simplifyParseError (FancyError sourcePositions fancyErrors) =
  customUnlines
    [ headerMsg
    , positionMsg sourcePositions
    , fancyMsg fancyErrors
    ]

customUnlines :: [String] -> String
customUnlines = intercalate "\n\n" . filter (not . null)

headerMsg :: String
headerMsg = "Error: Could not parse .ssa file"

positionMsg :: NE.NonEmpty SourcePos -> String
positionMsg positions =
  errorPluralized ++ (intercalate ", and " $ map positionMsg' $ toList positions) ++ ":"
  where
    errorPluralized = if length positions == 1
      then "due to an error located "
      else "due to errors located "
    positionMsg' :: SourcePos -> String
    positionMsg' sourcePos =
      concat [ "in file '"
            , sourceName sourcePos
            , "' at line "
            , show $ unPos $ sourceLine sourcePos
            , " and column "
            , show $ unPos $ sourceColumn sourcePos
            ]

unexpectedMsg :: Maybe (ErrorItem Char) -> String
unexpectedMsg = maybe "" $
  \errItem -> "Unexpected " ++ printErrorItem errItem ++ "."

expectedMsg :: S.Set (ErrorItem Char) -> String
expectedMsg expecteds =
  "Expected "
  ++ (intercalate ", or " $ fmap printErrorItem $ toList expecteds)
  ++ "."

fancyMsg :: S.Set (ErrorFancy Void) -> String
fancyMsg fancyErrors =
  customUnlines $ fmap printErrorFancy $ toList fancyErrors

printErrorItem :: ErrorItem Char -> String
printErrorItem (Tokens ts) = show $ fmap convertTabs $ toList ts
  where convertTabs :: Char -> Char
        convertTabs '\t' = ' '
        convertTabs c    = c
printErrorItem (Label cs)  = show $ toList cs
printErrorItem EndOfInput  = "end of input"

printErrorFancy :: ErrorFancy Void -> String
printErrorFancy (ErrorFail errMsg)       = errMsg
printErrorFancy (ErrorIndentation _ _ _) = "Indentation error."
-- printErrorFancy (ErrorCustom void) -- impossible

--------------------------------------------------------------------------------
-- Custom error messages
--------------------------------------------------------------------------------

versionErrorMessage :: String -> String
versionErrorMessage version = concat
  [ "Unsupported .ssa version '"
  , version
  , "' (supported .ssa versions: "
  , intercalate ", " supportedSsaVersions
  , ")."
  ]
