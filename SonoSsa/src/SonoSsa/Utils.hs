{-# LANGUAGE PatternGuards #-}

module SonoSsa.Utils
  ( channelLabels
  , channelIndex
  , channelData
  , ticksFromSeconds

  , trxDataTraces
  )
  where

import           Control.Arrow       (second)
import           Control.Lens
import           Data.List           (elemIndex, stripPrefix)
import qualified Data.Map.Strict     as M
import           Data.Maybe          (mapMaybe)
import           Data.Tuple          (swap)
import qualified Data.Vector.Unboxed as V

import           SonoSsa.Ssa

--------------------------------------------------------------------------------

channelLabels :: Traversal' SSA String
channelLabels = ssaDataTraces . traverse . traceLabel

channelIndex :: String -> SSA -> Either String Int
channelIndex chLabel ssa =
  case elemIndex chLabel (ssa ^.. channelLabels) of
    Just i  -> Right i
    Nothing -> Left $ "No sono. channel with label '" ++ chLabel ++ "'"

channelData :: String -> SSA -> Either String (V.Vector Double)
channelData chLabel ssa = do
  i <- channelIndex chLabel ssa
  return $ ssa ^. ssaDataTraces . ix i . traceSeries

ticksFromSeconds :: SSA -> Double -> Int
ticksFromSeconds ssa t = floor $ t / ssa ^. ssaSampleTimeInterval

--------------------------------------------------------------------------------
-- Extract TRX traces by crystal number
--------------------------------------------------------------------------------

readTRX :: String -> Maybe (Int, Int)
readTRX str | Just suffix <- stripPrefix "TRX" str
            , [t1, t2, ':', r1, r2] <- suffix
            = Just (read [t1, t2], read [r1, r2])
            | otherwise = Nothing

trxDataTraces :: SSA -> M.Map (Int, Int) (V.Vector Double)
trxDataTraces ssa =
  let traces     = ssa ^. ssaDataTraces
      labels     = map (view traceLabel) traces
      dataTraces = map (view traceSeries) traces
  in    M.fromList $ map swap
      $ mapMaybe (sequence . second readTRX) $ zip dataTraces labels
