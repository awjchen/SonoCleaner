-- A data type to hold a sonomicrometry distance trace and some metadata

{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.TraceState
  ( TraceState
  , context, series, diffSeries, diff2Series, seriesBounds, modifiedJumps
  , TraceContext (..)

  , cropTraceState
  , uncropTraceState

  , initTraceState
  , setSeries
  , updateDiffSeries

  , TraceStateOperator
  , pattern IdOperator
  , pattern TraceStateOperator
  , idOperator
  , getOperator
  , unsafeTraceStateOperator
  ) where

import           Control.Lens
import qualified Data.IntSet         as S
import qualified Data.Vector.Unboxed as V

import qualified Types.IndexInterval as I
import           Types.Series

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

data TraceContext = RootContext | CroppedContext TraceState

data TraceState = TraceState
  { _context       :: TraceContext
  , _series        :: V.Vector Double
  , _diffSeries    :: (Double, V.Vector Double)
  , _diff2Series   :: (Double, (Double, V.Vector Double))
  , _seriesBounds  :: (Double, Double)
  , _modifiedJumps :: S.IntSet
  }
makeLenses ''TraceState

--------------------------------------------------------------------------------
-- Cropping
--------------------------------------------------------------------------------

cropTraceState :: I.IndexInterval -> TraceState -> TraceState
cropTraceState cropInterval ts =
  let croppedSeries = I.slice cropInterval (ts ^. series)
      context' = CroppedContext ts
      modifiedJumps' = S.map (subtract (I.leftEndpoint cropInterval))
                     $ S.filter (`I.elem` I.diff cropInterval)
                     $ ts ^. modifiedJumps
  in  initTraceState croppedSeries
        & context .~ context'
        & modifiedJumps .~ modifiedJumps'

uncropTraceState :: I.IndexInterval -> TraceState -> TraceState
uncropTraceState cropInterval ts = case ts ^. context of
  RootContext -> ts
  CroppedContext cts ->
    let start = I.leftEndpoint cropInterval
        ds = snd $ ts ^. diffSeries
        updates = V.zip (V.enumFromN start (V.length ds)) ds
        diffSeries' = fmap (`V.update` updates) (cts ^. diffSeries)
        newModifiedJumps = S.map (+start) (ts ^. modifiedJumps)
    in  setDiffSeries diffSeries' cts
          & modifiedJumps %~ S.union newModifiedJumps

--------------------------------------------------------------------------------
-- Utility
--------------------------------------------------------------------------------

initTraceState :: V.Vector Double -> TraceState
initTraceState series' =
  TraceState
    { _context       = RootContext
    , _series        = series'
    , _diffSeries    = diffSeries'
    , _diff2Series   = diff2Series'
    , _seriesBounds  = unboxedMinAndMax series'
    , _modifiedJumps = S.empty }
  where diffSeries'  = diff series'
        diff2Series' = fmap diff diffSeries'

setSeries :: V.Vector Double -> TraceState -> TraceState
setSeries series' =
    set seriesBounds (unboxedMinAndMax series')
  . set series      series'
  . set diffSeries  diffSeries'
  . set diff2Series diff2Series'
  where diffSeries'  = diff series'
        diff2Series' = fmap diff diffSeries'

setDiffSeries :: (Double, V.Vector Double) -> TraceState -> TraceState
setDiffSeries diffSeries' =
    set seriesBounds (unboxedMinAndMax series')
  . set series      series'
  . set diffSeries  diffSeries'
  . set diff2Series diff2Series'
  where series'      = undiff diffSeries'
        diff2Series' = fmap diff diffSeries'

updateDiffSeries :: Double -> [(Int, Double)] -> TraceState -> TraceState
updateDiffSeries offset updates traceState =
  let diffSeries'  =
        bimap (+offset) (V.// updates) (traceState ^. diffSeries)
      series'      = undiff diffSeries'
      diff2Series' = fmap diff diffSeries'
  in  setDiffSeries diffSeries' traceState
        & over modifiedJumps (S.union (S.fromList $ fst $ unzip updates))

--------------------------------------------------------------------------------
-- TraceStateOperator
--------------------------------------------------------------------------------
-- We included a special case for the identity operator for efficiency, but it
-- turns out it helps only a little. Oops.

data TraceStateOperator = RealIdOperator
                        | RealTraceStateOperator (TraceState -> TraceState)

pattern IdOperator :: TraceStateOperator
pattern IdOperator <- RealIdOperator

pattern TraceStateOperator :: (TraceState -> TraceState) -> TraceStateOperator
pattern TraceStateOperator a <- RealTraceStateOperator a

idOperator :: TraceStateOperator
idOperator = RealIdOperator

unsafeTraceStateOperator :: (TraceState -> TraceState) -> TraceStateOperator
unsafeTraceStateOperator = RealTraceStateOperator

getOperator :: TraceStateOperator -> (TraceState -> TraceState)
getOperator RealIdOperator             = id
getOperator (RealTraceStateOperator f) = f
