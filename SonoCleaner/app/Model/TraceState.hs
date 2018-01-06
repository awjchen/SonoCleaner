-- A data type to hold a sonomicrometry distance trace and some metadata

{-# LANGUAGE TemplateHaskell #-}

module Model.TraceState
  ( TraceState
  , context, series, diffSeries, diff2Series, seriesBounds, modifiedSegments
  , TraceContext (..)

  , cropTraceState
  , uncropTraceState

  , initTraceState
  , updateDiffSeries
  ) where

import           Control.Arrow (first)
import           Control.Lens

import           Types.Indices

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

data TraceContext = RootContext | CroppedContext TraceState

data TraceState = TraceState
  { _context          :: TraceContext
  , _series           :: IVector Index0 Double
  , _diffSeries       :: (Double, IVector Index1 Double)
  , _diff2Series      :: (Double, (Double, IVector Index2 Double))
  , _seriesBounds     :: (Double, Double)
  , _modifiedSegments :: IIntSet Index1
  }
makeLenses ''TraceState

--------------------------------------------------------------------------------
-- Cropping
--------------------------------------------------------------------------------

cropTraceState :: IndexInterval Index0 -> TraceState -> TraceState
cropTraceState cropInterval ts =
  let croppedSeries = unsafeIvSlice cropInterval (ts ^. series)
      context' = CroppedContext ts
      offset = unsafeRunIndex0 $ iiLeft cropInterval
      modifiedSegments' =
          iisTranslate (-offset)
        $ iisFilter (`iiMember` iiDiff cropInterval)
        $ ts ^. modifiedSegments
  in  initTraceState croppedSeries
        & context .~ context'
        & modifiedSegments .~ modifiedSegments'

uncropTraceState :: IndexInterval Index0 -> TraceState -> TraceState
uncropTraceState cropInterval ts = case ts ^. context of
  RootContext -> ts
  CroppedContext cts ->
    let start = unsafeRunIndex0 $ iiLeft cropInterval
        ds = snd $ ts ^. diffSeries
        updates = ivMap (first (iTranslate start)) $ ivIndexed1 ds
        diffSeries' = fmap (`ivUpdate` updates) (cts ^. diffSeries)
        modifiedSegments' = iisTranslate start (ts ^. modifiedSegments)
    in  setDiffSeries diffSeries' cts
          & modifiedSegments %~ mappend modifiedSegments'

--------------------------------------------------------------------------------
-- Construction
--------------------------------------------------------------------------------

initTraceState :: IVector Index0 Double -> TraceState
initTraceState series' =
  TraceState
    { _context          = RootContext
    , _series           = series'
    , _diffSeries       = diffSeries'
    , _diff2Series      = diff2Series'
    , _seriesBounds     = ivMinMax series'
    , _modifiedSegments = mempty }
  where diffSeries'  = ivDiff series'
        diff2Series' = fmap ivDiff diffSeries'

--------------------------------------------------------------------------------
-- Modification
--------------------------------------------------------------------------------

setDiffSeries :: (Double, IVector Index1 Double) -> TraceState -> TraceState
setDiffSeries diffSeries' traceState =
  traceState
    { _series       = series'
    , _diffSeries   = diffSeries'
    , _diff2Series  = diff2Series'
    , _seriesBounds = (ivMinMax series')
    } where series'      = ivUndiff diffSeries'
            diff2Series' = fmap ivDiff diffSeries'

updateDiffSeries :: Double -> [(Index1, Double)] -> TraceState -> TraceState
updateDiffSeries offset updates traceState =
  let diffSeries'  =
        bimap (+offset) (*// updates) (traceState ^. diffSeries)
  in  setDiffSeries diffSeries' traceState
        & over modifiedSegments (mappend (iisFromList1 $ fst $ unzip updates))
