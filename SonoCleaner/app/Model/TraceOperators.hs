-- TraceOperators are "blessed" transformations (TraceState -> TraceState),
-- being the only ones allowed to act upon the data in the Model.

module Model.TraceOperators
  ( TraceOperator (IdOperator)
  , isIdentityOp
  , getOp

  , interpolationBrushOp

  , setZeroOp
  , setMedianOp

  , interpolateGroupOp
  , matchGroupOp

  , applyMatchesOp
  ) where

import           Model.InterpolationBrush
import           Model.Manual
import           Model.Matching
import           Model.TraceState

import           Types.Indices
import           Types.LevelShifts

--------------------------------------------------------------------------------

data TraceOperator
  = IdOperator
  | TraceOperator (TraceState -> TraceState)

isIdentityOp :: TraceOperator -> Bool
isIdentityOp IdOperator = True
isIdentityOp _          = False

getOp :: TraceOperator -> TraceState -> TraceState
getOp IdOperator        = id
getOp (TraceOperator f) = f

-- Interpolation brush
interpolationBrushOp
  :: ReplaceDataAboveOrBelow
  -> IndexInterval Index0
  -> (Double, Double)
  -> TraceOperator
interpolationBrushOp = (fmap.fmap.fmap) TraceOperator interpolationBrush

-- Manual correction: single
setZeroOp :: Hold -> Double -> Index1 -> IIntMap Index1 Double -> TraceOperator
setZeroOp = (fmap.fmap.fmap.fmap) TraceOperator setZero

setMedianOp
  :: Hold -> Double -> Index1 -> IIntMap Index1 Double -> TraceOperator
setMedianOp = (fmap.fmap.fmap.fmap) TraceOperator setMedianSlope

-- Manual correction: multiple
interpolateGroupOp
  :: Double -> [Index1] -> IIntMap Index1 Double -> TraceOperator
interpolateGroupOp = (fmap.fmap.fmap) TraceOperator interpolateGroup

matchGroupOp :: Double -> [Index1] -> IIntMap Index1 Double -> TraceOperator
matchGroupOp = (fmap.fmap.fmap) TraceOperator matchGroup

-- Automated correction
applyMatchesOp :: LevelShiftMatches -> Int -> TraceOperator
applyMatchesOp = (fmap.fmap) TraceOperator applyMatches
