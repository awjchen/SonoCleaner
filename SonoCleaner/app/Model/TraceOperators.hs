module Model.TraceOperators
  ( TraceOperator (IdOperator)
  , isIdentityOp
  , getOp

  , interpolateGapsOp

  , zeroJumpOp
  , estimateSlopeBothOp

  , interpolateBetweenJumpsOp
  , matchGroupOp

  , applyMatchesOp
  ) where

import qualified Data.IntMap.Strict as M

import           Model.Gaps
import           Model.Jumps
import           Model.Matching
import           Model.TraceState

import           Types.LevelShifts

--------------------------------------------------------------------------------

-- TraceOperators are "blessed" transformations (TraceState -> TraceState),
-- being the only ones allowed to act upon the data in the Model.
data TraceOperator
  = IdOperator
  | TraceOperator (TraceState -> TraceState)

isIdentityOp :: TraceOperator -> Bool
isIdentityOp IdOperator = True
isIdentityOp _          = False

getOp :: TraceOperator -> TraceState -> TraceState
getOp IdOperator        = id
getOp (TraceOperator f) = f

-- Gaps
interpolateGapsOp
  :: LabelledStratum
  -> (Int, Int)
  -> (Double, Double)
  -> TraceOperator
interpolateGapsOp = (fmap.fmap.fmap) TraceOperator interpolateGaps

-- Jumps: single
zeroJumpOp :: Hold -> Double -> Int -> M.IntMap Double -> TraceOperator
zeroJumpOp = (fmap.fmap.fmap.fmap) TraceOperator zeroJump

estimateSlopeBothOp :: Hold -> Double -> Int -> M.IntMap Double -> TraceOperator
estimateSlopeBothOp = (fmap.fmap.fmap.fmap) TraceOperator estimateSlopeBoth

-- Jumps: multiple
interpolateBetweenJumpsOp :: Double -> [Int] -> M.IntMap Double -> TraceOperator
interpolateBetweenJumpsOp = (fmap.fmap.fmap) TraceOperator interpolateBetweenJumps

matchGroupOp :: Double -> [Int] -> M.IntMap Double -> TraceOperator
matchGroupOp = (fmap.fmap.fmap) TraceOperator matchGroup

-- Matching
applyMatchesOp :: LevelShiftMatches -> Int -> TraceOperator
applyMatchesOp = (fmap.fmap) TraceOperator applyMatches
