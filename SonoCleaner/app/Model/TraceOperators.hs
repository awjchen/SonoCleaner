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

import           Model.Gaps
import           Model.Jumps
import           Model.Matching
import           Model.TraceState

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
interpolateGapsOp = (fmap.fmap.fmap) TraceOperator interpolateGaps

-- Jumps: single
zeroJumpOp          = (fmap.fmap.fmap.fmap) TraceOperator zeroJump
estimateSlopeBothOp = (fmap.fmap.fmap.fmap) TraceOperator estimateSlopeBoth

-- Jumps: multiple
interpolateBetweenJumpsOp = (fmap.fmap.fmap) TraceOperator interpolateBetweenJumps
matchGroupOp              = (fmap.fmap.fmap) TraceOperator matchGroup

-- Matching
applyMatchesOp = (fmap.fmap) TraceOperator applyMatches
