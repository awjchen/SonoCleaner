-- Automatic matching and elimination of level-shifts

-- For an explanation, see 'Procedural details' section of the user's guide.

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf   #-}

module Model.Matching
  ( LevelShiftMatches, matchLevels
  , applyMatches
  , matchLevelShifts
  ) where

import           Control.Lens
import           Control.Monad.Loops        (whileJust)
import           Control.Monad.ST
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Data.Default
import           Data.Function
import qualified Data.Heap                  as H
import           Data.List                  (groupBy)
import qualified Data.List.NonEmpty         as NE
import           Data.Maybe
import           Data.STRef.Strict
import qualified Data.Vector.Unboxed        as V

import           Types.Indices

import qualified Model.IndexedChain         as IC
import           Model.Slope
import           Model.TraceState

-------------------------------------------------------------------------------
-- Interface types
-------------------------------------------------------------------------------

data LevelShiftMatches = LevelShiftMatches
  { matchLevels          :: Int
  , getLevelShiftMatches :: [[(Index1, Double)]] }

levelShiftMatches :: [[(Index1, Double)]] -> LevelShiftMatches
levelShiftMatches matches = LevelShiftMatches (length matches) matches

instance Default LevelShiftMatches where
  def = levelShiftMatches []

-------------------------------------------------------------------------------
-- Internal types
-------------------------------------------------------------------------------

type GroupCount = Int
type GroupSpan  = Int -- span: difference of the indices of the first and last level-shifts
type GroupError = Double
type LevelShiftPosition = Index1
type LevelShiftError    = Double

type MatchSeedCandidate = (GroupSpan, IC.ElemIndex)

data MatchingEnv s = MatchingEnv
  { envNoiseThreshold :: Double
  , envChain          :: IC.IndexedChain s (LevelShiftPosition, LevelShiftError)
  , envHeap           :: STRef s (H.Heap MatchSeedCandidate)
  }

type Run s a = ReaderT (MatchingEnv s) (ST s) a

data Match = Match
  { matchSpan          :: GroupSpan
  , matchErrorSum      :: GroupError
  , matchedLevelShiftIndices :: [LevelShiftPosition]
  }

data ZeroSumSearchResult
  = ZeroSum LevelShiftError [LevelShiftPosition]
  | NextSpan GroupSpan
  | NoSolution

-------------------------------------------------------------------------------
-- Constants
-------------------------------------------------------------------------------

-- groupCountLimit:
-- The maximum number of level-shifts in a matching group.
groupCountLimit :: Int
groupCountLimit = 8

-- searchGroupCountLimit:
-- In order to find all matching groups, we need to consider groups larger than
-- the `groupCountLimit`, due to the implementation of the procedure.
-- Considering groups of arbitrary size leads to terrible worst-case complexity,
-- so we enforce an empirically-determined limit.
searchGroupCountLimit :: Int
searchGroupCountLimit = 2*groupCountLimit

-- interpolationLimit:
-- The time span of the level-shift group, in terms of the number of data points
-- within the span of the level-shifts (alternatively, the number of "displaced"
-- data points), at or below which the correction method applied will be linear
-- interpolation rather than the usual method of "redistribution". For example,
-- a single-point-outlier displaces one point.
interpolationLimit :: Int
interpolationLimit = 3

-------------------------------------------------------------------------------
-- Exported functions
-------------------------------------------------------------------------------

applyMatches :: LevelShiftMatches -> Int -> TraceState -> TraceState
applyMatches matches progression = updateDiffSeries 0 updates
  where updates = concat $ take progression $ getLevelShiftMatches matches

matchLevelShifts
  :: Double
  -> IIntSet Index1
  -> TraceState
  -> LevelShiftMatches
matchLevelShifts noiseTh levelShifts ts = levelShiftMatches corrections
  where
    (_, ds) = ts ^. diffSeries
    noSlopeInfo = mappend levelShifts $ ts ^. modifiedSegments
    levelShiftSlopesMap = iimFromSet estimateSlope' levelShifts where
      estimateSlope' j = estimateSlope ds noSlopeInfo radius j
        where radius = 4
    levelShiftErrorsMap = iimMapWithKey f levelShiftSlopesMap
      where f i slope = ivIndex ds i - slope

    matches = runST $
      case NE.nonEmpty (iimToList1 levelShiftErrorsMap) of
        Nothing -> return []
        Just neLevelShiftList -> do
          matchEnv <- initMatchingEnv noiseTh neLevelShiftList
          runReaderT matchLevelShifts' matchEnv

    corrections = (map.concatMap)
                    (applyCorrection (ts ^. series) levelShiftSlopesMap) matches

-------------------------------------------------------------------------------
-- Corrections on matches
-------------------------------------------------------------------------------

applyCorrection
  :: IVector Index0 Double
  -> IIntMap Index1 Double
  -> Match
  -> [(Index1, Double)]
applyCorrection v levelShifts match
  | matchSpan match <= interpolationLimit = interpolate v match
  | otherwise                             = redistribute levelShifts match

redistribute :: IIntMap Index1 Double -> Match -> [(Index1, Double)]
redistribute levelShifts (Match _ err levelShiftPositions) =
  let slopeEstimates =
        map (fromJust . flip iimLookup levelShifts) levelShiftPositions
  in  zip levelShiftPositions $ over _head (+err) slopeEstimates

interpolate :: IVector Index0 Double -> Match -> [(Index1, Double)]
interpolate v (Match groupSpan _ levelShiftPositions) =
  interpolationUpdates v (iiUndiff $ IndexInterval (i, iTranslate groupSpan i))
  where i = head levelShiftPositions

-------------------------------------------------------------------------------
-- The matching procedure
-------------------------------------------------------------------------------

initMatchingEnv
  :: Double
  -> NE.NonEmpty (LevelShiftPosition, LevelShiftError)
  -> ST s (MatchingEnv s)
initMatchingEnv noiseTh neLevelShiftsList = do
  let levelShiftsList = NE.toList neLevelShiftsList
  chain <- IC.fromList levelShiftsList
  heap <- let positions = map fst levelShiftsList
              distances = zipWith iMinus (tail positions) positions
              heapElems = zip distances [0..]
          in  newSTRef $ H.fromList heapElems
  pure MatchingEnv
    { envNoiseThreshold      = noiseTh
    , envChain               = chain
    , envHeap                = heap
    }

-- Returns matches grouped by span, which increases monotonically.
matchLevelShifts' :: Run s [[Match]]
matchLevelShifts' =
      groupBy ((==) `on` matchSpan) . catMaybes
  <$> whileJust popCandidate tryCandidate

popCandidate :: Run s (Maybe MatchSeedCandidate)
popCandidate = do
  heapRef <- asks envHeap
  mbViewMin <- lift $ fmap H.viewMin $ readSTRef heapRef
  case mbViewMin of
    Nothing        -> pure Nothing
    Just (c, heap) -> lift (writeSTRef heapRef heap) >> pure (Just c)

tryCandidate :: MatchSeedCandidate -> Run s (Maybe Match)
tryCandidate (targetSpan, targetLevelShiftID) = do
  searchResult <- searchZeroSum targetSpan targetLevelShiftID
  case searchResult of
    (NoSolution, _) -> pure Nothing
    (NextSpan nextSpan, _) -> do
      heapRef <- asks envHeap
      lift $ modifySTRef' heapRef (H.insert (nextSpan, targetLevelShiftID))
      pure Nothing
    (ZeroSum err levelShiftPositions, levelShiftIDs) -> do
      chain <- asks envChain
      lift $ do
        mapM_ (IC.remove chain) levelShiftIDs
        pure $ Just $ Match targetSpan err levelShiftPositions

-------------------------------------------------------------------------------
-- Searching for zero-sum groups of level-shifts
-------------------------------------------------------------------------------

-- Search for a contiguous interval of level-shifts starting at 'startIdx',
-- spanning exactly 'spanLimit' time units, containing at most 'groupCountLimit'
-- level-shifts, and whose displacements sum to zero (within some tolerance
-- `envNoiseThreshold`).
searchZeroSum
  :: GroupSpan -> IC.ElemIndex -> Run s (ZeroSumSearchResult, [IC.ElemIndex])
searchZeroSum spanLimit startIdx = do
  chain  <- asks envChain
  errLim <- asks envNoiseThreshold
  mbVal  <- lift $ IC.query chain startIdx
  case mbVal of
    Nothing -> pure (NoSolution, [])
    Just (startPos, _) -> lift $
      foldChainFrom (searchZeroSumHelper errLim
                                         startPos
                                         (iTranslate spanLimit startPos))
                    (const NoSolution)
                    (0, 0, [])
                    startIdx
                    chain

searchZeroSumHelper
  :: Double
  -> LevelShiftPosition
  -> LevelShiftPosition
  -> (LevelShiftPosition, LevelShiftError)
  -> (GroupCount, GroupError, [LevelShiftPosition])
  -> ST s (Either ZeroSumSearchResult (GroupCount, GroupError, [LevelShiftPosition]))
searchZeroSumHelper
  errLim startPos targetPos (pos, err) (accCount, accErr, accPos) =
  let accCount' = succ accCount
      accErr' = err + accErr
      accPos' = pos : accPos
      acc' = (accCount', accErr', accPos')
      next = pure (Right acc') in
  if  | pos < targetPos ->
          next
      | pos == targetPos ->
          if  | accCount' > searchGroupCountLimit ->
                  pure $ Left NoSolution
              | accCount' > groupCountLimit ->
                  next
              | abs (err+accErr) <= errLim ->
                  pure $ Left $ ZeroSum accErr' (reverse accPos')
              | otherwise ->
                  next
      | otherwise ->
          pure $ Left $ NextSpan (pos `iMinus` startPos)

foldChainFrom :: (V.Unbox a)
  => (a -> b -> ST s (Either c b)) -- Combining function, returning Left to terminate and Right to continue
  -> (b -> c) -- Termination function to call in case there are no more elements
  -> b -- Initial accumulator
  -> IC.ElemIndex -- Initial index
  -> IC.IndexedChain s a -- The chain
  -> ST s (c, [IC.ElemIndex]) -- Also returns a list of visited indices
foldChainFrom f g z idx chain = do
  mbVal <- IC.query chain idx
  case mbVal of Nothing -> pure (g z, [])
                Just a  -> do
                  mbAcc <- f a z
                  case mbAcc of
                    Left c  -> pure (c, [idx])
                    Right b -> foldChainFrom' f g b idx [idx] chain

-- Because 'next' obtains the both next index and its value simultaneously, we
-- use `foldChainFrom'` to avoid redundant queries. It assumes that there exists
-- an element at index 'idx' in the chain, and its value is already incorporated
-- into the accumulator 'z'.
foldChainFrom' :: (V.Unbox a)
  => (a -> b -> ST s (Either c b)) -> (b -> c) -> b -> IC.ElemIndex
  -> [IC.ElemIndex] -> IC.IndexedChain s a -> ST s (c, [IC.ElemIndex])
foldChainFrom' f g !z idx idxAcc chain = do
  mbNext <- IC.next chain idx
  case mbNext of  Nothing -> pure (g z, idxAcc)
                  Just (nextIdx, a) -> do
                    let idxAcc' = nextIdx : idxAcc
                    mbAcc <- f a z
                    case mbAcc of
                      Left c  -> pure (c, idxAcc')
                      Right b -> foldChainFrom' f g b nextIdx idxAcc' chain
