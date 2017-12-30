-- Automatic matching and elimination of level-shifts

-- For an explanation, see 'Procedural details' section of the user's guide.

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf   #-}

module Model.Matching
  ( LevelShiftMatches, matchLevels
  , matchJumpsTrace
  , matchJumps
  ) where

import           Control.Arrow              ((&&&))
import           Control.Lens
import           Control.Monad.Loops        (whileJust)
import           Control.Monad.ST
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Data.Default
import           Data.Foldable
import           Data.Function
import qualified Data.Heap                  as H
import qualified Data.IntMap.Strict         as M
import qualified Data.IntSet                as S
import           Data.List                  (groupBy)
import qualified Data.List.NonEmpty         as NE
import           Data.Maybe
import           Data.STRef.Strict
import qualified Data.Vector.Unboxed        as V

import qualified Model.IndexedChain         as IC
import           Model.TraceState
import           Model.Util

import           Types.Series

-- Note: In this module, level-shifts are referred to as "jumps".

-------------------------------------------------------------------------------
-- Interface types
-------------------------------------------------------------------------------

data LevelShiftMatches = LevelShiftMatches
  { matchLevels          :: Int
  , getLevelShiftMatches :: [[(Int, Double)]] }

instance Default LevelShiftMatches where
  def = LevelShiftMatches 0 []

-------------------------------------------------------------------------------
-- Internal types
-------------------------------------------------------------------------------

type GroupSize    = Int
type GroupSpan    = Int -- index of last jump minus index of first
type GroupError   = Double
type JumpPosition = Int
type JumpError    = Double

type MatchSeedCandidate = (GroupSpan, IC.ElemIndex)

-- Holds state as well, since we are in the ST monad
data MatchingEnv s = MatchingEnv
  { envNoiseThreshold :: Double
  , envChain          :: IC.IndexedChain s (JumpPosition, JumpError)
  , envHeap           :: STRef s (H.Heap MatchSeedCandidate)
  }

type Run s a = ReaderT (MatchingEnv s) (ST s) a

data Match = Match
  { matchSpan          :: GroupSpan
  , matchErrorSum      :: GroupError
  , matchedJumpIndices :: [JumpPosition]
  }

data ZeroSumSearchResult
  = ZeroSum JumpError [JumpPosition]
  | NextSpan GroupSpan
  | NoSolution

-------------------------------------------------------------------------------
-- Constants
-------------------------------------------------------------------------------

-- groupSizeLimit:
--   The maximum number of jumps in a jump group.
groupSizeLimit :: Int
groupSizeLimit = 8

-- searchGroupSizeLimit:
--   The maximum number of jumps to consider when searching for a jump group.
--   (This is a vague description, sorry. Examine its use in the code.)
searchGroupSizeLimit :: Int
searchGroupSizeLimit = 2*groupSizeLimit

-- interpolationLimit:
--   The time span of the jump group, in terms of the number of data points
--   within the span of the jumps (alternatively, the number of "displaced"
--   data points), at or below which the correction method applied will be
--   linear interpolation rather than the usual method of "redistribution".
--   For example, a single-point-outlier displaces one point.
interpolationLimit :: Int
interpolationLimit = 3

-------------------------------------------------------------------------------
-- Exported functions
-------------------------------------------------------------------------------

matchJumpsTrace :: LevelShiftMatches -> Int -> TraceStateOperator
matchJumpsTrace matches progression =
  let changes = concat $ take progression $ getLevelShiftMatches matches
  in  if null changes then idOperator else
        unsafeTraceStateOperator $ \traceState -> traceState
          & setDiffSeries (fmap (V.// changes) (traceState ^. diffSeries))
          & modifiedJumps %~ S.union (S.fromList $ map fst changes)

matchJumps
  :: Double
  -> M.IntMap Double
  -> TraceState
  -> LevelShiftMatches
matchJumps noiseTh jumpsMap ts =
  LevelShiftMatches (length corrections) corrections
  where
    (_, ds) = ts ^. diffSeries
    jumpSlopesMap = M.mapWithKey estimateSlope' jumpsMap where
      estimateSlope' k _ = estimateSlope ds jumpsMap radius k
        where radius = 4
    jumpErrorsMap = M.unionWith (-) jumpsMap jumpSlopesMap

    matches = runST $
      case NE.nonEmpty (M.toList jumpErrorsMap) of
        Nothing -> return []
        Just neJumpList -> do
          matchEnv <- initMatchingEnv noiseTh neJumpList
          runReaderT matchJumps' matchEnv

    corrections =
      (map.concatMap) (applyCorrection (ts ^. series) jumpSlopesMap) matches

-------------------------------------------------------------------------------
-- Corrections on matches
-------------------------------------------------------------------------------

applyCorrection
  :: V.Vector Double -> M.IntMap Double -> Match -> [(Int, Double)]
applyCorrection v jumps match
  -- matchSpan gives the number of jumps
  | matchSpan match <= interpolationLimit = interpolate v match
  | otherwise = redistribute jumps match

redistribute :: M.IntMap Double -> Match -> [(Int, Double)]
redistribute jumps (Match span err indices) =
  let indices' = toList indices
      slopeEstimates = map (fromJust . flip M.lookup jumps) indices'
  in  zip indices' $ over _head (+err) slopeEstimates

interpolate :: V.Vector Double -> Match -> [(Int, Double)]
interpolate v (Match span err indices) =
  let slopeInterval = (head &&& last) indices
      pointInterval = outerInterval slopeInterval
      xSpan = uncurry subtract pointInterval :: Int
      ySpan = uncurry subtract $ over both (v V.!) pointInterval :: Double
      avgSlope = ySpan / fromIntegral xSpan
  in  zip (uncurry enumFromTo slopeInterval) (repeat avgSlope)

-------------------------------------------------------------------------------
-- The matching procedure
-------------------------------------------------------------------------------

initMatchingEnv
  :: Double
  -> NE.NonEmpty (JumpPosition, JumpError)
  -> ST s (MatchingEnv s)
initMatchingEnv noiseTh neJumpsList = do
  let jumpsList = NE.toList neJumpsList
  chain <- IC.fromList jumpsList
  heap <- let positions = map fst jumpsList
              distances = zipWith (-) (tail positions) positions
              heapElems = zip distances [0..]
          in  newSTRef $ H.fromList heapElems
  pure MatchingEnv
    { envNoiseThreshold      = noiseTh
    , envChain               = chain
    , envHeap                = heap
    }

-- Returns matches grouped by (increasing) span.
matchJumps' :: Run s [[Match]]
matchJumps' =
      groupBy ((==) `on` matchSpan) . catMaybes
  <$> whileJust popCandidate tryCandidate

popCandidate :: Run s (Maybe MatchSeedCandidate)
popCandidate = do
  heapRef <- asks envHeap
  mbViewMin <- lift $ fmap H.viewMin $ readSTRef heapRef
  case mbViewMin of
    Nothing        -> pure Nothing
    Just (c, heap) -> lift (writeSTRef heapRef heap) >> pure (Just c)

-- Try the next candidate
tryCandidate :: MatchSeedCandidate -> Run s (Maybe Match)
tryCandidate (size, jumpID) = do
  searchResult <- searchZeroSum size jumpID
  case searchResult of
    (NoSolution, _) -> pure Nothing
    (NextSpan nextSize, _) -> do
      heapRef <- asks envHeap
      lift $ modifySTRef' heapRef (H.insert (nextSize, jumpID))
      pure Nothing
    (ZeroSum err revJumpPositions, jumpIDs) -> do
      chain <- asks envChain
      lift $ do
        mapM_ (IC.remove chain) jumpIDs
        pure $ Just $ Match size err (reverse revJumpPositions)

-------------------------------------------------------------------------------
-- Searching for zero-sum groups of jumps
-------------------------------------------------------------------------------

-- Search for a contiguous interval of jumps starting at 'startIdx', spanning
-- exactly 'sizeLimit' time units, containing at most 'groupSizeLimit' jumps,
-- and whose displacements sum to zero (within some tolerance).
searchZeroSum
  :: GroupSpan -> IC.ElemIndex -> Run s (ZeroSumSearchResult, [IC.ElemIndex])
searchZeroSum sizeLimit startIdx = do
  chain  <- asks envChain
  errLim <- asks envNoiseThreshold
  mbVal  <- lift $ IC.query chain startIdx
  case mbVal of
    Nothing -> pure (NoSolution, [])
    Just (startPos, startErr) -> lift $
      foldChainFrom (searchZeroSumHelper errLim startPos (startPos+sizeLimit))
                    (const NoSolution)
                    (0, 0, [])
                    startIdx
                    chain

searchZeroSumHelper
  :: Double
  -> JumpPosition
  -> JumpPosition
  -> (JumpPosition, JumpError)
  -> (GroupSize, GroupError, [JumpPosition])
  -> ST s (Either ZeroSumSearchResult (GroupSize, GroupError, [JumpPosition]))
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
          if  | accCount' > searchGroupSizeLimit ->
                  pure $ Left NoSolution
              | accCount' > groupSizeLimit ->
                  next
              | abs (err+accErr) <= errLim ->
                  pure $ Left $ ZeroSum accErr' accPos'
              | otherwise ->
                  next
      | otherwise ->
          pure $ Left $ NextSpan (pos - startPos)

foldChainFrom :: (V.Unbox a)
  -- Combining function, returning Left to terminate and Right to continue
  => (a -> b -> ST s (Either c b))
  -- Termination function, in case there are no more elements
  -> (b -> c)
  -- Initial accumulator
  -> b
  -- Initial index
  -> IC.ElemIndex
  -- The chain
  -> IC.IndexedChain s a
  -- Also returns a list of visited indices
  -> ST s (c, [IC.ElemIndex])
foldChainFrom f g z idx chain = do
  mbVal <- IC.query chain idx
  case mbVal of Nothing -> pure (g z, [])
                Just a  -> do
                  mbAcc <- f a z
                  case mbAcc of
                    Left c  -> pure (c, [idx])
                    Right b -> foldChainFrom' f g b idx [idx] chain
  where
    -- Because 'next' obtains the both next index and its value simultaneously,
    -- to avoid redundant queries we use the following invariant:
    -- there exists an element at index 'idx' in the chain,
    -- and its value is already incorporated into the accumulator 'z'.
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
