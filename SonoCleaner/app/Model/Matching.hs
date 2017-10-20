-- Automatic matching and elimination of level-shifts

-- For an explanation, see 'Procedural details' section of the user's guide.

{-# LANGUAGE MultiWayIf #-}

module Model.Matching
  ( LevelShiftMatches, matchLevels
  , matchJumpsTrace
  , matchJumps
  ) where

import           Control.Lens
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

-- [ Currently not implemented ]
-- interpolationLimit:
--   Defines the maximum time span between jumps, in terms of the ticks between
--   the start of the first jump to the end of the last, that will be collapsed
--   by linear interpolation rather than the usual method of "redistribution".
-- interpolationLimit :: Int
-- interpolationLimit = 4

-------------------------------------------------------------------------------
-- Env
-------------------------------------------------------------------------------

type ShiftSize    = Int
type JumpID       = Int
type JumpPosition = Int

type ErrorSize = Double

data MatchEnv s = MatchEnv
  { envNoiseThreshold :: Double
  , envChain          :: IC.IndexedChain s (JumpPosition, ErrorSize)
  , envHeap           :: STRef s (H.Heap (ShiftSize, JumpID))
  }

-------------------------------------------------------------------------------
-- Go
-------------------------------------------------------------------------------

type Run s a = ReaderT (MatchEnv s) (ST s) a

data LevelShiftMatches = LevelShiftMatches
  { matchLevels          :: Int
  , getLevelShiftMatches :: [[(Int, Double)]] }

instance Default LevelShiftMatches where
  def = LevelShiftMatches 0 []

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
matchJumps noiseTh jumpsMap ts = LevelShiftMatches n changes where
  (_, ds) = ts ^. diffSeries
  jumpSlopesMap = M.mapWithKey estimateSlope' jumpsMap where
    estimateSlope' k _ = fromMaybe 0 $ estimateSlope ds jumpsMap radius k
      where radius = 4
  jumpErrorsMap = M.unionWith (-) jumpsMap jumpSlopesMap
  newErrors = runST $
    case NE.nonEmpty (M.toList jumpErrorsMap) of
      Nothing -> return []
      Just jumpList -> do
        matchEnv <- initEnv noiseTh jumpList
        runReaderT matchJumps' matchEnv

  applyError :: (Int, Double) -> (Int, Double)
  applyError (k, newErr) =
    let oldErr = fromJust $ M.lookup k jumpErrorsMap
        change = newErr - oldErr
        height = fromJust $ M.lookup k jumpsMap
    in  (k, height + change)

  changes = (map.map) applyError newErrors
  n = length changes

-- Initialize the matching procedure.
initEnv
  :: Double
  -> NE.NonEmpty (Int, ErrorSize)
  -> ST s (MatchEnv s)
initEnv noiseTh neJumpsList = do
  let jumpsList = NE.toList neJumpsList
  chain <- IC.fromList jumpsList
  heap <- let positions = map fst jumpsList
              distances = zipWith (-) (tail positions) positions
              heapElems = zip distances [0..] :: [(ShiftSize, JumpID)]
          in  newSTRef $ H.fromList heapElems
  return MatchEnv
    { envNoiseThreshold      = noiseTh
    , envChain               = chain
    , envHeap                = heap
    }

-- Aggregate matches of the same size.
matchJumps' :: Run s [[(Int, Double)]]
matchJumps' = fmap (map (concatMap snd) . groupBy ((==) `on` fst)) matchJumps''

-- Loop until done.
matchJumps'' :: Run s [(Int, [(Int, Double)])]
matchJumps'' = do
  isDone <- done
  if isDone
    then return []
    else do
      mMatch <- step
      case mMatch of
        Nothing    -> matchJumps''
        Just match -> (match:) <$> matchJumps''

-- We are done when the heap of candidates is empty.
done :: Run s Bool
done = fmap null $ asks envHeap >>= lift . readSTRef

-- Try the next candidate.
step :: Run s (Maybe (ShiftSize, [(Int, Double)]))
step = do
  heapRef <- asks envHeap
  heap <- lift $ readSTRef heapRef
  let (size, jumpID) = H.minimum heap
      heap' = H.deleteMin heap

  mSum <- searchZeroSum size jumpID
  case mSum of
    NoSolution -> do
      lift $ writeSTRef heapRef heap'
      return Nothing
    NextSize nextSize -> do
      let heap'' = heap' & H.insert (nextSize, jumpID)
      lift $ writeSTRef heapRef heap''
      return Nothing
    ZeroSum err jumpIDs -> do
      chain <- asks envChain
      (j0:js) <- lift $ do
        writeSTRef heapRef heap'
        positions <- fmap (reverse . map fst . catMaybes)
          $ mapM (IC.query chain) $ toList jumpIDs
        mapM_ (IC.remove chain) jumpIDs
        return positions
      return $ Just (size, (j0, err):zip js (repeat 0))

-- Search for a contiguous interval of jumps starting at 'startID', spanning
-- exactly 'sizeLimit' time units, containing at most 'groupSizeLimit' jumps,
-- and whose displacements sum to zero (within some tolerance).
data SearchResult = ZeroSum Double (NE.NonEmpty JumpID)
                  | NextSize ShiftSize
                  | NoSolution

searchZeroSum :: ShiftSize -> JumpID -> Run s SearchResult
searchZeroSum sizeLimit startID = do
  chain <- asks envChain
  mVal <- lift $ IC.query chain startID
  case mVal of
    Nothing -> return NoSolution
    Just (startPos, startErr) ->
      tryNext 1 startErr (startID NE.:| []) where
        posTarget = startPos + sizeLimit

        tryNext
          :: Int
          -> Double
          -> NE.NonEmpty JumpID
          -> Run s SearchResult
        tryNext accCount accErr accIDs@(jumpID NE.:| _) = do
            chain' <- asks envChain
            mNext <- lift $ IC.next chain' jumpID
            case mNext of
              Nothing -> return NoSolution
              Just (nextID, (pos, err)) ->
                if  | pos < posTarget ->
                        tryNext (succ accCount) (err+accErr)
                                (nextID `NE.cons` accIDs)
                    | pos == posTarget -> do
                        errLim <- asks envNoiseThreshold
                        if  | accCount >= searchGroupSizeLimit ->
                              return NoSolution
                            | accCount >= groupSizeLimit ->
                              targetFollowingJump startPos nextID
                            | abs (err+accErr) <= errLim ->
                              return $
                                ZeroSum (err+accErr) (nextID `NE.cons` accIDs)
                            | otherwise ->
                              targetFollowingJump startPos nextID
                    | otherwise -> return $ NextSize (pos - startPos)

targetFollowingJump :: JumpPosition -> JumpID -> Run s SearchResult
targetFollowingJump startPos jumpID = do
  chain <- asks envChain
  mNext <- lift $ IC.next chain jumpID
  case mNext of
    Nothing -> return NoSolution
    Just (_, (nextPos, _)) ->
      return $ NextSize (nextPos - startPos)
