-- Searching the keys of an IntMap

module Types.IntMap where

import           Control.Applicative
import           Control.Monad
import qualified Data.IntMap.Strict  as M
import qualified Data.IntSet         as S

--------------------------------------------------------------------------------

findNearestIndex :: Int -> M.IntMap a -> Maybe Int
findNearestIndex target intMap = nearest <|> lower <|> upper where
  lower = fst <$> M.lookupLE target intMap
  upper = fst <$> M.lookupGE target intMap
  nearest = do
    l <- lower
    u <- upper
    if abs (l - target) <= abs (u - target)
      then pure l else pure u

findIntermediateIndices :: Num a => (Int, Int) -> M.IntMap a -> Maybe [Int]
findIntermediateIndices (low, high) intMap = do
  lowIndex  <- fst <$> M.lookupGE low  intMap
  highIndex <- fst <$> M.lookupLE high intMap
  guard (lowIndex <= highIndex)
  pure $ M.keys $ boundIntMap (lowIndex, highIndex) intMap

boundIntMap :: (Int, Int) -> M.IntMap a -> M.IntMap a
boundIntMap (low, high) =
  fst . M.split (succ high) . snd . M.split (pred low)

boundIntSet :: (Int, Int) -> S.IntSet -> S.IntSet
boundIntSet (low, high) =
  fst . S.split (succ high) . snd . S.split (pred low)
