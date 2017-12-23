-- Searching the keys of an IntMap

module Types.IntMap where

import           Control.Applicative
import           Control.Monad
import qualified Data.IntMap.Strict  as M

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
  -- M.split excludes the split point so we use pred and succ here
  let removeLower =  snd . M.split (pred lowIndex)
      removeUpper =  fst . M.split (succ highIndex)
      intermediateIndices = M.keys $ removeUpper $ removeLower intMap
  pure intermediateIndices
