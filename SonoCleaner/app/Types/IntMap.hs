-- Searching the keys of an IntMap

module Types.IntMap where

import           Control.Applicative
import           Control.Monad
import qualified Data.IntMap.Strict  as M

--------------------------------------------------------------------------------

findNearestIndex :: Int -> M.IntMap a -> Maybe Int
findNearestIndex target intMap = index where
  lower = fst <$> M.lookupLE target intMap
  upper = fst <$> M.lookupGE target intMap
  nearest = do
    l <- lower
    u <- upper
    if abs (l - target) <= abs (u - target)
      then pure l else pure u
  index = nearest <|> lower <|> upper

findIntermediateIndices :: Num a => (Int, Int) -> M.IntMap a -> Maybe [Int]
findIntermediateIndices (low, high) intMap = do
  lowIndex  <- fst <$> M.lookupGE low  intMap
  highIndex <- fst <$> M.lookupLE high intMap
  guard (lowIndex <= highIndex)
  -- M.split excludes the split point so we must reintroduce them
  let removeLower =  M.insert lowIndex  0 . snd . M.split lowIndex
      removeUpper =  M.insert highIndex 0 . fst . M.split highIndex
      subIndices = fmap fst $ M.toList $ removeUpper $ removeLower intMap
  pure subIndices
