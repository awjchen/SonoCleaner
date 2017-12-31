{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types.IndexInterval
  ( IndexInterval
  , getEndpoints
  , fromEndpoints
  ) where

import Data.Tuple (swap)

-- IndexedIntervals are defined by their endpoints, and so are closed intervals.
newtype IndexInterval = IndexInterval { getEndpoints :: (Int, Int) }
  deriving (Eq)

fromEndpoints :: (Int, Int) -> IndexInterval
fromEndpoints pair@(a, b) =
  if a < b then IndexInterval pair else IndexInterval (swap pair)
