{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types.IndexInterval
  ( IndexInterval
  , getEndpoints
  , leftEndpoint

  , fromEndpoints

  , elem

  , slice

  , diff
  , undiff
  ) where

import           Prelude             hiding (elem)

import           Data.Tuple          (swap)
import qualified Data.Vector.Unboxed as VU

-- IndexedIntervals represent contiguous intervals of integers that represent
-- indices into time series. An IndexedInterval a b can be thought of as
-- [a..b]. The purpose of this type is to avoid off-by-one errors caused by an
-- unclear convention as to whether the right endpoint is open or closed (we
-- have chosen it to be closed).
newtype IndexInterval = IndexInterval { getEndpoints :: (Int, Int) }
  deriving (Eq)

leftEndpoint :: IndexInterval -> Int
leftEndpoint = fst . getEndpoints

fromEndpoints :: (Int, Int) -> IndexInterval
fromEndpoints pair@(a, b) =
  if a < b then IndexInterval pair else IndexInterval (swap pair)
                                        --
-- Whether an index (of the time series) lies within the interval.
elem :: Int -> IndexInterval -> Bool
elem i (IndexInterval (i0, i1)) = i0 <= i && i <= i1

slice :: IndexInterval -> VU.Vector Double -> VU.Vector Double
slice (IndexInterval (i, j)) = VU.slice i (j-i+1)

--------------------------------------------------------------------------------
-- Conversions from indices into a series to its 'diff' series
--------------------------------------------------------------------------------

-- The interval of segments the indices of the original interval.
diff :: IndexInterval -> IndexInterval
diff (IndexInterval (a, b)) = IndexInterval (a, pred b)

-- The inverse of `diff`.
undiff :: IndexInterval -> IndexInterval
undiff (IndexInterval (a, b)) = IndexInterval (a, succ b)
