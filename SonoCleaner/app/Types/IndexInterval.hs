{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types.IndexInterval
  ( IndexInterval
  , getEndpoints
  , leftEndpoint

  , fromEndpoints

  , translate
  , grow
  , elem

  , slice
  , interpolationUpdates

  , diff
  , undiff
  ) where

import           Prelude             hiding (elem)

import           Data.Tuple          (swap)
import qualified Data.Vector.Unboxed as VU

-- An IndexInterval represents a contiguous interval of indices.
-- (IndexedInterval (a, b)) means [a..b]. The purpose of this type is to avoid
-- off-by-one errors caused by an unclear convention as to whether the right
-- endpoint is open or closed (we have made it closed).
newtype IndexInterval = IndexInterval { getEndpoints :: (Int, Int) }
  deriving (Eq)

leftEndpoint :: IndexInterval -> Int
leftEndpoint = fst . getEndpoints

fromEndpoints :: (Int, Int) -> IndexInterval
fromEndpoints pair@(a, b) =
  if a < b then IndexInterval pair else IndexInterval (swap pair)

translate :: Int -> IndexInterval -> IndexInterval
translate shift (IndexInterval (a, b)) = IndexInterval (a + shift, b + shift)

grow :: IndexInterval -> IndexInterval
grow (IndexInterval (a, b)) = IndexInterval (pred a, succ b)

elem :: Int -> IndexInterval -> Bool
elem i (IndexInterval (i0, i1)) = i0 <= i && i <= i1

slice :: IndexInterval -> VU.Vector Double -> VU.Vector Double
slice (IndexInterval (i, j)) = VU.slice i (j-i+1)

toList :: IndexInterval -> [Int]
toList (IndexInterval (a, b)) = [a..b]

interpolationUpdates :: IndexInterval -> (Double, Double) -> [(Int, Double)]
interpolationUpdates interval (y0, y1) =
  let xSpan = uncurry subtract $ getEndpoints interval
      ySpan = y1 - y0
      avgSlope = ySpan / fromIntegral xSpan
  in  zip (toList $ diff interval) (repeat avgSlope)

--------------------------------------------------------------------------------
-- Conversions between indices into a series and its "derivative"
--------------------------------------------------------------------------------

-- The interval of segments lying between the indices of the original interval.
diff :: IndexInterval -> IndexInterval
diff (IndexInterval (a, b)) = IndexInterval (a, pred b)

-- The inverse of `diff`.
undiff :: IndexInterval -> IndexInterval
undiff (IndexInterval (a, b)) = IndexInterval (a, succ b)
