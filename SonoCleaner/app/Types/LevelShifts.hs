-- Types for manual corrections to level-shifts

module Types.LevelShifts where

import qualified Data.IntMap         as M
import qualified Data.Vector.Unboxed as V

-------------------------------------------------------------------------------
-- Single level-shifts
-------------------------------------------------------------------------------

data Hold = HoldLeft | HoldRight
  deriving (Eq, Enum, Bounded)

type SingleShift = V.Vector Double -> M.IntMap Double -> Int -> Double -> Maybe Double

data SingleAction
  = SingleIgnore
  | SingleZero
  | SingleSlopeFit
  deriving (Eq, Enum, Bounded)

-------------------------------------------------------------------------------
-- Multiple level-shifts
-------------------------------------------------------------------------------

data MultipleAction
  = MultipleIgnore
  | MultipleLine
  | MultipleCancel
  deriving (Eq, Enum, Bounded)
