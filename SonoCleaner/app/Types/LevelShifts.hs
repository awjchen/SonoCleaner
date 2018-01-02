-- Types for manual corrections to level-shifts

module Types.LevelShifts
  ( Hold (..)
  , SingleAction (..)
  , MultipleAction (..)
  ) where

-------------------------------------------------------------------------------
-- Single level-shifts
-------------------------------------------------------------------------------

data Hold = HoldLeft | HoldRight
  deriving (Eq, Enum, Bounded)

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
