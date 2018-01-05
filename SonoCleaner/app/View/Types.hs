module View.Types where

import           Data.Colour         (AlphaColour)
import           Data.Default

import           Types.Indices

--------------------------------------------------------------------------------
-- Specification of the display
--------------------------------------------------------------------------------

data ChartSpec = ChartSpec
  { plotTitle            :: String
  , plotTitleColour      :: AlphaColour Double
  , plotSeries           :: IVector Index0 Double
  , plotJumpIndices      :: IIntSet Index1
  , plotModifiedIndices  :: IIntSet Index1
  , plotOriginalSeries   :: Maybe (IVector Index0 Double)
  , plotTwinSeries       :: Maybe (IVector Index0 Double)
  , plotCustomSeries     :: Maybe (IVector Index0 Double)
  , plotHighlightRegion  :: Maybe (Double, Double)
  , plotXRange           :: (Double, Double)
  , plotYRange           :: (Double, Double)
  , plotBackgroundColour :: AlphaColour Double
  , plotAnnotation       :: Maybe (Double, Double, String)
  , plotTimes            :: IVector Index0 Double
  , plotTimeStep         :: Double
  , plotToTime           :: Index0 -> Double
  , plotToIndex          :: Double -> Index0
  }

-------------------------------------------------------------------------------
-- Specification of the set of traces to display
-------------------------------------------------------------------------------

data TraceSet = TraceSet
  { showOriginal :: Bool
  , showTwin     :: Bool
  }

instance Default TraceSet where
  def = TraceSet
    { showOriginal   = False
    , showTwin       = False
    }
