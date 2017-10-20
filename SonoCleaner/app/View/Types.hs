module View.Types where

import           Data.Colour         (AlphaColour)
import           Data.Default
import qualified Data.Vector.Unboxed as V

--------------------------------------------------------------------------------
-- Specification of the display
--------------------------------------------------------------------------------

data ChartSpec = ChartSpec
  { plotTitle            :: String
  , plotTitleColour      :: AlphaColour Double
  , plotSeries           :: V.Vector Double
  , plotJumpIndices      :: [Int] -- assumed sorted order
  , plotModifiedIndices  :: [Int] -- assumed sorted order
  , plotOriginalSeries   :: Maybe (V.Vector Double)
  , plotTwinSeries       :: Maybe (V.Vector Double)
  , plotCustomSeries     :: Maybe (V.Vector Double)
  , plotHighlightRegion  :: Maybe (Double, Double)
  , plotXRange           :: (Double, Double)
  , plotYRange           :: (Double, Double)
  , plotBackgroundColour :: AlphaColour Double
  , plotAnnotation       :: Maybe (Double, Double, String)
  , plotTimeStep         :: Double
  , plotToTime           :: Int -> Double
  , plotToIndex          :: Double -> Int
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
