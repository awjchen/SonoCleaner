-- Handling bounded and unbounded intervals

{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE TemplateHaskell #-}

module Types.Bounds where

import           Control.Lens
import           Data.Default

--------------------------------------------------------------------------------
-- ViewBounds <--> ViewPort
--------------------------------------------------------------------------------

-- ViewBounds

data ViewBounds = ViewBounds
  { _viewBoundsX :: (Double, Double)
  , _viewBoundsY :: (Double, Double)
  } deriving (Eq)
makeLenses ''ViewBounds

instance Default ViewBounds where
  def = ViewBounds { _viewBoundsX = (0, 2)
                   , _viewBoundsY = (0, 2) }

viewBoundsXY :: Traversal' ViewBounds (Double, Double)
viewBoundsXY f (ViewBounds bx by) = ViewBounds <$> f bx <*> f by

-- ViewPort

data ViewPort = ViewPort
  { _viewPortCenter :: (Double, Double)
  , _viewPortRadii  :: (Double, Double)
  } deriving (Eq)
makeLenses ''ViewPort

instance Default ViewPort where
  def = ViewPort { _viewPortCenter = (1, 1)
                 , _viewPortRadii  = (1, 1) }

-- iso

toViewPort :: Iso' ViewBounds ViewPort
toViewPort = iso f g
  where
    f :: ViewBounds -> ViewPort
    f vb = let (x0, x1) = vb ^. viewBoundsX
               (y0, y1) = vb ^. viewBoundsY
               cx = (x1+x0)/2; cy = (y1+y0)/2
               rx = (x1-x0)/2; ry = (y1-y0)/2
           in ViewPort (cx, cy) (rx, ry)
    g :: ViewPort -> ViewBounds
    g vp = let (cx, cy) = vp ^. viewPortCenter
               (rx, ry) = vp ^. viewPortRadii
               x0 = cx-rx; x1 = cx+rx
               y0 = cy-ry; y1 = cy+ry
           in  ViewBounds (x0, x1) (y0, y1)

--------------------------------------------------------------------------------
-- Misc.
--------------------------------------------------------------------------------

bound :: Ord a => (a, a) -> a -> a
bound (lower, upper) x
  | x < lower = lower
  | x > upper = upper
  | otherwise = x
