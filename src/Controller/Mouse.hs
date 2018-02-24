-- Representing and interpreting mouse events

{-# LANGUAGE TemplateHaskell #-}

module Controller.Mouse
  ( MouseEvent
  , captureMouseEvent

  , KeyModifier (..)

  , MouseGesture (..)
  , interpretMouseGesture
  ) where

import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad.IO.Class   (liftIO)
import           Graphics.Rendering.Chart hiding (Limit)
import           Graphics.UI.Gtk          hiding (Point, set)

import           Controller.Util

--------------------------------------------------------------------------------
-- Recording mouse events
--------------------------------------------------------------------------------

data MouseEvent = MouseEvent
  { _mouseEventPixel          :: (Double, Double)
  , _mouseEventLayoutPick     :: Maybe LayoutPick'
  , _mouseEventTime           :: Int
  , _mouseEventButton         :: MouseButton
  , _mouseEventModifiers      :: [Modifier]
  , _mouseEventMouseModifiers :: [Modifier]
  }
makeLenses ''MouseEvent

captureMouseEvent ::
     STM (PickFn LayoutPick')
  -> EventM EButton MouseEvent
captureMouseEvent getPickFn = do
  pixelCoordinates <- eventCoordinates
  time <- eventTime
  pickFn <- liftIO $ atomically getPickFn
  button <- eventButton
  modifiers <- eventModifier
  mouseModifiers <- eventModifierMouse

  return MouseEvent
      { _mouseEventPixel          = pixelCoordinates
      , _mouseEventLayoutPick     = pickFn (uncurry Point pixelCoordinates)
      , _mouseEventTime           = fromIntegral time
      , _mouseEventButton         = button
      , _mouseEventModifiers      = modifiers
      , _mouseEventMouseModifiers = mouseModifiers
      }

--------------------------------------------------------------------------------
-- Interpreting mouse gestures
--------------------------------------------------------------------------------

data KeyModifier = ModNone
                 | ModControl
                 | ModShift
  deriving (Show)

-- We only recognize certain mouse gestures.
data MouseGesture =
    MouseClickLeft  Point
  | MouseClickRight Point
  | MouseDragRightX (Double, Double)
  | MouseDragRightXY KeyModifier Point Point
  deriving (Show)

interpretMouseGesture :: MouseEvent -> MouseEvent -> Maybe MouseGesture
interpretMouseGesture event1 event2 = do
  button    <- consistentMouseButtons event1 event2
  let keyModifier = case event2 ^. mouseEventModifiers of
        [Control] -> ModControl
        [Shift]   -> ModShift
        _         -> ModNone

  -- Location of mouse press
  p1 <- uncurry Point <$> case event1 ^. mouseEventLayoutPick of
    (Just (LayoutPick_PlotArea x y _)) -> Just (x, y)
    _                                  -> Nothing

  -- First distinguish between the left and right mouse buttons.

  -- There is only one function associated with the left mouse button (panning),
  -- so we need not distinguish between clicking and clicking-and-dragging for
  -- the left mouse button.

  -- We must, however, distinguish these two cases for the right mouse button.

  case button of
    LeftButton -> Just $ MouseClickLeft p1
    RightButton -> if isMouseClick event1 event2
      then Just $ MouseClickRight p1
      else do
        (lx1, ly1) <- fromLayoutPick =<< event1 ^. mouseEventLayoutPick
        (lx2, ly2) <- fromLayoutPick =<< event2 ^. mouseEventLayoutPick
        pt1@(x1, _) <- (,) <$> limitToMaybe lx1 <*> limitToMaybe ly1
        pt2@(x2, _) <- (,) <$> limitToMaybe lx2 <*> limitToMaybe ly2
        let xrange = (min x1 x2, max x1 x2)
            leftPoint  = uncurry Point $ min pt1 pt2 -- lexicographic order
            rightPoint = uncurry Point $ max pt1 pt2
        case keyModifier of
          ModNone    -> return $ MouseDragRightX xrange
          ModControl -> return $ MouseDragRightXY ModControl leftPoint rightPoint
          ModShift   -> return $ MouseDragRightXY ModShift   leftPoint rightPoint
    _ -> Nothing

consistentMouseButtons :: MouseEvent -> MouseEvent -> Maybe MouseButton
consistentMouseButtons event1 event2 =
  let pressButton = event1 ^. mouseEventButton
      sameButtons = pressButton == event2 ^. mouseEventButton
      noMouseModifiers =
            null   (event1 ^. mouseEventMouseModifiers)
        && length (event2 ^. mouseEventMouseModifiers) == 1
  in if sameButtons && noMouseModifiers
      then Just pressButton
      else Nothing

-- As opposed to a click-and-drag
isMouseClick :: MouseEvent -> MouseEvent -> Bool
isMouseClick event1 event2 =
  let (px1, py1) = event1 ^. mouseEventPixel
      (px2, py2) = event2 ^. mouseEventPixel
      dx = abs (px1 - px2)
      dy = abs (py1 - py2)
  in dx <= 5 && dy <= 5

fromLayoutPick :: LayoutPick' -> Maybe (Limit Double, Limit Double)
fromLayoutPick (LayoutPick_PlotArea x y _) = Just (Finite x, Finite y)
fromLayoutPick (LayoutPick_XTopAxis x)     = Just (Finite x, UpperLimit)
fromLayoutPick (LayoutPick_XBottomAxis x)  = Just (Finite x, LowerLimit)
fromLayoutPick (LayoutPick_YLeftAxis y)    = Just (LowerLimit, Finite y)
fromLayoutPick (LayoutPick_YRightAxis y)   = Just (UpperLimit, Finite y)
fromLayoutPick _                           = Nothing

--------------------------------------------------------------------------------
-- An ordering with lower and upper limits
--------------------------------------------------------------------------------

data Limit a = LowerLimit
             | Finite a
             | UpperLimit
  deriving (Eq, Ord)

limitToMaybe :: Limit a -> Maybe a
limitToMaybe (Finite a) = Just a
limitToMaybe _          = Nothing
