-- The 'View' of "Model, View, Controller"
--
-- In this program, the View is responsible for accepting a specification of a
-- chart (ChartSpec) and rendering it to the display, and for reporting the
-- user's interactions with the rendered chart.

-- This file defines the interface of the View to the rest of the program.

module View
  ( TraceSet (..)

  , setupRenderer
  ) where

import           Control.Concurrent                     (forkIO)
import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad                          (void)
import           Control.Monad.IO.Class                 (liftIO)
import           Data.Default
import qualified Graphics.Rendering.Cairo               as Cairo
import           Graphics.Rendering.Chart               (LayoutPick, PickFn,
                                                         layoutToRenderable)
import qualified Graphics.Rendering.Chart.Backend.Cairo as Chart
import           Graphics.UI.Gtk                        hiding (set)

import           View.ChartLayout
import           View.Rendering
import           View.Types

-------------------------------------------------------------------------------
-- View interface
-------------------------------------------------------------------------------

setupRenderer ::
     Window
  -> Image
  -> Box
  -> IO ( TVar (PickFn (LayoutPick Double Double Double))
        , ChartSpec -> STM ()
        , FilePath -> Chart.FileFormat -> ChartSpec -> IO () )
setupRenderer controllerWindow image controllerBox = do
  -- Define global rendering variables
  drawCommandTMVar <- newEmptyTMVarIO :: IO (TMVar DrawCommand)

  surfaceTVar <- do
    surface <- Cairo.createImageSurface Cairo.FormatRGB24 1024 540
    newTVarIO surface

  pickFnTVar <- atomically $ newTVar undefined
    :: IO (TVar (PickFn (LayoutPick Double Double Double)))

  -- Start renderer
  _ <- forkIO $ renderer image surfaceTVar drawCommandTMVar pickFnTVar

  let requestDraw :: ChartSpec -> STM ()
      requestDraw chartSpec =
        fillTMVar drawCommandTMVar (DrawNew chartSpec)

      requestScreenshot :: FilePath -> Chart.FileFormat -> ChartSpec -> IO ()
      requestScreenshot filePath format chartSpec = do
        surface <- atomically $ readTVar surfaceTVar
        w <- Cairo.imageSurfaceGetWidth  surface
        h <- Cairo.imageSurfaceGetHeight surface
        let fileOptions = def & Chart.fo_size .~ (w, h)
                              & Chart.fo_format .~ format
        void $ Chart.renderableToFile fileOptions filePath
          $ layoutToRenderable (chartLayout chartSpec (w, h))

  -- Register callback: resize surface and redraw on window resize
  _ <- controllerWindow `on` configureEvent $ do
    (w, h) <- eventSize
    boxHeight <- liftIO $ widgetGetAllocatedHeight controllerBox
    liftIO $ do
      newSurface <-
        Cairo.createImageSurface Cairo.FormatRGB24 w (max 0 (h-boxHeight))
      void $ atomically $ do
        writeTVar surfaceTVar newSurface
        fillTMVar drawCommandTMVar Redraw
    return False

  return (pickFnTVar, requestDraw, requestScreenshot)

-------------------------------------------------------------------------------
-- Misc.
-------------------------------------------------------------------------------

fillTMVar :: TMVar a -> a -> STM ()
fillTMVar tmvar a = do
   _ <- tryTakeTMVar tmvar
   putTMVar tmvar a
