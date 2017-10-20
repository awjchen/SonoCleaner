-- The display rendering loop, interfacing with Cairo and GTK
--
-- Rendering is done in a separate thread for responsiveness. However, we must
-- take care to call all GTK actions from the main GUI thread. (for more
-- information see http://dmwit.com/gtk2hs/)

module View.Rendering
  ( DrawCommand (..)
  , renderer
  ) where

import           Control.Concurrent.STM
import qualified Graphics.Rendering.Cairo               as Cairo
import           Graphics.Rendering.Chart
import qualified Graphics.Rendering.Chart.Backend.Cairo as B
import           Graphics.UI.Gtk

import           View.ChartLayout
import           View.Types

--------------------------------------------------------------------------------

data DrawCommand = DrawNew ChartSpec
                 | Redraw

renderer ::
     Image
  -> TVar Cairo.Surface
  -> TMVar DrawCommand
  -> TVar (PickFn (LayoutPick Double Double Double))
  -> Maybe ChartSpec
  -> IO ()
renderer gtkImage surfaceTVar commandTMVar pickFnTVar lastChartSpec = do
  (command, surface) <- atomically $
    (,) <$> takeTMVar commandTMVar <*> readTVar surfaceTVar

  case command of
    DrawNew chartSpec -> drawChart surface chartSpec
    Redraw -> case lastChartSpec of
      Just chartSpec ->  drawChart surface chartSpec
      Nothing ->
        renderer gtkImage surfaceTVar commandTMVar pickFnTVar lastChartSpec

  where
    drawChart :: Cairo.Surface -> ChartSpec -> IO ()
    drawChart surface chartSpec = do
      w <- Cairo.imageSurfaceGetWidth  surface
      h <- Cairo.imageSurfaceGetHeight surface

      pickFn <- Cairo.renderWith surface $ basicRender chartSpec (w, h)
      atomically $ writeTVar pickFnTVar pickFn

      postGUISync $ do
        pixbuf <- pixbufNewFromSurface surface 0 0 w h
        imageSetFromPixbuf gtkImage pixbuf

      renderer gtkImage surfaceTVar commandTMVar pickFnTVar (Just chartSpec)

basicRender ::
     ChartSpec
  -> (Int, Int)
  -> Cairo.Render (PickFn (LayoutPick Double Double Double))
basicRender chartSpec (width, height) =
  B.runBackend (B.defaultEnv bitmapAlignmentFns) backendProgram
  where
    rectSize = (fromIntegral width, fromIntegral height)
    backendProgram = render renderable rectSize
    renderable = layoutToRenderable (chartLayout chartSpec (width, height))
