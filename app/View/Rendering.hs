-- The display rendering loop, interfacing with Cairo and GTK
--
-- Rendering is done in a separate thread for responsiveness. However, we must
-- take care to call all GTK actions from the main GUI thread. (for more
-- information see http://dmwit.com/gtk2hs/)

{-# LANGUAGE PatternSynonyms #-}

module View.Rendering
  ( DrawCommand (..)
  , renderer
  ) where

import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad.Loops                    (iterateM_)
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
  -> IO a
renderer gtkImage surfaceTVar commandTMVar pickFnTVar =
  iterateM_ renderer' Nothing
  where
    renderer' :: Maybe ChartSpec -> IO (Maybe ChartSpec)
    renderer' lastChartSpec = do
      (command, surface) <- atomically $
        (,) <$> takeTMVar commandTMVar <*> readTVar surfaceTVar

      let newChartSpec = case command of
            DrawNew chartSpec -> Just chartSpec
            _                 -> lastChartSpec

      case newChartSpec of
        Nothing -> return ()
        Just chartSpec -> do
          w <- Cairo.imageSurfaceGetWidth  surface
          h <- Cairo.imageSurfaceGetHeight surface

          pickFn <- Cairo.renderWith surface $ basicRender chartSpec (w, h)
          atomically $ writeTVar pickFnTVar pickFn

          postGUISync $ do
            pixbuf <- pixbufNewFromSurface surface 0 0 w h
            imageSetFromPixbuf gtkImage pixbuf

      pure newChartSpec

basicRender ::
     ChartSpec
  -> (Int, Int)
  -> Cairo.Render (PickFn (LayoutPick Double Double Double))
basicRender chartSpec dims = B.runBackend env backendProgram
  where
    env = B.defaultEnv bitmapAlignmentFns
    backendProgram = render renderable rectSize
      where
        renderable = layoutToRenderable (chartLayout RegularMode chartSpec dims)
        rectSize = over both fromIntegral dims
