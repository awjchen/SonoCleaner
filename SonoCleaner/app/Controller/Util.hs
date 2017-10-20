module Controller.Util where

import           Control.Concurrent.STM
import           Control.Monad.Trans.Except
import           Graphics.Rendering.Chart
import           Graphics.UI.Gtk            hiding (set)

-------------------------------------------------------------------------------
-- MVar handling
-------------------------------------------------------------------------------

fillTMVar :: TMVar a -> a -> STM ()
fillTMVar tmvar a = do
   _ <- tryTakeTMVar tmvar
   putTMVar tmvar a

-------------------------------------------------------------------------------
-- Errors and error message dialogues
-------------------------------------------------------------------------------

confirmDialog :: Window -> String -> IO Bool
confirmDialog window msg = do
  msgDialog <- messageDialogNew (Just window) [] MessageInfo ButtonsOkCancel msg
  responseID <- dialogRun msgDialog
  widgetDestroy msgDialog
  return $ case responseID of
    ResponseOk -> True
    _          -> False

messageDialog :: Window -> String -> IO ()
messageDialog window msg = do
  msgDialog <- messageDialogNew (Just window) [] MessageInfo ButtonsOk msg
  _ <- dialogRun msgDialog
  widgetDestroy msgDialog

guiRunExceptT :: Window -> ExceptT String IO () -> IO ()
guiRunExceptT window m = do
  e <- runExceptT m
  case e of
    Left s   -> messageDialog window s
    Right () -> return ()

-------------------------------------------------------------------------------
-- Bounded and Enum
-------------------------------------------------------------------------------

listAll :: (Bounded a, Enum a) => [a]
listAll = [minBound..maxBound]

-------------------------------------------------------------------------------
-- Chart LayoutPick
-------------------------------------------------------------------------------

type LayoutPick' = LayoutPick Double Double Double
