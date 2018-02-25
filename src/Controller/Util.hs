module Controller.Util where

import           Graphics.UI.Gtk            hiding (set)

messageDialog :: Window -> String -> IO ()
messageDialog window msg = do
  msgDialog <- messageDialogNew (Just window) [] MessageInfo ButtonsOk msg
  _ <- dialogRun msgDialog
  widgetDestroy msgDialog
