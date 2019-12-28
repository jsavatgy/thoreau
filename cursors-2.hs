
import Graphics.UI.Gtk
import Control.Monad.Trans
import Control.Concurrent.MVar
import System.Glib.UTFString
import Data.List
import Data.Maybe

cursors = [
  --"none",
  "default","help","pointer","context-menu","progress",
  "wait","cell","crosshair","text","vertical-text","alias",
  "copy","no-drop","move","not-allowed","grab","grabbing",
  "all-scroll","col-resize","row-resize","n-resize","e-resize",
  "s-resize","w-resize","ne-resize","nw-resize","sw-resize",
  "se-resize","ew-resize","ns-resize","nesw-resize",
  "nwse-resize","zoom-in","zoom-out" ]

content = intercalate " " cursors 

timeoutFunc var window view = do
  n <- readMVar var
  modifyMVar_ var (\_ -> 
    return ((n+1) `mod` (length cursors)))
  let name = cursors !! n
  liftIO $ print (n,name)
  display <- displayGetDefault
  maybeCursor <- cursorNewFromName (fromJust display) name
  let cursor = fromJust maybeCursor
  set window [
    windowTitle := show name ]
  drawWindowM <- textViewGetWindow view TextWindowText
  let drawWindow = fromJust drawWindowM
  drawWindowSetCursor drawWindow (Just cursor)
  return True

main = do
  initGUI
  var <- newMVar 0
  window <- windowNew
  sw <- scrolledWindowNew Nothing Nothing
  set sw [
    scrolledWindowVscrollbarPolicy := PolicyAlways,
    scrolledWindowHscrollbarPolicy := PolicyAutomatic ]
  view <- textViewNew
  buffer <- textViewGetBuffer view
  table <- textBufferGetTagTable buffer
  font <- fontDescriptionFromString "Monospace 9"
  widgetModifyFont view (Just font)
  widgetModifyBase view StateNormal (gray 0.94)
  textViewSetWrapMode view WrapWord
  textBufferSetText buffer content
  containerAdd (toContainer sw) view
  set window [
    windowDefaultWidth := 310,
    windowDefaultHeight := 160,
    containerChild := sw]
  timeoutAdd (timeoutFunc var window view) 1000
  on window objectDestroy mainQuit
  widgetShowAll window
  mainGUI

gray n = Color gt gt gt
  where
    gt = round (n * 65535)

