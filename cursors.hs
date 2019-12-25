
import Graphics.UI.Gtk
import Control.Monad.Trans
import Control.Concurrent.MVar
import System.Glib.UTFString
import Data.List
import Data.Maybe

content = intercalate "\n" [
 "When I wrote the following pages, or rather the bulk of them, I",
 "lived alone, in the woods, a mile from any neighbor, in a house",
 "which I had built myself, on the shore of Walden Pond, in",
 "Concord, Massachusetts, and earned my living by the labor of my",
 "hands only. I lived there two years and two months. At present I",
 "am a sojourner in civilized life again." ]

timeoutFunc var window view = do
  n <- readMVar var
  modifyMVar_ var (\_ -> 
    return ((n+1) `mod` (length [XCursor ..]-1)))
  let name = [XCursor .. ] !! n
  liftIO $ print (n,name)
  cursor <- cursorNew name
  set window [
    windowTitle := show name ]
  drawWindowM <- textViewGetWindow view TextWindowText
  case drawWindowM of
    Nothing -> return ()
    Just drawWindow -> 
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
  textBufferSetText buffer content
  containerAdd (toContainer sw) view
  set window [
    windowDefaultWidth := 310,
    windowDefaultHeight := 160,
    containerChild := sw]
  timeoutAdd (timeoutFunc var window view) 1000
  on window objectDestroy mainQuit
  widgetShowAll window
  drawWindowM <- textViewGetWindow view TextWindowText
  cursor <- cursorNew Arrow
  case drawWindowM of
    Nothing -> return ()
    Just drawWindow -> 
      drawWindowSetCursor drawWindow (Just cursor)
  mainGUI

gray n = Color gt gt gt
  where
    gt = round (n * 65535)

