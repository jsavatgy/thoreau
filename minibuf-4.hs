
import Graphics.UI.Gtk
import qualified Graphics.UI.Gtk.Gdk.EventM as M
import Graphics.UI.Gtk.ModelView as Model
import Control.Monad.Trans 
import Control.Monad
import Control.Concurrent.MVar
import Control.Exception
import Text.Regex.Posix
import Text.Regex
import Text.Regex.Base
import Data.Array
import System.Glib.UTFString
import qualified Data.Map as Map
import Data.Maybe
import Data.Char
import Data.List.Split
import Data.List

data Changed = TextChanged | MouseMoved | Painted | Leaved
  deriving (Eq,Show)

re = "[[:alpha:]]+" 
reX = makeRegex re :: Regex

matchesA2 value = map elems $ matchAll reX (value :: String)
matchesA3 value = map assocs $ matchAll reX (value :: String)
matchesA4 value = map (!0) $ matchAll reX (value :: String)

subsK value = subRegex reX value "*\\0*"

gray n = Color gt gt gt
  where
    gt = round (n * 65535)

prolongar n x = x ++ replicate r ' '
  where
    l = length x
    r = n - l

prolongar2 n x y = x ++ replicate r ' ' ++ y
  where
    l = length x + length y
    r = n - l

grays = map gray [1.0,0.9..0.0]

createTag table n = do
  tag <- textTagNew (Just (stringToGlib ("gray" ++ show n)))
  let fg = if n > length grays `div` 2 
             then head grays else last grays 
  set tag [
    textTagStyle := StyleItalic,
    textTagForegroundGdk := fg,
    textTagBackgroundGdk := grays !! n]
  textTagTableAdd table tag

createTags table = do
 mapM (createTag table) [0 .. length grays - 1]

textBufferGetValue buffer = do
  start <- textBufferGetStartIter buffer
  end <- textBufferGetEndIter buffer
  value <- textBufferGetText buffer start end True
  return value

paintTag buffer ((a,b),n) = do
  iter0 <- textBufferGetIterAtOffset buffer a
  iter1 <- textBufferGetIterAtOffset buffer (a+b)
  textBufferApplyTagByName buffer 
    (stringToGlib ("gray" ++ show n)) iter0 iter1

paintTag3 buffer ((a,b),name,member)  = do
  let n = if member then 0 else 6
  paintTag buffer ((a,b),n)

paintTags2 buffer xs = do
  mapM_ (paintTag buffer) (zip xs [0 .. length grays - 1])

slice xs (start,n) = take n (drop start xs)

tabula1 xs = res1
  where
    res1 = [prolongar2 n a b | (a,b) <- xs] 
    n = maximum [length a + length b + 1 | (a,b) <- xs]

tabula2 ws ks = map (intercalate " ") res2
  where
    res2 = transpose cs
    cs = map tabula1 rows
    rows = chunksOf c xs
    c = l `div` cols + if (l `mod` cols > 0) then 1 else 0
    l = length xs
    xs = zip ws ms
    ms = map show ks
    cols = 4

bfChanged var buffer = do
  (old,o,wd) <- readMVar var
  insMark <- textBufferGetInsert buffer
  insIter <- textBufferGetIterAtMark buffer insMark
  offset <- textIterGetOffset insIter
  modifyMVar_ var (\_ -> return (0,TextChanged,wd))

matchingWds tree rules wd =
  [ (w,x) | (w,x) <- wrs, w `Map.member` tree  ]
  where
    wd1 = map toLower wd
    vrs = ruleEtExpls wd1 rules
    wrs = [(wd1,w2)] ++ vrs
    wr =  Map.lookup wd1 tree
    w1 = fromMaybe [] wr
    w2 = intercalate " " w1

isMember tree rules wd = or [ w `Map.member` tree | w <- ws ]
  where
    variants = applyRules2 wd rules
    ws = [wd] ++ variants
    
paintBuffer1 buffer dictTree rules = do
  start <- textBufferGetStartIter buffer
  end <- textBufferGetEndIter buffer
  textBufferRemoveAllTags buffer start end
  value <- textBufferGetValue buffer
  let 
    xs = matchesA2 value
    ts = matchesA4 value
    ws1 = map (slice value) ts
    ws = map (map toLower) ws1
    ks = map (isMember dictTree rules) ws
    ys = zip3 ts ws ks
    table = tabula2 ws ks
  mapM_ (paintTag3 buffer) ys

paintBuffer var buffer tree rules = do
  (old,o,wd) <- readMVar var
  modifyMVar_ var (\_ -> return (0,Painted,wd))
  paintBuffer1 buffer tree rules
  return ()

showT (x,y) = x ++ " " ++ y

showL xs = intercalate "\n" (map showT xs)

paintOtherWin var vbfs tree rules = do
  (old,o,wd) <- readMVar var
  let 
    (view1,buffer1,table1) = vbfs !! 0
    (view3,buffer3,table3) = vbfs !! 2
    f1 = matchingWds tree rules wd
    txt = showL f1
  liftIO $ textBufferSetText buffer3 txt
  liftIO $ putStrLn txt
  modifyMVar_ var (\_ -> return (0,Painted,wd))
  return ()

timeoutFunc var vbfs tree rules = do 
  let 
    (view1,buffer1,table1) = vbfs !! 0
    (view3,buffer3,table3) = vbfs !! 2
  (old,o,wd) <- readMVar var
  modifyMVar_ var (\_ -> return (old+1,o,wd))
  when (old >= 9 && o == TextChanged)
    (paintBuffer var buffer1 tree rules)
  when (old >= 9 && o == MouseMoved)
    (paintOtherWin var vbfs tree rules)
  return True

breakTab w = (map toLower a, words (drop 1 b))
  where
    (a,b) = break (=='\t') w

isWord w = w == takeWhile isAlpha w

ruleEtExpl [r,repl,expl] w = 
  if res1 /= w then (res1, expl2) else ("","") 
  where
    rplus = "^" ++ r ++ "$"
    reX = makeRegex rplus :: Regex
    res1 = subRegex reX w repl
    expl2 = "→ " ++ w ++ " "++ expl
ruleEtExpl _ _ = ("","")

ruleEtExpls w rs = filter (/= ("","")) [ruleEtExpl r w | r <- rs]

applyRule [r,repl,expl] w = if result /= w then result else "" 
  where
    rplus = "^" ++ r ++ "$"
    reX = makeRegex rplus :: Regex
    result = subRegex reX w repl
applyRule _ _ = ""

applyRules2 w rs = filter (not . null) [applyRule r w | r <- rs]

createRules = map (splitOn "\t") . splitOn "\n"

createWs2 dictText = ds3
  where
    ls = lines dictText
    ds1 = [breakTab w | w <- ls]
    ds2 = filter (\(a,b) -> isWord a) ds1
    ds3 = Map.fromList ds2

put1 (a,b) = do
  putStr a
  print b
  putStrLn ""

wrdUnderCursor var view buffer mouseX mouseY log = do
  (x,y) <- textViewWindowToBufferCoords view TextWindowText 
      (round mouseX, round mouseY) 
  (iter, _) <- textViewGetIterAtPosition view x y
  start <- textIterCopy iter
  end <- textIterCopy iter
  startsWord <- textIterStartsWord start
  endsWord <- textIterEndsWord end
  insideWord <- textIterInsideWord iter  
  when (log > 0) (print ("startsWord = ",startsWord,
    "endsWord = ",endsWord,"insideWord = ",insideWord))
  when (insideWord && not startsWord) 
    (void $ textIterBackwardWordStart start)
  when (insideWord && not endsWord)
    (void $ textIterForwardWordEnd end)
  value <- textBufferGetText buffer start end True
  (old,o,wd) <- readMVar var
  modifyMVar_ var (\_ -> 
    return (0,MouseMoved,glibToString value))
  return value

setMouse view = do
  drawWindowM <- textViewGetWindow view TextWindowText
  display <- displayGetDefault
  cursor2 <- cursorNewFromName (fromJust display) "crosshair"
  let cursor = fromJust cursor2
      drawWindow = fromJust drawWindowM
  drawWindowSetCursor drawWindow (Just cursor)

vwLeave1 var = do
  (old,o,wd) <- readMVar var
  modifyMVar_ var (\_ -> return (0,Leaved,wd))

vwLeave var = do
  liftIO $ vwLeave1 var 
  return False

vwMotion var tree rules vbfs = do
  let 
    (view1,buffer1,table1) = vbfs !! 0
    (view3,buffer3,table3) = vbfs !! 2
  liftIO $ setMouse view1
  (mouseX,mouseY) <- M.eventCoordinates
  liftIO $ wrdUnderCursor var view1 buffer1 mouseX mouseY 0
  return False

vwButtonPress var view buffer  = do
  (mouseX,mouseY) <- M.eventCoordinates
  value <- liftIO $ wrdUnderCursor var view buffer mouseX mouseY 1
  return False

addTextView name scrollBars n = do
  sw <- scrolledWindowNew Nothing Nothing
  set sw [
    scrolledWindowVscrollbarPolicy := 
      if scrollBars then PolicyAlways else PolicyNever,
    scrolledWindowHscrollbarPolicy := PolicyAutomatic ]
  view <- textViewNew
  widgetSetName view name
  widgetAddEvents view [PointerMotionMask,LeaveNotifyMask]
  buffer <- textViewGetBuffer view
  table <- textBufferGetTagTable buffer
  font <- fontDescriptionFromString "Monospace 9"
  widgetModifyFont view (Just font)
  widgetModifyBase view StateNormal (gray 0.94)
  ct <- widgetGetPangoContext view
  t <- layoutText ct "^_^"
  (_,Rectangle x1 y1 x2 y2) <- layoutGetPixelExtents t
  widgetSetSizeRequest sw (-1) (n*(y2-y1))
  containerAdd (toContainer sw) view
  return (sw,(view,buffer,table))

createWindow = do
  window <- windowNew
  vbox1 <- vBoxNew False 0
  containerAdd window vbox1
  (sw1,vbf1) <- addTextView "view1" True 5
  (sw2,vbf2) <- addTextView "view2" False 1
  (sw3,vbf3) <- addTextView "view3" False 3
  set window [
    windowDefaultWidth := 480,
    windowDefaultHeight := 300
    ]
  boxPackStart vbox1 sw1 PackGrow 0
  boxPackStart vbox1 sw2 PackNatural 1
  boxPackStart vbox1 sw3 PackNatural 0
  return (window,[vbf1,vbf2,vbf3])

main = do
  initGUI
  var <- newMVar (9,TextChanged,"")
  content <- readFile "thoreau-initio-2.txt"
  dictText <- readFile "51500-tab.txt"
  rulesText <- readFile "regulas-5.txt" 
  (window,vbfs) <- createWindow
  let 
    dictTree = createWs2 dictText
    regulas = createRules rulesText
    (view,buffer,table) = vbfs !! 0
  textBufferSetText buffer content
  view `on` motionNotifyEvent $ vwMotion var dictTree regulas vbfs 
  view `on` leaveNotifyEvent $ vwLeave var 
  view `on` buttonPressEvent $ vwButtonPress var view buffer 
  timeoutAdd (timeoutFunc var vbfs dictTree regulas) 50

  buffer `on` bufferChanged $ bfChanged var buffer
  window `on` objectDestroy $ mainQuit

  widgetShowAll window
  createTags table
  mainGUI

