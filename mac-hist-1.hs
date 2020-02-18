import Control.Concurrent.MVar 
import System.Directory
import System.Environment (getArgs)
import Graphics.UI.Gtk
import Graphics.Rendering.Cairo
import Control.Monad
import qualified Graphics.UI.Gtk.Gdk.EventM as M
import System.Glib.UTFString (glibToString)
import Database.HDBC
import Database.HDBC.Sqlite3 (connectSqlite3)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Array.MArray
import Data.Word
import Data.Int
import Data.Bits
import Data.List
import Data.List.Split
import Data.Maybe (fromJust)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as Lazy

justWhen :: (a -> Bool) -> (a -> b) -> (a -> Maybe b)
justWhen f g a = if f a then Just (g a) else Nothing

nothingWhen :: (a -> Bool) -> (a -> b) -> (a -> Maybe b)
nothingWhen f = justWhen (not . f)

chunksOf1 :: Int -> BS.ByteString -> [BS.ByteString]
chunksOf1 x = unfoldr (nothingWhen BS.null (BS.splitAt x))

chunksOf' :: Int64 -> Lazy.ByteString -> [Lazy.ByteString]
chunksOf' x = unfoldr (nothingWhen Lazy.null (Lazy.splitAt x))

ratios = [
 (' ',0.15604),('e',0.11835),('a',0.08029),('o',0.06147),('i',0.06096),
 ('n',0.05648),('r',0.05105),('s',0.05030),('l',0.05013),('t',0.04979),
 ('u',0.03144),('c',0.02938),('d',0.02446),('m',0.02224),('p',0.02195),
 ('v',0.01608),('.',0.01069),(',',0.01014),('b',0.00874),
 ('g',0.00814),('h',0.00685),('f',0.00677),('q',0.00614),('I',0.00423),
 ('L',0.00280),('-',0.00198),('A',0.00183),('x',0.00180),('S',0.00155),
 ('j',0.00137),('E',0.00136),('M',0.00135),('C',0.00132),('P',0.00123),
 ('T',0.00112),('N',0.00107),('y',0.00098),('?',0.00097),(':',0.00085),
 ('1',0.00085),('D',0.00084),('R',0.00073),('U',0.00070),('0',0.00069),
 ('"',0.00066),('O',0.00066),(')',0.00060),('(',0.00060),('B',0.00060),
 (';',0.00060),('V',0.00058),('G',0.00055),('!',0.00051),('k',0.00051),
 ('Q',0.00051),('F',0.00050),('2',0.00047),('H',0.00044),
 ('_',0.00032),('5',0.00032),('9',0.00031),('z',0.00031),
 ('J',0.00029),('3',0.00025),('8',0.00024),('4',0.00022),('7',0.00019),
 ('6',0.00018),('\'',0.00015),('w',0.00012),('K',0.00011),('–',0.00010),
 ('W',0.00010),('X',0.00008),('é',0.00005),
 ('Y',0.00005),('à',0.00004),('/',0.00003),('Z',0.00003),
 ('+',0.00001)
 ]

intToDouble :: Int -> Double
intToDouble i = fromRational (toRational i)

word8ToDouble :: Word8 -> Double
word8ToDouble i = fromRational (toRational i)

w8 :: Double -> Word8
w8 d = round (255 * d)

octets :: Word32 -> [Word8]
octets w = [ 
  fromIntegral (w `shiftR` 24), 
  fromIntegral (w `shiftR` 16), 
  fromIntegral (w `shiftR` 8), 
  fromIntegral w ]

firstArg args = 
  case args of
    [] -> error "must supply a file to open"
    [arg] -> arg
    _ -> error "too many arguments"

insS = Set.insert
delS = Set.delete
letS = Set.singleton
memS = Set.member
--notS = Set.notMember

data Changed = 
  ImgChanged | MouseMoved | Painted | Leaved | Scrolled
  deriving (Eq,Ord,Show)

timeoutFunc var var2 canvas surf = do
  (old,o,scaleOld,oldX,oldY) <- readMVar var 
  modifyMVar_ var (\_ -> return (old+1,o,scaleOld,oldX,oldY))
  win <- widgetGetDrawWindow canvas
  logMsg0 ("timeoutFunc: " ++ s (old+1,o,scaleOld,oldX,oldY))
  when (old >= 9 && 
   (ImgChanged `memS` o || Scrolled `memS` o))
    (renderWithDrawable win $ paintImage1 var var2 surf)
  return True

putSurf surface = do
  setSourceSurface surface 0 0
  paint
  return ()

data RGB = RGB Double Double Double
  deriving Show

setColor (RGB red grn blu) = do
  setSourceRGBA red grn blu 1.0

black      = RGB 0.00 0.00 0.00 
white      = RGB 1.00 1.00 1.00 
pink       = RGB 0.96 0.57 0.70
violet     = RGB 0.69 0.61 0.85
orange     = RGB 0.98 0.63 0.15
blue       = RGB 0.33 0.67 0.82
sand       = RGB 0.90 0.80 0.55
darkBrown  = RGB 0.67 0.45 0.27
gray n = RGB n n n

data EvtType = Press | Release | Move | Scroll | None

release Press button var mouseX mouseY = do
  (old,o,varS,varX,varY) <- readMVar var
  let
    x = (mouseX - varX) / varS
    y = (mouseY - varY) / varS
  logMsg0 ("Add point: " ++ s x ++ s y ++ s button)
  logMsg1 (s x ++ s y)

release _ button var x y = do
  liftIO $ logMsg0 ("Release (other): " ++ s x ++ s y)

changePos vPos var surf canvas mouseX mouseY = do
  (cause,vPosX,vPosY) <- readMVar vPos
  (old,o,scaleOld,oldX,oldY) <- readMVar var
  let
    dx = vPosX - mouseX
    dy = vPosY - mouseY
  modifyMVar_ var (\_ -> 
    return (0,letS ImgChanged,scaleOld,oldX - dx,oldY - dy))
  modifyMVar_ vPos (\_ -> 
    return (Move,mouseX,mouseY))

s x = show x ++ " "

printMouse = do
  (mouseX,mouseY) <- M.eventCoordinates
  logMsg0 ("Mouse: " ++ s mouseX ++ s mouseY)
  return  (mouseX,mouseY)

printPointer canvas = do
  (widX,widY) <- widgetGetPointer canvas
  logMsg0 ("Widget: " ++ s widX ++ s widY)

printMVar var mouseX mouseY = do
  (old,o,varS,varX,varY) <- readMVar var
  let
    x = (mouseX - varX) / varS
    y = (mouseY - varY) / varS
  logMsg0 ("MVar: " ++ s varS ++ s varX ++ s varY)
  logMsg0 ("Calc: " ++ s x ++ s y)

centerImg var surf canvas = do
  w1 <- imageSurfaceGetWidth surf
  h1 <- imageSurfaceGetHeight surf
  (w2,h2) <- widgetGetSizeRequest canvas
  let
    dh = intToDouble (h2 - h1)
    dw = intToDouble (w2 - w1)
  liftIO $ modifyMVar_ var (\_ -> return (4,letS ImgChanged,1.0,dw / 2,dh / 2))

letpix1 ltr1 = do
  let
    ltr = [ltr1] 
    filename = "results4/" ++ letterName ltr ++ ".png"
  (ltr,w,h,pxs1) <- withImageSurfaceFromPNG filename $ \surface -> do
    (w,h,pxs1) <- surfacePixels surface False
    return (ltr,w,h,pxs1)
  return (ltr,w,h,pxs1)

avgGray xs = (sum (map word8ToDouble xs) / genericLength xs) / 255

avg xs = sum xs / genericLength xs

avgLineHist = [ (0,21.533),(1,21.521),(2,20.585),(3,20.496),
  (4,20.084),(5,20.397),(6,20.628),(7,20.757),(8,14.842),
  (9,12.161),(10,11.324),(11,13.573),(12,16.222),(13,16.22),
  (14,14.916),(15,14.807),(16,15.198),(17,17.124),(18,16.752),
  (19,14.287),(20,9.516),(21,9.897),(22,12.04),(23,20.429),
  (24,21.356),(25,21.356),(26,21.284),(27,21.111),(28,21.128),
  (29,21.186) ]

avgColHist = [ (0,29.301),(1,28.917),(2,27.292),(3,24.874),
  (4,22.036),(5,20.15),(6,20.491),(7,20.985),(8,22.091),
  (9,21.61),(10,21.345),(11,21.631),(12,21.777),(13,23.205),
  (14,22.695),(15,21.618),(16,21.395),(17,21.997),(18,24.291),
  (19,26.961),(20,28.83),(21,29.235) ]

letterHeightD = intToDouble letterHeight
letterWidthD =  intToDouble letterWidth
letterHeight = length avgLineHist
letterWidth = length avgColHist

litteras = [c | (c,a) <- ratios]

letterName "\"" = "double-quote"
letterName ")" = "right-bracket"
letterName "(" = "left-bracket"
letterName "é" = "e-acute"
letterName "à" = "a-grave"
letterName "'" = "single-quote"
letterName "." = "dot"
letterName "," = "comma"
letterName "/" = "slash"
letterName "!" = "exclamation-mark"
letterName "?" = "question-mark"
letterName "-" = "hyphen"
letterName "+" = "plus"
letterName "–" = "en-dash"
letterName "_" = "underscore"
letterName ":" = "colon"
letterName ";" = "semicolon"
letterName " " = "space"
letterName c = c

surfacePixels sf transps = do
  w <- imageSurfaceGetWidth sf
  h <- imageSurfaceGetHeight sf
  pixelData <- imageSurfaceGetData sf -- order: BGRA
  stride <- imageSurfaceGetStride sf
  let 
    zs1 = chunksOf1 stride pixelData
    zs2 = map (chunksOf1 4) zs1
    zs4 = (map . map) (avgGray . init . BS.unpack) zs2
    zs5 = if transps then transpose1 zs4 else zs4
  logMsg0 ("w,h,ln,ln", w,h,length (head zs4), length zs4)
  return (w,h,zs5)

lineHistog pxs = map sum pxs

localMinima (a:b:c:xs)
  | x2 < x1 && x2 < x3 = b : localMinima xs
  | otherwise = localMinima (b:c:xs)
  where 
    (_,x1) = a
    (_,x2) = b
    (_,x3) = c
localMinima _ = []

avg5 n = map (\(x,y) -> (x, intToDouble n * y))
avgL2 = avg5 55 avgLineHist
avgC2 = avg5 1 avgColHist
avgL = Map.fromList avgL2
avgC = Map.fromList avgC2

line1 c x1 y1 x2 y2 = do
  setColor (gray c)
  moveTo x1 y1
  lineTo x2 y2
  stroke

lineTx i x1 y1 x2 y2 = do
  setColor (gray 0.5)
  moveTo x1 y1
  lineTo x2 y2
  stroke
  moveTo (x1 + 2 + intToDouble i) y1
  showText (show i)

lineTx2 i c x1 y1 x2 y2 = do
  setColor (gray c)
  moveTo x1 y1
  lineTo x2 y2
  stroke
  setColor black
  moveTo x1 (y1 + 40 + intToDouble i)
  showText (show i)

artPx x
  | x > 0.80 = gr !! 4
  | x > 0.60 && x <= 0.80 = gr !! 3
  | x > 0.40 && x <= 0.60 = gr !! 2
  | x > 0.20 && x <= 0.40 = gr !! 1
  | otherwise = gr !! 0
  where gr = "█▓▒░ "

asciiArt pxs  = intercalate "\n" [[artPx x
  | x <- xs] | xs <- pxs] ++ "\n"

comparePxs pxs pix = sortOn snd p1
  where
    p1 = [(letterName c, cmpPx pxs px) | (c,d,w,h,px) <- pix]
    cmpPx px1 px2 = sum [cmpLn a b | (a,b) <- zip px1 px2]
    cmpLn a b =  sum [abs (a - b) | (a,b) <- zip a b]

paintBl x y r = do
  let s = intToDouble scale2
  setColor (gray r)
  rectangle1 (intToDouble x * s) (intToDouble y * s) s s 
  fill

paintLn (y,ln) = do
  mapM_ (\(x,r) -> paintBl x y r) (zip [0..] ln)

paintMagn px = do
  mapM_ paintLn (zip [0..] px)

updateCanvas2 var2 canvas2 = do
  pxs <- readMVar var2
  logMsg1 "updateCanvas2"
  logMsg2 ("length pxs =",length pxs)
  let (cds,px) = head pxs
  win <- widgetGetDrawWindow canvas2
  renderWithDrawable win $ do
    paintMagn px
  return True

drawBestColumn letpix surface (x,y) = do
  let filename = "stripes/letter-" ++ show (round x) ++ "-" ++ show (round y) ++ ".png"
  ((x,y),pxs1) <- liftIO $ withImageSurface FormatARGB32 letterWidth letterHeight $ \surface2 -> do
    renderWith surface2 $ do
      setSourceSurface surface (-x) 0
      paint
      (w,h,pxs1) <- liftIO $ surfacePixels surface2 False
      let pr1 = comparePxs pxs1 letpix
      logMsg3 (take 5 pr1)
      logMsg1 (asciiArt pxs1)
      logMsg0 ("filename = ",filename)
      return ((x,y),pxs1)
  return ((x,y),pxs1)

drawStripe letpix original i w y surface var2 = do
  let filename = "stripes/stripe-" ++ show i ++ ".png"
  setSourceSurface original 0 (-y-1)
  --setSourceSurface original 0 (-y)
  paint
  (w1,h1,pxs2) <- liftIO $ surfacePixels surface True
  let
    colHist  = lineHistog pxs2
    ch = Map.fromList (zip [0..] colHist)
    ch2 = [sum [abs ((ch Map.! (x1+x)) - (avgC Map.! x))
      | (x,y) <- avgC2] 
        | x1 <- [0..length colHist - length avgC]]
    cmax = maximum ch2
    cm = localMinima (zip [0..] ch2)
    sr = sortOn snd cm
    a = fsts sr
    b = snds sr
    sortedCm = zip3 [1..] a b
    coords = [(x,y) | x <- a]
    crd = take 5 coords
  logMsg0 ("length = ",length cm)
  logMsg0 ("coords = ",coords)
  --return surface
  bst <- mapM (drawBestColumn letpix surface) crd
  liftIO $ modifyMVar_ var2 (\_ -> return bst)

  {-
  liftIO $ withImageSurface FormatARGB32 w 200 $ \surface -> do
    renderWith surface $ drawStripeWithResults original i w y surface sortedCm
  -}

rectangle1 = Graphics.Rendering.Cairo.rectangle
transpose1 = Data.List.transpose
chunksOf2 = Data.List.Split.chunksOf

drawStripeWithResults original i w y surface sortedCm = do
  let filename = "results5/stripe-" ++ show i ++ ".png"
  setColor white
  paint
  setSourceSurface original 0 (-y)
  rectangle1 0 0 (intToDouble w) letterHeightD
  clip
  paint
  resetClip
  setLineWidth 1.0
  setDash [1,3] 0
  mapM_ (\(i,x,z) -> lineTx2 i 0.5 x 0 x 200) sortedCm 
  liftIO $ surfaceWriteToPNG surface filename

mkStripe letpix original i y w var2 = do
  withImageSurface FormatARGB32 w letterHeight $ \surface -> do
    renderWith surface $ drawStripe letpix original i w y surface var2

fsts xs = [a | (a,b) <- xs]
snds xs = [b | (a,b) <- xs]

bestLines pxs1 = sortedLm
  where
    lineHist = lineHistog pxs1
    lh = Map.fromList (zip [0..] lineHist)
    lh2 = [sum [abs ((lh Map.! (x1+x)) - (avgL Map.! x))
      | (x,y) <- avgL2] 
        | x1 <- [0..length lineHist - length avgL]]
    lmax = maximum lh2
    lm = localMinima (zip [0..] lh2)
    sr = sortOn snd lm
    a = fsts sr
    b = snds sr
    sortedLm = zip3 [1..] a b

drawBestLines original w h pxs1 letpix sortedLm var2 = do
  let [wd,hd] = map intToDouble [w,h]
  setSourceSurface original 0 0
  paint
  setLineWidth 1.0
  setDash [1,3] 0
  mapM_ (\(i,y,z) -> lineTx i 0 y wd y) sortedLm
  mapM_ (\(i,y,z) -> liftIO $ mkStripe letpix original i y w var2) (take 1 sortedLm)

paintBestLines surface sortedLm pxs1 letpix var2 = do
  w <- imageSurfaceGetWidth surface
  h <- imageSurfaceGetHeight surface
  surface2 <- withImageSurface FormatARGB32 w h $ \surface2 -> do
    renderWith surface2 $ drawBestLines surface w h pxs1 letpix sortedLm var2
    renderWith surface $ putSurf surface2
    return surface2
  return ()

createTable1 sql table = do
  logMsg1 "createTable1"
  quickQuery sql ( 
    "create table " ++ table ++ 
    " (rank integer, line real, value real);") []
  commit sql

insertInto1 :: IConnection conn => conn -> String -> [(Int,Double,Double)] -> IO ()
insertInto1 sql table values = do
  logMsg1 "insertInto1"
  mapM_ (\(a,b,c) ->
   quickQuery sql (
    "insert into " ++ table ++ " values (?,?,?);" )
    [toSql a, toSql b, toSql c])
   values
  commit sql

convRow1 :: [SqlValue] -> (Int,Double,Double)
convRow1 [sRank,sLine,sValue] = (rank,line,value)
  where
    rank = fromJust (fromSql sRank)
    line = fromJust (fromSql sLine)
    value = fromJust (fromSql sValue)

selectFrom1 sql table = do
  liftIO $ logMsg1 "selectFrom1"
  r <- quickQuery sql (
   "select rank, line, value from " ++ table ++ ";") []
  return (map convRow1 r)

createIns1 sql table pxs1 = do
  let res = bestLines pxs1
  createTable1 sql table
  insertInto1 sql table res
  return res

readValues1 sql table pxs1 = do
  tables <- getTables sql
  x <- if table `elem` tables
    then selectFrom1 sql table
    else createIns1 sql table pxs1
  return x

createTable2 sql table = do
  logMsg1 "createTable2"
  quickQuery sql ( 
    "create table " ++ table ++ 
    " (letter string, count integer, width integer, height integer, value blob);") []
  commit sql

insertInto2 :: IConnection conn => conn -> String -> 
  [(String,Int,Int,Int,BS.ByteString)] -> IO ()
insertInto2 sql table values = do
  logMsg1 "insertInto2"
  mapM_ (\(a,b,c,d,e) ->
   quickQuery sql (
    "insert into " ++ table ++ " values (?,?,?,?,?);" )
    [toSql a, toSql b, toSql c,toSql d, toSql e])
   values
  commit sql

convRow2 :: [SqlValue] -> (String,Int,Int,Int,BS.ByteString)
convRow2 [sLetter,sCount,sw,sh,sValue] = (letter,count,w,h,value)
  where
    letter = fromJust (fromSql sLetter)
    count = fromJust (fromSql sCount)
    value = fromJust (fromSql sValue)
    w = fromJust (fromSql sw)
    h = fromJust (fromSql sh)

selectFrom2 sql table = do
  logMsg1 "selectFrom2"
  r <- quickQuery sql (
   "select letter, count, width, height, value from " ++ table ++ ";") []
  return (map convRow2 r)

zconv (s1,c1,w1,h1,b1) = (s1,c1,w1,h1,zs4) 
  where
    zs1 = chunksOf1 w1 b1
    zs2 = map BS.unpack zs1 
    zs4 = [[avgGray [c] | c <- ls] | ls <- zs2]

selectConv2 sql table = do
  ts <- selectFrom2 sql table
  let t3 = map zconv ts
  return t3

createIns2 sql table = do
  letpix <- mapM letpix1 litteras
  let 
    f px = (BS.pack . map w8) (concat px)
    res = [(letterName letter,1,w,h,f pxs) | (letter,w,h,pxs) <- letpix]
    res2 = [(letter,1,w,h,pxs) | (letter,w,h,pxs) <- letpix]
  createTable2 sql table
  insertInto2 sql table res 
  return res2

readValues2 sql table = do
  tables <- getTables sql
  x <- if table `elem` tables
    then selectConv2 sql table
    else createIns2 sql table 
  return x

getHistogram var sql surface var2 = do
  letpix <- readValues2 sql "letpix"
  (w,h,pxs1) <- surfacePixels surface False
  sortedLm <- readValues1 sql "minima" pxs1
  paintBestLines surface sortedLm pxs1 letpix var2

keyInput var sql surf canvas key = do
  logMsg2 ("key = ",key)
  case key of
    "q" -> do
      liftIO $ mainQuit
    "1" -> do
      liftIO $ centerImg var surf canvas

changeRef var d mouseX mouseY = do
  (old,o,scaleOld,oldX,oldY) <- readMVar var
  let
    scaleD = scale1 d
    scaleNew = scaleD * scaleOld
    dx = (mouseX - oldX) * (scaleD - 1)
    dy = (mouseY - oldY) * (scaleD - 1)
    newX = oldX - dx
    newY = oldY - dy
    result =  (0,letS Scrolled,scaleNew,newX,newY)
  modifyMVar_ var (\_ -> return result)
  logMsg0 ("Change MVar: " ++ s scaleNew ++ 
    s newX ++ s newY)
  where
    factor = 5 / 4
    scale1 ScrollUp   = factor
    scale1 ScrollDown = 1 / factor

updateCanvas1 var var2 canvas surf = do
  win <- widgetGetDrawWindow canvas
  (width, height) <- widgetGetSize canvas
  renderWithDrawable win $
    paintImage1 var var2 surf

  return True

surfaceFromPNG file =
  withImageSurfaceFromPNG file $ \png -> do
    liftIO $ logMsg0 "Load Image"
    w <- renderWith png $ imageSurfaceGetWidth png
    h <- renderWith png $ imageSurfaceGetHeight png
    surf <- createImageSurface FormatRGB24 w h
    renderWith surf $ do
      setSourceSurface png 0 0
      paint
    return surf

paintImage1 var var2 surf = do
  (old,o,sc,x,y) <- liftIO $ readMVar var
  pxs <- liftIO $ readMVar var2
  let crd = [(xc,yc) | ((xc,yc),px) <- pxs]
  let o1 = (insS Painted . delS ImgChanged . delS Scrolled) o
  liftIO $ modifyMVar_ var (\_ -> return (0,o1,sc,x,y))
  setColor white
  paint
  translate x y
  scale sc sc
  logMsg0 ("Paint Image: ", s sc, s x, s y)
  setSourceSurface surf 0 0
  paint
  setColor orange
  mapM_ (\(xc,yc) -> rectangle1 xc yc letterWidthD letterHeightD) crd
  stroke

logMsg3 s = do
  liftIO $ mapM_ (putStrLn . show) s
logMsg2 s = do
  liftIO $ putStrLn (show s)
logMsg1 s = do
  liftIO $ putStrLn s
logMsg0 _ = do
  return ()

grayGtk n = Color gt gt gt
  where
    gt = round (n * 65535)

scale2 = 20

createWindow2 = do
  window <- windowNew
  canvas <- drawingAreaNew
  widgetAddEvents canvas [Button1MotionMask] 
  widgetSetSizeRequest canvas (scale2*letterWidth) (scale2*letterHeight)
  set window [containerChild := canvas]
  return (window,canvas)

main = do
  args <- getArgs
  let arg1 = firstArg args
  sql <- connectSqlite3 "macov.sqlite"
  initGUI
  var <- newMVar (4,letS ImgChanged,1.0,0.0,0.0)
  vPos <- newMVar (None,0.0,0.0)
  var2 <- newMVar []
  window <- windowNew
  canvas <- drawingAreaNew
  surf <- surfaceFromPNG arg1
  (window2,canvas2) <- createWindow2
  widgetAddEvents canvas [Button1MotionMask] 
  windowSetDefaultSize window 640 920
  --widgetSetSizeRequest canvas 630 891
  centerImg var surf canvas 
  canvas `on` motionNotifyEvent $ do
    (mouseX,mouseY) <- eventCoordinates
    t <- M.eventTime
    liftIO $ 
      changePos vPos var surf canvas mouseX mouseY
    liftIO $ logMsg0 ("Motion Time: " ++ s t)
    return False
  window `on` keyPressEvent $ tryEvent $ do
    key <- eventKeyName
    keyInput var sql surf canvas (glibToString key)
    liftIO $ updateCanvas1 var var2 canvas surf
    return ()
  canvas `on` buttonPressEvent $ tryEvent $ do
    (mouseX,mouseY) <- printMouse
    liftIO $ printPointer canvas
    liftIO $ printMVar var mouseX mouseY
    liftIO $ modifyMVar_ vPos (\_ -> 
      return (Press,mouseX,mouseY)) 
  canvas `on` buttonReleaseEvent $ tryEvent $ do
    (mouseX,mouseY) <- M.eventCoordinates
    m <- M.eventModifier
    b <- M.eventButton
    (cause,vPosX,vPosY) <- liftIO $ readMVar vPos
    liftIO $ release cause b var vPosX vPosY
  canvas `on` scrollEvent $ tryEvent $ do
    (mouseX,mouseY) <- M.eventCoordinates
    m <- M.eventModifier
    d <- M.eventScrollDirection
    t <- M.eventTime
    liftIO $ changeRef var d mouseX mouseY
    liftIO $ logMsg0 ("Scroll: " ++ s t ++ s mouseX ++
      s mouseY ++ s m ++ s d) 
  onDestroy window mainQuit
  onExpose canvas $ const (updateCanvas1 var var2 canvas surf)
  onExpose canvas2 $ const (updateCanvas2 var2 canvas2)
  set window [containerChild := canvas]
  widgetShowAll window
  timeoutAdd (timeoutFunc var var2 canvas surf) 50
  widgetShowAll window2
  getHistogram var sql surf var2
  mainGUI

