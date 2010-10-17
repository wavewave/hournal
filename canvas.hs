import Graphics.UI.Gtk
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk.Gdk.EventM

import System.IO

type Stroke = [(Double,Double)] 

matchMarker :: String -> String -> (Bool,String)  
matchMarker [] xs = (True, xs) 
matchMarker (c:cs) (x:xs) = if c == x 
                            then matchMarker cs xs 
                            else (False, (x:xs) ) 

dropUntilMarker :: String -> String -> (String,String) 
dropUntilMarker [] xs = ([],xs)
dropUntilMarker cs [] = ([],[])
dropUntilMarker mkr@(c:cs) str = let str' = dropWhile (/= c) str 
                                     (ismatched,str'') = matchMarker mkr str'  
                                 in  if ismatched 
                                     then (mkr,str'') 
                                     else dropUntilMarker mkr (tail str'')

--readheader :: String -> (String, String) 
--readheader str = let str2 = dropWhile (/= '<') str
--                 in if (str2 !! 1) == '?' 
--                    then takeWhile (/= '?') str2 
                            
readheadertest str = dropUntilMaker "<?" str


readxojtest :: String

readxoj :: String -> IO ((Double,Double),[Stroke])
readxoj filename = do 
  handle <- openFile filename ReadMode
  str <- hGetContents handle
  let strs = lines str
      pagesizestr = words (strs !! 0) 
  
      width = read $ pagesizestr !! 0  
      height = read $ pagesizestr !! 1
  
      strokesstr = tail strs
      
      strokes = map (parseOneStroke.words) strokesstr 
  
  
  return ((width,height),strokes)

parseOneStroke :: [String] -> Stroke
parseOneStroke [] = [] 
parseOneStroke (x:y:xs) = (read x, read y) : parseOneStroke xs 

drawOneStroke :: Stroke -> Render ()
drawOneStroke ((x0,y0) : xs)  = do 
  moveTo x0 y0
  mapM_ f xs 
    where f (x,y) = lineTo x y 

main :: IO () 
main = do 
  
  ((w,h),strokes) <- readxoj "test.xoj"
  
  print $ w
  print $ h
--  print oneline

  initGUI
  window <- windowNew 
  canvas <- drawingAreaNew
  canvas `on` sizeRequest $ return (Requisition 40 40)
  ctxt <- cairoCreateContext Nothing 
  onExpose canvas $ const (updateCanvas canvas strokes)
  
  set window [containerChild := canvas ]
  
  
  widgetShowAll window
  onDestroy window mainQuit
  
  mainGUI
  
  
updateCanvas :: DrawingArea -> [Stroke] -> IO Bool
updateCanvas canvas strokes = do 
  win <- widgetGetDrawWindow canvas
  (width',height') <- widgetGetSize canvas
  
  let width  = realToFrac width'
      height = realToFrac height'

--  print width
--  print height
  renderWithDrawable win $ do 
    setSourceRGB 1 0 0
    setLineWidth 1
    setLineCap LineCapRound
    setLineJoin LineJoinRound

--    moveTo 30 30
--    lineTo (width-30) (height-30)
--    lineTo (width-30) 30
--    lineTo 30 (height-30)
---    stroke
    
    

  
  
--    drawCircle 0.0 0.0 width 
    
    mapM_ drawOneStroke strokes
    stroke
  return True



keepState render = do 
  save
  render
  restore

mystroke =
  keepState $ do 
    setSourceRGBA 1 1 0 0.7
    stroke

fillStroke = do 
  fillPreserve
  mystroke 
  
drawCircle x y r = do 
  arc x y r 0 (2*pi)
  fillStroke