module Main where 

import Graphics.UI.Gtk
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk.Gdk.EventM

import System.IO
import System.Environment 

import Data.IORef

import Hournal.Type
import Hournal.Parse

                                    

drawOneStroke :: Stroke -> Render ()
drawOneStroke ((x0,y0) : xs)  = do 
  moveTo x0 y0
  mapM_ f xs 
    where f (x,y) = lineTo x y 

main :: IO () 
main = do 
  args <- getArgs 
  
  myxoj <- read_xournal $ args !! 0
  
  pagenumref <- newIORef (0 :: Int )
  
  initGUI
  window <- windowNew 
  hbox  <- hBoxNew False 0 
  vbox  <- vBoxNew False 0
  
  
  buttonleft <- buttonNewWithLabel "<"
  buttonright <- buttonNewWithLabel ">"
   
  canvas <- drawingAreaNew
  
  set window [containerChild := vbox ]
 
  boxPackStart hbox buttonleft  PackGrow 0 
  boxPackStart hbox buttonright PackGrow 0

  boxPackEnd vbox hbox   PackNatural 0 
  boxPackEnd vbox canvas PackGrow 0 
  
  
  
  
  canvas `on` sizeRequest $ return (Requisition 40 40)
  ctxt <- cairoCreateContext Nothing 
  onExpose canvas $ const (updateCanvas canvas myxoj pagenumref)
  
  onClicked buttonleft $ do modifyIORef pagenumref (\x->x-1) 
                            updateCanvas canvas myxoj pagenumref 
                            return ()
  onClicked buttonright $do modifyIORef pagenumref (+1) 
                            updateCanvas canvas myxoj pagenumref
                            return ()
  
  
  widgetShowAll window
  onDestroy window mainQuit
  
  mainGUI 
  
  putStrLn "test ended"
  
  
updateCanvas :: DrawingArea -> Xournal -> IORef Int -> IO Bool
updateCanvas canvas xoj pagenumref = do 
  pagenumval <- readIORef pagenumref
  win <- widgetGetDrawWindow canvas
  (w',h') <- widgetGetSize canvas
  
  let totalnumofpages = (length . xoj_pages) xoj
  
  let currpagenum = if pagenumval >= totalnumofpages 
                    then totalnumofpages - 1
                    else if pagenumval < 0 
                         then 0 
                         else pagenumval
                         
  writeIORef pagenumref currpagenum 
  
  let currpage = ((!!currpagenum).xoj_pages) xoj
  let strokes = (layer_strokes . (!!0) . page_layers ) currpage 
      (Dim w h) = page_dim currpage
  
  renderWithDrawable win $ do 
    scale (realToFrac w' / w) (realToFrac h' / h)

    setSourceRGB 1 1 1 
    rectangle 0 0 w h 
    fill

    setSourceRGB 0 0 0
    setLineWidth 1
    setLineCap LineCapRound
    setLineJoin LineJoinRound

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