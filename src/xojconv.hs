module Main where 

import System.IO
import System.Environment 

import Graphics.UI.Gtk
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk.Gdk.EventM


import Data.IORef

import Text.Xournal.Type 
import Text.Xournal.Parse

                                    

drawOneStroke :: Stroke -> Render ()
drawOneStroke ((x0,y0) : xs)  = do 
  moveTo x0 y0
  mapM_ f xs 
    where f (x,y) = lineTo x y 


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


cairoDrawPage :: Page -> Render ()
cairoDrawPage page = do 
  let strokes = (layer_strokes . (!!0) . page_layers ) page 
      (Dim w h) = page_dim page

  setSourceRGB 1 1 1 
  rectangle 0 0 w h 
  fill

  setSourceRGB 0 0 0
  setLineWidth 1
  setLineCap LineCapRound
  setLineJoin LineJoinRound

  mapM_ drawOneStroke strokes
  stroke


main :: IO () 
main = do 
  args <- getArgs 
  if length args /= 1
     then error "Usage : ./hournal filename.xoj"
     else return ()
          
  let filename = args !! 0  
  xoj <- read_xournal filename 
 
  let pages = xoj_pages xoj
      names = map (\x -> "test" ++ show x ++ ".png") [1..] 
      namePages = zip names pages 
  let Dim w h = page_dim (head pages)

  putStrLn $ " w = " ++ show w
  putStrLn $ " h = " ++ show h  

--  let svgoutfn x = withSVGSurface (fst x) w h (\s -> renderWith s (cairoDrawPage (snd x)))
  
  let pngoutfn x = do 
        sfc <- createImageSurface FormatARGB32 (floor w) (floor h) 
        renderWith sfc (cairoDrawPage (snd x))
        surfaceWriteToPNG sfc (fst x) 

  mapM_ pngoutfn namePages

{-  initGUI
  window <- windowNew 
  hbox  <- hBoxNew False 0 
  vbox  <- vBoxNew False 0
  
  buttonleft    <- buttonNewWithLabel "<"
  buttonright   <- buttonNewWithLabel ">"
  buttonrefresh <- buttonNewWithLabel "Refresh"  
  buttonquit    <- buttonNewWithLabel "Quit"

  canvas <- drawingAreaNew
  
  set window [containerChild := vbox ]
 
  boxPackStart hbox buttonleft    PackGrow 0 
  boxPackStart hbox buttonright   PackGrow 0
  boxPackStart hbox buttonrefresh PackGrow 0
  boxPackStart hbox buttonquit    PackGrow 0 
  
  boxPackEnd vbox hbox   PackNatural 0 
  boxPackEnd vbox canvas PackGrow 0 
 
  canvas `on` sizeRequest $ return (Requisition 40 40)
  -- ctxt <- cairoCreateContext Nothing 
  onExpose canvas $ const (updateCanvas canvas xojref pagenumref)
  
  onClicked buttonleft    $ do modifyIORef pagenumref (\x->x-1) 
                               updateCanvas canvas xojref pagenumref 
                               return ()
  onClicked buttonright   $ do modifyIORef pagenumref (+1) 
                               updateCanvas canvas xojref pagenumref
                               return ()
  onClicked buttonrefresh $ do refresh_xournal xojref filename 
                               updateCanvas canvas xojref pagenumref
                               putStrLn "refresh button clicked"
                               return ()
  onClicked buttonquit    mainQuit           
                    
  
  widgetShowAll window
  onDestroy window mainQuit
  
  mainGUI  -}
   
  putStrLn "test ended"
