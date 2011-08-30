module Main where 

import System.Environment 

import Graphics.UI.Gtk
import Graphics.Rendering.Cairo

import Data.IORef

import Text.Xournal.Type
import Text.Xournal.Parse
import Graphics.Xournal.Render
                                    
refresh_xournal :: IORef Xournal -> FilePath -> IO () 
refresh_xournal xojref str = do 
  xoj <- read_xournal str 
  writeIORef xojref $! xoj

  
  
updateCanvas :: DrawingArea -> IORef Xournal -> IORef Int -> IO Bool
updateCanvas canvas xojref pagenumref = do 
  pagenumval <- readIORef pagenumref 
  xoj <- readIORef xojref 
  
  let totalnumofpages = (length . xoj_pages) xoj
  
  let currpagenum = if pagenumval >= totalnumofpages 
                    then totalnumofpages - 1
                    else if pagenumval < 0 
                         then 0 
                         else pagenumval
  writeIORef pagenumref currpagenum 
  let currpage = ((!!currpagenum).xoj_pages) xoj
  
  win <- widgetGetDrawWindow canvas
  (w',h') <- widgetGetSize canvas
  let (Dim w h) = page_dim currpage
  renderWithDrawable win $ do
    scale (realToFrac w' / w) (realToFrac h' / h)
    cairoDrawPage currpage
  return True 

main :: IO () 
main = do 
  args <- getArgs 
  if length args /= 1
     then error "Usage : ./hournal filename.xoj"
     else return ()
          
  let filename = args !! 0  
--  myxoj <- read_xournal filename 
  
  pagenumref <- newIORef (0 :: Int )
  xojref     <- newIORef (undefined :: Xournal)
  refresh_xournal xojref filename 

--  ctxt <- cairoCreateContext Nothing 

{-
  xoj <- readIORef xojref
 
  withSVGSurface "test.svg" 640 480 (\s -> renderWith s (cairoDrawing xoj 0 )) -}

  initGUI
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
  
  mainGUI  
   
  putStrLn "test ended"
