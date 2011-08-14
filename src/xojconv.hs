module Main where 

import System.IO
import System.Environment 

import Graphics.UI.Gtk
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk.Gdk.EventM


import Data.IORef

import Text.Xournal.Type 
import Text.Xournal.Parse
import Text.Xournal.Predefined 

import Graphics.Xournal.Render 

import qualified Data.Map as M

import Debug.Trace

{-
drawOneStroke :: Stroke -> Render ()
drawOneStroke s = do 
  trace (stroke_color s) $ return ()
  case M.lookup (stroke_color s) predefined_pencolor of
    Just (r,g,b,a) -> setSourceRGBA r g b a 
    Nothing -> setSourceRGBA 0 0 0 1
  setLineWidth (stroke_width s) 
  drawOneStrokeCurve (stroke_data s)
  stroke

drawOneStrokeCurve :: [(Double,Double)] -> Render ()
drawOneStrokeCurve ((x0,y0) : xs) = do 
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


cairoDrawBackground :: Page -> Render () 
cairoDrawBackground page = do 
  let Background typ col sty = page_bkg page
      Dim w h = page_dim page  
  let c = M.lookup col predefined_bkgcolor  
  case c of 
    Just (r,g,b,a) -> setSourceRGB r g b 
    Nothing        -> setSourceRGB 1 1 1 
  rectangle 0 0 w h 
  fill
  cairoDrawRuling w h sty

cairoDrawRuling :: Double -> Double ->  String -> Render () 
cairoDrawRuling w h style = do
  let drawHorizRules = do 
      let (r,g,b,a) = predefined_RULING_COLOR         
      setSourceRGBA r g b a 
      setLineWidth predefined_RULING_THICKNESS
      let drawonerule y = do 
            moveTo 0 y 
            lineTo w y
            stroke  
      mapM_ drawonerule [predefined_RULING_TOPMARGIN
                        ,predefined_RULING_TOPMARGIN+predefined_RULING_SPACING
                        ..
                        h-1]
  case style of 
    "plain" -> return () 
    "lined" -> do 
      drawHorizRules
      let (r2,g2,b2,a2) = predefined_RULING_MARGIN_COLOR
      setSourceRGBA r2 g2 b2 a2 
      setLineWidth predefined_RULING_THICKNESS
      moveTo predefined_RULING_LEFTMARGIN 0 
      lineTo predefined_RULING_LEFTMARGIN h
      stroke
    "ruled" -> drawHorizRules 
    "graph" -> do 
      let (r3,g3,b3,a3) = predefined_RULING_COLOR 
      setSourceRGBA r3 g3 b3 a3 
      setLineWidth predefined_RULING_THICKNESS
      let drawonegraphvert x = do 
            moveTo x 0 
            lineTo x h
            stroke  
      let drawonegraphhoriz y = do 
            moveTo 0 y
            lineTo w y
            stroke
      mapM_ drawonegraphvert  [0,predefined_RULING_GRAPHSPACING..w-1] 
      mapM_ drawonegraphhoriz [0,predefined_RULING_GRAPHSPACING..h-1]
    _ -> return ()     

cairoDrawPage :: Page -> Render ()
cairoDrawPage page = do 
  let strokes = (layer_strokes . (!!0) . page_layers ) page 
      (Dim w h) = page_dim page
  cairoDrawBackground page
  setSourceRGB 0 0 0
  setLineWidth 1
  setLineCap LineCapRound
  setLineJoin LineJoinRound

  mapM_ drawOneStroke strokes
  stroke


-}

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
   
  putStrLn "test ended"
