{-# LANGUAGE OverloadedStrings #-}

import Graphics.UI.Gtk
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk.Gdk.EventM

import System.IO

import Control.Applicative hiding (many)

import qualified Data.Attoparsec as P 
import Data.Attoparsec.Char8 
import qualified Data.ByteString.Char8 as B hiding (map) 
import Data.ByteString.Internal (c2w,w2c)
import Data.ByteString.Lex.Double

-- import Data.Attoparsec.Incremental.Char8

skipSpaces :: Parser () 
skipSpaces = P.satisfy isHorizontalSpace *> P.skipWhile isHorizontalSpace

trim_starting_space :: Parser ()
trim_starting_space = do try endOfInput
                         <|> (many . satisfy . inClass ) " \n" *> return () 
                
langle :: Parser Char 
langle = char '<'

rangle :: Parser Char 
rangle = char '>'

header :: Parser String
header = do string "<?" 
            result <- headercontent
            return result
            
oneelem = try (string "?>" >> return Nothing )
          <|> (many1 (notChar '?') >>= return . Just ) 
          <|> do a <- anyChar
                 return (Just [a])
                 
headercontent :: Parser String 
headercontent = mymaybeWhile oneelem >>= return . concat 

mymaybeWhile :: Parser (Maybe a) -> Parser [a]
mymaybeWhile oneelem = do x <- oneelem
                          case x of 
                            Just t -> do ts <- mymaybeWhile oneelem
                                         return (t:ts)  
                            Nothing -> return []
                            
              

stroketagopen :: Parser B.ByteString 
stroketagopen = string "<stroke" *> trim_starting_space *> P.takeWhile (/= (c2w '>')) <* char '>'

stroketagclose :: Parser B.ByteString 
stroketagclose = string "</stroke>"


double :: Parser Double 
double = do x <- (many1 . satisfy . inClass) "0123456789.+-" 
            return $ read x 
  
 --  numeric "Double" readDouble

onestroke :: Parser Stroke 
onestroke =  do stroketagopen
                coordlist <- many (do { trim_starting_space ; x <- double ; skipSpace ; y <- double ; skipSpace ; return (x,y) }) 
                  -- str <- P.takeWhile (/= (c2w '<'))
                stroketagclose
                return coordlist

--onecoord :: B.ByteString -> (Double, Double) 
--onecoord = do x <- readDouble
--              y <- readDouble
--              return (x,y)


parser_xournal :: Parser [Stroke]
parser_xournal = do trim_starting_space 
                    -- header
                    many (onestroke <* trim_starting_space)
                  


read_xournal :: String -> IO [Stroke] 
read_xournal str = do 
  bytestr <- B.readFile str
  
  let r = parse parser_xournal bytestr
  let s = case r of 
        Partial _  -> onlyresult (feed r B.empty)
        Done _  _  -> onlyresult r
        Fail _ _ _ -> onlyresult r  

  
  return s
  
onlyresult (Done _ r) = r 

                                    


type Stroke = [(Double,Double)] 

drawOneStroke :: Stroke -> Render ()
drawOneStroke ((x0,y0) : xs)  = do 
  moveTo x0 y0
  mapM_ f xs 
    where f (x,y) = lineTo x y 

main :: IO () 
main = do 
  
  strokes <- read_xournal  "test2.xoj"
--  print r1
--  print str2
--  ((w,h),strokes) <- readxoj "test.xoj"
  
--  print $ w
--  print $ h
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