{-# LANGUAGE OverloadedStrings #-}

import Graphics.UI.Gtk
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk.Gdk.EventM

import System.IO
import System.Environment 

import Data.IORef

import Control.Applicative hiding (many)

import qualified Data.Attoparsec as P 
import Data.Attoparsec.Char8 
import qualified Data.ByteString.Char8 as B hiding (map) 
import Data.ByteString.Internal (c2w,w2c)
import Data.ByteString.Lex.Double

-- import Data.Attoparsec.Incremental.Char8
type Title = String 
type Stroke = [(Double,Double)]
data Dimension = Dim { dim_width :: Double, dim_height :: Double }
               deriving Show

data Background = Background { bkg_string :: String }
                deriving Show 

data Xournal = Xournal { xoj_title :: Title, xoj_pages :: [Page] }
             deriving Show 
data Page = Page { page_dim :: Dimension
                 , page_bkg :: Background 
                 , page_layers :: [Layer] }
          deriving Show 
data Layer = Layer { layer_strokes :: [Stroke] } 
           deriving Show 

skipSpaces :: Parser () 
skipSpaces = P.satisfy isHorizontalSpace *> P.skipWhile isHorizontalSpace

trim_starting_space :: Parser ()
trim_starting_space = do try endOfInput
                         <|> (many . satisfy . inClass ) " \n" *> return () 
                
langle :: Parser Char 
langle = char '<'

rangle :: Parser Char 
rangle = char '>'

xmlheader :: Parser String
xmlheader = string "<?" *> (many . satisfy . notInClass) "?>" <* string "?>"
 
            
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
  

onestroke :: Parser Stroke 
onestroke =  do trim
                stroketagopen
                coordlist <- many $ do trim_starting_space 
                                       x <- double
                                       skipSpace 
                                       y <- double
                                       skipSpace 
                                       return (x,y)  
                stroketagclose
                return coordlist


trim = trim_starting_space

parser_xournal :: Parser Xournal
parser_xournal = do trim
                    xmlheader
                    trim
                    xournal
                  

xournal :: Parser Xournal 
xournal = do trim 
             xournalheader
             trim
             t <- title
             trim
             pgs <- many1 page
             trim
             xournalclose
             
             return $ Xournal  t pgs
             
page :: Parser Page 
page = do trim 
          dim <- pageheader
          trim 
          bkg <- background 
          trim 
          layers <- many1 layer
          trim
          pageclose
          return $ Page dim bkg layers
         
          
layer :: Parser Layer
layer = do trim
           layerheader
           trim
           strokes <- many1 onestroke
           trim
           layerclose
           return $ Layer strokes


title :: Parser String 
title = do trim 
           titleheader
           str <- (many . satisfy . notInClass ) "<"
           titleclose
           return str 
          
titleheader = string "<title>"
titleclose = string "</title>"


xournalheader = xournalheaderstart *> (many . satisfy . notInClass ) ">" <* xournalheaderend
xournalheaderstart = string "<xournal"
xournalheaderend = char '>'
xournalclose =  string "</xournal>"

pageheader :: Parser Dimension 
pageheader = do pageheaderstart  
                trim
                string "width=" 
                char '"'
                w <- double
                char '"'
                trim 
                string "height="
                char '"' 
                h <- double 
                char '"'
                ( many . satisfy . notInClass ) ">" 
                pageheaderend
                return $ Dim w h
                 
pageheaderstart = string "<page"
pageheaderend = char '>'
                  
pageclose = string "</page>"

layerheader = string "<layer>"

layerclose = string "</layer>"

background :: Parser Background 
background = do trim
                backgroundheader
                x <- ( many . satisfy . notInClass ) "/>"
                backgroundclose
                return $ Background x 
                
backgroundheader = string "<background"
backgroundclose = string "/>"

read_xournal :: String -> IO Xournal 
read_xournal str = do 
  bytestr <- B.readFile str
  
  let r = parse parser_xournal bytestr
  case r of 
    Partial _  -> return $ onlyresult (feed r B.empty)
    Done _  _  -> return $ onlyresult r
    Fail x y z -> do print x 
                     print y 
                     print z
                     return undefined  

  
  
onlyresult (Done _ r) = r 

                                    

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