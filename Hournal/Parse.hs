{-# LANGUAGE OverloadedStrings #-}

module Hournal.Parse where

import Control.Applicative hiding (many)

import qualified Data.Attoparsec as P 
import Data.Attoparsec.Char8 
import qualified Data.ByteString.Char8 as B hiding (map) 
import Data.ByteString.Internal (c2w,w2c)
import Data.ByteString.Lex.Double

-- import Data.Attoparsec.Incremental.Char8

import qualified Data.Iteratee as Iter
import qualified Data.ListLike as LL

import qualified Data.Attoparsec.Iteratee as AI
import Data.Word (Word8)

import Control.Monad.IO.Class

import Hournal.Type

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

{-
double :: Parser Double 
double = do x <- (many1 . satisfy . inClass) "0123456789.+-" 
            return $ read x 
-}  

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

--- Parser to Iteratee 

iter_xournal :: Iter.Iteratee B.ByteString IO Xournal
iter_xournal = AI.parserToIteratee parser_xournal 


read_xournal :: String -> IO Xournal 
read_xournal str =  Iter.fileDriver iter_xournal str 
{-
  bytestr <- B.readFile str
  
  let r = parse parser_xournal bytestr
  case r of 
    Partial _  -> return $ onlyresult (feed r B.empty)
    Done _  _  -> return $ onlyresult r
    Fail x y z -> do print x 
                     print y 
                     print z
                     return undefined  
-}
  
  
onlyresult (Done _ r) = r 

