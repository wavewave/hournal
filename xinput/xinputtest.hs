{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, GADTs #-}

module Main where

import Graphics.Rendering.Cairo
-- import Type
import UI 
import InitDevice
import Control.Monad.IO.Class
import Control.Applicative
-- import Event 

import Graphics.UI.Gtk
import Control.Monad.Reader 

import Data.Int 
import Data.IORef
import Foreign.Storable
import Foreign.Ptr 
import Foreign.Marshal.Array
import Foreign.C

data Status = Drawing | NonDrawing

main :: IO () 
main = do 
  pointref <- newIORef ((0,0) :: (Double,Double))
  stateref <- newIORef NonDrawing
  (stylusdevptr :: Ptr CInt) <- mallocArray 100 
  
  initGUI
  initdevice stylusdevptr
  stylusdev <- peekByteOff stylusdevptr 0
  
  (window,buttonpen,buttonerase,canvas) <- initWindows 
 
  widgetAddEvents canvas [PointerMotionMask,ButtonMotionMask]
  canvas `on` sizeRequest $ return (Requisition 480 640)
  canvas `on` buttonPressEvent $ tryEvent $ do
    ptr <- ask 
    liftIO $ do 
      (ty :: Int32) <- peek (castPtr ptr) 
      (x :: Double) <- peekByteOff ptr 16
      (y :: Double) <- peekByteOff ptr 24
      (device :: CInt ) <- peekByteOff ptr 44
      if device == stylusdev 
        then do 
          (ptrax :: Ptr CDouble ) <- peekByteOff ptr 32
          (wacomx :: Double) <- peekByteOff ptrax 0
          (wacomy :: Double) <- peekByteOff ptrax 8
          point <- readIORef pointref 
          if wacomx < 1.0 && wacomx > 0.0 && wacomy < 1.0 && wacomy > 0.0 
            then do 
              writeIORef stateref Drawing
              writeIORef pointref (wacomx,wacomy)
            else return ()
        else return ()
   
  canvas `on` buttonReleaseEvent $ tryEvent $ do 
    liftIO $ writeIORef stateref NonDrawing

  canvas `on` motionNotifyEvent  $ tryEvent $ do 
    st <- liftIO $ readIORef stateref
    case st of 
      NonDrawing -> return () 
      Drawing -> do 
        ptr <- ask 
        liftIO $ do 
          (ty :: Int32) <- peek (castPtr ptr) 
          (x :: Double) <- peekByteOff ptr 16
          (y :: Double) <- peekByteOff ptr 24
          (device :: CInt ) <- peekByteOff ptr 44
          if device == stylusdev 
            then do 
              (ptrax :: Ptr CDouble ) <- peekByteOff ptr 32
              (wacomx :: Double) <- peekByteOff ptrax 0
              (wacomy :: Double) <- peekByteOff ptrax 8
              point <- readIORef pointref 
              if wacomx < 1.0 && wacomx > 0.0 && wacomy < 1.0 && wacomy > 0.0 
                then do putStrLn $ show point ++ show (wacomx,wacomy)
                        win <- widgetGetDrawWindow canvas
                        (w',h') <- widgetGetSize canvas
                        (x0,y0) <- drawWindowGetOrigin win
                        let (x0' :: Double ,y0' :: Double) = (fromIntegral x0, fromIntegral y0)
                        screen <- widgetGetScreen canvas
                        (ws,hs) <- (,) <$> screenGetWidth screen <*> screenGetHeight screen
                        -- print (w',h')
                        -- print (ws,hs)
                        -- print (x0',y0')
                        let f (x,y) = (fromIntegral ws*x-x0',fromIntegral hs*y-y0')
                        -- print (f point)
                        renderWithDrawable win $ do 
                          setLineWidth 1.0 
                          uncurry moveTo (f point)
                          uncurry lineTo (f (wacomx,wacomy))
                          stroke
               
                        writeIORef pointref (wacomx,wacomy)
                else return ()
          -- print (wacomx, wacomy)
          -- (w :: Double) <- peekByteOff ptr 48 
          -- putStrLn $ show (ty,x,y,device,w) 
            else return ()
    
  widgetSetExtensionEvents canvas [ExtensionEventsAll]  
  onDestroy window mainQuit
  widgetShowAll window
  
  mainGUI
  
  
  
