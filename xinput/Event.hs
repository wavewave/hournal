{-# LANGUAGE PackageImports, Rank2Types, ExistentialQuantification, 
             ScopedTypeVariables, FlexibleContexts, GADTs #-}

module Event where

import Type
import Graphics.UI.Gtk
import Coroutine

import "mtl" Control.Monad.Trans

import Data.IORef


data AllCont = forall o a. (Show o)  => 
               AllCont { runcont :: (EventResult -> CoroutineT EventResult o IO a) }


data EventGADT a where
  GADTButtonPress   :: EventGADT EButton
  GADTButtonRelease :: EventGADT EButton 
  GADTMouseMove     :: EventGADT EMotion   


coroutineHandler :: EventGADT a -> IORef AllCont -> EventM a ()  
coroutineHandler GADTButtonPress contref = do 
  (x,y) <- eventCoordinates
  let eventdata = (ButtonPress,(x,y))
  action contref eventdata
coroutineHandler GADTButtonRelease contref = do 
  (x,y) <- eventCoordinates
  let eventdata = (ButtonRelease,(x,y))
  action contref eventdata
coroutineHandler GADTMouseMove contref = do 
  (x,y) <- eventCoordinates
  let eventdata = (MouseMove,(x,y))
  action contref eventdata
      
run exp = runCoroutineT exp 
action contref eventdata = do 
  cont <- liftIO $ readIORef contref
  case cont of 
    AllCont fun -> do 
      r <- liftIO $ run $ fun eventdata           
      case r of 
        Result a -> do 
          let newone = \_ -> CoroutineT $ (return r)  
          liftIO $ writeIORef contref (AllCont newone)
          return ()
        Yield o ncont -> do 
          liftIO $ putStrLn $ show o
          liftIO $ writeIORef contref (AllCont ncont)  
          return ()
