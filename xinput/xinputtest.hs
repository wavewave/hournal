{-# LANGUAGE PackageImports, ForeignFunctionInterface, Rank2Types, ExistentialQuantification, 
             ScopedTypeVariables, FlexibleContexts #-}

module Main where

import Coroutine

import Type
import UI 
import InitDevice

import Graphics.UI.Gtk

import "mtl" Control.Monad.State

import "mtl" Control.Monad.Trans

import Data.IORef


{-newtype T = forall a. C a

newtype CoroutineIORef = forall i. forall o. forall m. forall a. CoroutineIORef { 
  runCoroutineIORef :: IORef (i -> CoroutineT i o m a)
  } 
-}


data AllCont = forall o a. (Show o)  => AllCont { runcont :: (String -> CoroutineT String o IO a) }



coroutineHandler :: IORef AllCont -> EventM a ()  
coroutineHandler contref = do 
  cont <- liftIO $ readIORef contref
  
  case cont of 
    AllCont fun -> do 
      r <- liftIO $ run $ fun "test"
      case r of 
        Result a -> do 
          let newone = \_ -> CoroutineT $ (return r)  -- (Result a :: Result String o IO b)
          liftIO $ writeIORef contref (AllCont newone)
          return ()
        Yield o ncont -> do 
          liftIO $ putStrLn $ show o
          liftIO $ writeIORef contref (AllCont ncont)  
          return ()
  
  where run exp = runCoroutineT exp 



data T = forall o. (Show o) => MkT o


f :: T -> String
f (MkT o) = show o

reportPoint msg = do (x,y) <- eventCoordinates 
                     liftIO $ putStrLn $ msg ++ (show (x,y)) 
                  



myPenButtonClicked pentyperef   = do liftIO $ modifyIORef pentyperef (const Pen) 
                                     liftIO $ putStrLn "pen button clicked" 

myEraseButtonClicked pentyperef = do liftIO $ modifyIORef pentyperef (const Erase)
                                     liftIO $ putStrLn "erase button clicked"

showMode pentype = 
  case pentype of 
    Pen   -> liftIO $ putStrLn "in Pen mode" 
    Erase -> liftIO $ putStrLn "in Erase mode"

myButtonPressed statusref pentyperef = do 
  status  <- liftIO $ readIORef statusref 
  pentype <- liftIO $ readIORef pentyperef
  
  showMode pentype
                
  case status of   
    StandBy    -> do reportPoint "press:standby" 
                     liftIO $ modifyIORef statusref (const Drawing) 
    Drawing    -> reportPoint "press:drawing"
 


myMouseMoved statusref pentyperef = do 
  status <- liftIO $ readIORef statusref 
  pentype <- liftIO $ readIORef pentyperef
  
  showMode pentype
  
  case status of   
    StandBy    -> reportPoint "move:standby" 
    Drawing -> reportPoint "move:drawing"

myButtonReleased statusref pentyperef = do 
  status <- liftIO $ readIORef statusref
  pentype <- liftIO $ readIORef pentyperef
  
  showMode pentype

  case status of 
    StandBy    -> reportPoint "release:standby"
    Drawing -> do reportPoint "release:drawing"
                  liftIO $ modifyIORef statusref (const StandBy)
               
              

testCoroutine :: CoroutineT String String IO ()
testCoroutine = do 
  i <- yield "haha" 
  liftIO $ putStrLn i
  yield "babo"
  yield "what are you doing"
  return ()

main :: IO () 
main = do 
  
  statusref <- newIORef StandBy
  pentyperef <- newIORef Pen
  
  contref <- newIORef (AllCont (const testCoroutine)) 
  
  initGUI
  initdevice
  (window,buttonpen,buttonerase,canvas) <- initWindows 
 
  widgetAddEvents canvas [ButtonMotionMask]
  
  canvas `on` buttonPressEvent   $ tryEvent $ coroutineHandler contref
    -- myButtonPressed  statusref pentyperef
  canvas `on` motionNotifyEvent  $ tryEvent $ myMouseMoved     statusref pentyperef
  canvas `on` buttonReleaseEvent $ tryEvent $ myButtonReleased statusref pentyperef
                                               
    
  onClicked buttonpen   $ myPenButtonClicked   pentyperef
  onClicked buttonerase $ myEraseButtonClicked pentyperef
   
  widgetSetExtensionEvents canvas [ExtensionEventsNone]  
  

  onDestroy window mainQuit
  
  widgetShowAll window
  
  mainGUI
  
  putStrLn "test ended"
  
