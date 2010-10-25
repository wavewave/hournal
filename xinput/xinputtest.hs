{-# LANGUAGE PackageImports, Rank2Types, ExistentialQuantification, 
             ScopedTypeVariables, FlexibleContexts, GADTs #-}

module Main where

import Coroutine

import Type
import UI 
import InitDevice

import Event 

import Graphics.UI.Gtk

import "mtl" Control.Monad.State

import "mtl" Control.Monad.Trans

import Data.IORef

testCoroutine :: CoroutineT EventResult String IO ()
testCoroutine = do 
--  untilCoroutine (\(t,_)->t == ButtonPress) standby
  return ()
  yield "babo"
  yield "what are you doing"
  return ()

{-
standby :: CoroutineT EventResult String IO () 
standby = yield "standby phase"

untilCoroutine :: (o -> Bool) -> CoroutineT i String IO a -> CoroutineT i String IO a
untilCoroutine pred coroutine = do 
  t <- coroutine 
  if pred t then return () else untilCoroutine pred coroutine

-}

main :: IO () 
main = do 

  contref <- newIORef (AllCont (const testCoroutine)) 
  
  initGUI
  initdevice
  (window,buttonpen,buttonerase,canvas) <- initWindows 
 
  widgetAddEvents canvas [ButtonMotionMask]
  
  canvas `on` buttonPressEvent   $ tryEvent $ coroutineHandler GADTButtonPress   contref
--  canvas `on` motionNotifyEvent  $ tryEvent $ coroutineHandler GADTMouseMove     contref
--  canvas `on` buttonReleaseEvent $ tryEvent $ coroutineHandler GADTButtonRelease contref

                                               
    
--  onClicked buttonpen   $ myPenButtonClicked   pentyperef
--  onClicked buttonerase $ myEraseButtonClicked pentyperef
   
  widgetSetExtensionEvents canvas [ExtensionEventsNone]  
  onDestroy window mainQuit
  widgetShowAll window
  
  mainGUI
  
