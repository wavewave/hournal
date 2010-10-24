{-# LANGUAGE PackageImports, ForeignFunctionInterface #-}

module Main where



import Graphics.UI.Gtk

import "mtl" Control.Monad.State

import "mtl" Control.Monad.Trans

import Data.IORef


foreign import ccall "initdevice.h initdevice" c_initdevice
  :: IO ()

reportPoint msg = do (x,y) <- eventCoordinates 
                     liftIO $ putStrLn $ msg ++ (show (x,y)) 
                  

-- type PenDrawingMode a = State Int a 

data Status = StandBy | Drawing 
data PenStatus = Pen | Erase

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
               
              

main :: IO () 
main = do 
  
  statusref <- newIORef StandBy
  pentyperef <- newIORef Pen
  
  initGUI
  window <- windowNew
  
  
  vbox <- vBoxNew False 0 
  hbox <- hBoxNew False 0 
  
  buttonpen   <- buttonNewWithLabel "Pen"
  buttonerase <- buttonNewWithLabel "Erase"
  canvas <- drawingAreaNew
  
  boxPackStart hbox buttonpen   PackGrow 0 
  boxPackStart hbox buttonerase PackGrow 0 
  
  boxPackEnd vbox hbox   PackNatural 0 
  boxPackEnd vbox canvas PackGrow    0 
  
  c_initdevice
  
  set window [containerChild := vbox ]
  
 
  widgetAddEvents canvas [ButtonMotionMask]
  
  canvas `on` buttonPressEvent   $ tryEvent $ myButtonPressed  statusref pentyperef
  canvas `on` motionNotifyEvent  $ tryEvent $ myMouseMoved     statusref pentyperef
  canvas `on` buttonReleaseEvent $ tryEvent $ myButtonReleased statusref pentyperef
                                               
    
  onClicked buttonpen   $ myPenButtonClicked   pentyperef
  onClicked buttonerase $ myEraseButtonClicked pentyperef
   
  widgetSetExtensionEvents canvas [ExtensionEventsNone]  
  
--  (\(Widget arg1) args -> withForeignPtr arg1 $ \argPtr1 ->gtk_widget_set_extension_events argPtr1 arg2) (toWidget canvas) ((fromIntegral . fromFlags) [ExtensionEventsCursor])
  
  events <- widgetGetEvents canvas
  putStrLn $ show events
  
  modes <- widgetGetExtensionEvents canvas
  putStrLn $ show modes 


  onDestroy window mainQuit
  
  widgetShowAll window
  
  mainGUI
  
  putStrLn "test ended"
  
