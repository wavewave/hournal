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
