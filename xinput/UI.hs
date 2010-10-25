module UI where

import Graphics.UI.Gtk


initWindows = do 
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
  
  
  set window [containerChild := vbox ]

  return (window, buttonpen, buttonerase, canvas)