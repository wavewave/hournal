{-# LANGUAGE ForeignFunctionInterface #-}

module InitDevice (
   initdevice 
  ) where


foreign import ccall "initdevice.h initdevice" c_initdevice
  :: IO ()


initdevice :: IO () 
initdevice = c_initdevice