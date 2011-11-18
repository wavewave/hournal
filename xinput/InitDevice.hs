{-# LANGUAGE ForeignFunctionInterface #-}

module InitDevice (
   initdevice
-- , extEventCanvas
 ) where

import Foreign.Ptr
import Foreign.C

foreign import ccall "c_initdevice.h initdevice" c_initdevice
  :: Ptr CInt -> IO ()

-- foreign import ccall "c_initdevice.h extEventCanvas" c_extEventCanvas
--  :: Ptr () -> IO ()

initdevice :: Ptr CInt -> IO () 
initdevice = c_initdevice

{-
extEventCanvas :: Ptr () -> IO ()
extEventCanvas = c_extEventCanvas
-}