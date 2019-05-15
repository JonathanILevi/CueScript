{-# LANGUAGE ForeignFunctionInterface #-}
module VLCBackend where

import Foreign
import Foreign.C.Types
import Foreign.C.String

foreign import ccall "play" c_play :: CString -> IO ()
play :: String -> IO ()
play name = c_play =<< newCString name

foreign import ccall "setup" setup :: IO ()
foreign import ccall "cleanUp" cleanUp :: IO ()

