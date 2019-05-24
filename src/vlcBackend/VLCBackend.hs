{-# LANGUAGE ForeignFunctionInterface #-}
module VLCBackend where

import Foreign
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String


type Instance	= (ForeignPtr Libvlc_Instance)
type Media	= (ForeignPtr Libvlc_Media)
type MediaPlayer	= (ForeignPtr Libvlc_Media_Player)


newInstance :: IO Instance
newInstance = newForeignPtr libvlc_release_ptr =<< libvlc_new 0 nullPtr

newMediaFromPath :: Instance -> String -> IO Media
newMediaFromPath fi name = withForeignPtr fi (\i ->
		newForeignPtr libvlc_media_release_ptr =<< libvlc_media_new_path i =<< newCString name
	)

newMediaPlayerFromMedia :: Media -> IO MediaPlayer
newMediaPlayerFromMedia fm = withForeignPtr fm (\m ->
		newForeignPtr libvlc_media_player_release_ptr =<< libvlc_media_player_new_from_media m
	)

mediaPlayerPlay :: MediaPlayer -> IO ()
mediaPlayerPlay fp = withForeignPtr fp (\p ->
		libvlc_media_player_play p
	)


playFromPath :: Instance -> String -> IO ()
playFromPath inst name = mediaPlayerPlay =<< newMediaPlayerFromMedia =<< newMediaFromPath inst name




data Libvlc_Instance
data Libvlc_Media
data Libvlc_Media_Player

--Instance Create
foreign import ccall "libvlc_new" libvlc_new :: CInt -> Ptr () -> IO (Ptr Libvlc_Instance)
--Instance Destroy
foreign import ccall "libvlc_release" libvlc_release :: Ptr Libvlc_Instance -> IO ()
foreign import ccall "&libvlc_release" libvlc_release_ptr :: FunPtr (Ptr Libvlc_Instance -> IO ())

--Media Create
foreign import ccall "libvlc_media_new_path" libvlc_media_new_path :: Ptr Libvlc_Instance -> CString -> IO (Ptr Libvlc_Media)
--Media Destroy
foreign import ccall "libvlc_media_release" libvlc_media_release :: Ptr Libvlc_Media -> IO ()
foreign import ccall "&libvlc_media_release" libvlc_media_release_ptr :: FunPtr (Ptr Libvlc_Media -> IO ())

--Media Player Create
foreign import ccall "libvlc_media_player_new_from_media" libvlc_media_player_new_from_media :: Ptr Libvlc_Media -> IO (Ptr Libvlc_Media_Player)
--Media Player Destroy
foreign import ccall "libvlc_media_player_release" libvlc_media_player_release :: Ptr Libvlc_Media_Player -> IO ()
foreign import ccall "&libvlc_media_player_release" libvlc_media_player_release_ptr :: FunPtr (Ptr Libvlc_Media_Player -> IO ())
--Media Player Functions
foreign import ccall "libvlc_media_player_play" libvlc_media_player_play :: Ptr Libvlc_Media_Player -> IO ()




