{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, CPP #-}
module Vlc
where
#include "vlc/vlc.h"
#include "vlc/libvlc.h"
#include "vlc/libvlc_media.h"
#include "vlc/libvlc_events.h"
import Foreign.Ptr
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Array
import Graphics.UI.FLTK.LowLevel.Utils
import Graphics.UI.FLTK.LowLevel.X
import Graphics.UI.FLTK.LowLevel.FLTKHS as FL
import Control.Exception

{#enum libvlc_event_e as VlcEvent {upcaseFirstLetter} deriving (Show) #}

foreign import ccall "wrapper"
  mkEventCallbackPrim:: EventCallbackPrim -> IO (FunPtr EventCallbackPrim)

toEventCallbackPrim :: EventCallback -> IO (FunPtr EventCallbackPrim)
toEventCallbackPrim eventF =
  mkEventCallbackPrim
    (
      \eventPtr _ ->
        fromVlcEvent eventPtr >>= eventF
    )

type EventCallbackPrim = Ptr () -> Ptr () -> IO ()
type EventCallback = (Maybe Event) -> IO ()

data EventManager = EventManager (Ptr ())
data VlcInstance = VlcInstance (Ptr ())
data MediaPlayer = MediaPlayer (Ptr ())
data Media = Media (Ptr ())
type VlcTime = {#type libvlc_time_t #} 

data Event =
    Ended
  | TimeChanged VlcTime
  | LengthChanged VlcTime
  | Error
  deriving Show

fromVlcEvent :: Ptr () -> IO (Maybe Event)
fromVlcEvent eventPtr = do
  e :: VlcEvent <-
    fmap
      (toEnum . fromIntegral)
      ({#get libvlc_event_t->type #} eventPtr)
  case e of
    Libvlc_MediaPlayerTimeChanged ->
      fmap
        (Just . TimeChanged)
        ({#get libvlc_event_t->u.media_player_time_changed.new_time #} eventPtr)
    Libvlc_MediaPlayerLengthChanged ->
      fmap
        (Just . LengthChanged)
        ({#get libvlc_event_t->u.media_player_length_changed.new_length #} eventPtr)
    Libvlc_MediaPlayerEndReached ->
      pure (Just Ended)
    Libvlc_MediaPlayerEncounteredError ->
      pure (Just Error)
    _ ->
      pure Nothing

makeVlcInstance :: IO VlcInstance
makeVlcInstance =
  let args =
        [
          "--mouse-hide-timeout=5",
          "--swscale-mode=1",
          "--no-video-title-show",
          "--no-osd",
          "--vout=vdpau"
        ]
  in
    do
      stringsPtr <- mapM newCString args >>= newArray
      p <- {#call libvlc_new #} (fromIntegral (length args)) stringsPtr
      if (p == nullPtr)
        then (ioError (userError "Could not make VLC instance."))
        else pure (VlcInstance p)

makeMedia :: FilePath -> VlcInstance -> IO Media
makeMedia path (VlcInstance vlcInstance) =
  withCString
    path
    (
      \pathPtr -> do
        p <- {#call libvlc_media_new_path #} vlcInstance pathPtr
        if (p == nullPtr)
          then (ioError (userError "Could not make VLC media."))
          else pure (Vlc.Media p)
    )

makeMediaPlayer :: Vlc.Media -> IO MediaPlayer
makeMediaPlayer (Vlc.Media media) = do
  p <- {#call libvlc_media_player_new_from_media #} media
  if (p == nullPtr)
    then (ioError (userError "Could not make VLC media player."))
    else pure (MediaPlayer p)

makeEventManager :: MediaPlayer -> IO EventManager
makeEventManager (MediaPlayer mediaPlayer) = do
  p <- {#call libvlc_media_player_event_manager #} mediaPlayer
  if (p == nullPtr)
    then (ioError (userError "Could not make VLC event manager."))
    else pure (EventManager p)

setWindow :: MediaPlayer -> Maybe WindowHandle -> IO ()
setWindow (MediaPlayer mp) h =
  case h of
    Nothing -> ioError (userError "Could not get XID of window.")
    Just (WindowHandle xid) ->
#ifdef WIN32
      {#call libvlc_media_player_set_hwnd #} mp xid
#else
      {#call libvlc_media_player_set_xwindow #} mp ((fromInteger . toInteger . ptrToWordPtr) xid)
#endif

setInputs :: MediaPlayer -> IO ()
setInputs (MediaPlayer mp) = do
  {#call libvlc_video_set_key_input #} mp (fromIntegral 0)
  {#call libvlc_video_set_mouse_input #} mp (fromIntegral 0)
  return ()

setVolume :: MediaPlayer -> CInt -> IO ()
setVolume (MediaPlayer mp) vol = do
  {#call libvlc_audio_set_volume #} mp vol
  return ()

getVolume :: MediaPlayer -> IO CInt
getVolume (MediaPlayer mp) =
  {# call libvlc_audio_get_volume #} mp

play :: MediaPlayer -> IO ()
play (MediaPlayer mp) = do
  {#call libvlc_media_player_play #} mp
  return ()

pause :: MediaPlayer -> IO ()
pause (MediaPlayer mp) =
  {#call libvlc_media_player_pause #} mp

stop :: MediaPlayer -> IO ()
stop (MediaPlayer mp) = do
  {#call libvlc_media_player_stop #} mp
  return ()

playing :: MediaPlayer -> IO Bool
playing (MediaPlayer mp) = do
  res <- {#call libvlc_media_player_is_playing #} mp
  return (res /= 0)

releaseMedia :: Vlc.Media -> IO ()
releaseMedia (Vlc.Media m) =
  {#call libvlc_media_release #} m

releaseMediaPlayer :: Vlc.MediaPlayer -> IO ()
releaseMediaPlayer (Vlc.MediaPlayer m) =
  {#call libvlc_media_player_release #} m

releaseVlcInstance :: VlcInstance -> IO ()
releaseVlcInstance (VlcInstance i) =
  {#call libvlc_release #} i

setPosition :: MediaPlayer -> VlcTime -> IO ()
setPosition (MediaPlayer mp) pos =
  {#call libvlc_media_player_set_time #} mp pos

getPosition :: MediaPlayer -> IO VlcTime
getPosition (MediaPlayer mp) =
  {#call libvlc_media_player_get_time #} mp

getLength :: MediaPlayer -> IO VlcTime
getLength (MediaPlayer mp) =
  {#call libvlc_media_player_get_length #} mp

attachEvent :: EventManager -> VlcEvent -> FunPtr EventCallbackPrim  -> IO ()
attachEvent (EventManager em) e fp = do
  {#call libvlc_event_attach #} em (fromIntegral (fromEnum e)) fp nullPtr
  return ()

detachEvent :: EventManager -> VlcEvent -> FunPtr EventCallbackPrim  -> IO ()
detachEvent (EventManager em) e fp = do
  {#call libvlc_event_detach #} em (fromIntegral (fromEnum e)) fp nullPtr
  return ()

registerEvents :: EventManager -> EventCallback -> IO ()
registerEvents em cb = do
  fptr <- toEventCallbackPrim cb
  attachEvent em Libvlc_MediaPlayerTimeChanged fptr
  attachEvent em Libvlc_MediaPlayerLengthChanged fptr
  attachEvent em Libvlc_MediaPlayerEndReached fptr
  attachEvent em Libvlc_MediaPlayerEncounteredError fptr

unregisterEvents :: EventManager -> EventCallback -> IO ()
unregisterEvents em cb = do
  fptr <- toEventCallbackPrim cb
  detachEvent em Libvlc_MediaPlayerTimeChanged fptr
  detachEvent em Libvlc_MediaPlayerLengthChanged fptr
  detachEvent em Libvlc_MediaPlayerEndReached fptr
  detachEvent em Libvlc_MediaPlayerEncounteredError fptr
