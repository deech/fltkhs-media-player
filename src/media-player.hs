{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Main where
import Control.Exception
import Data.Int
import Data.Text as T
import Foreign.C.Types
import Foreign.Ptr
import Graphics.UI.FLTK.LowLevel.Ask
import Graphics.UI.FLTK.LowLevel.FL as FL
import Graphics.UI.FLTK.LowLevel.FLTKHS
import Graphics.UI.FLTK.LowLevel.Fl_Enumerations
import Graphics.UI.FLTK.LowLevel.Fl_Types
import Graphics.UI.FLTK.LowLevel.X
import MediaPlayer
import System.Exit
import System.Process
import UI
import Vlc
import Data.IORef
import System.Exit

data BadMedia = BadMedia

vlcEvents :: UI -> EventCallback
vlcEvents ui (Just ev) =
  case ev of
    Ended -> return ()
    TimeChanged t -> do
      setValue (playbackControl ui) (fromIntegral t)
      return ()
    LengthChanged t -> do
      bounds (playbackControl ui) 1 (fromIntegral t)
      setValue (playbackControl ui) 1
      return ()
    Error -> undefined

cleanUp :: UI -> IORef State -> IO ()
cleanUp ui stateRef = do
  state <- readIORef stateRef
  media' <- case (media state) of
    Nothing -> pure Nothing
    Just s -> releaseMedia s >> pure Nothing
  mediaPlayer' <- case (mediaPlayer state) of
    Nothing -> pure Nothing
    Just p -> do
      check <- playing p
      if check then stop p else pure ()
      releaseMediaPlayer p
      pure Nothing
  _ <- case (eventManager state) of
    Nothing -> pure ()
    Just em -> unregisterEvents em (vlcEvents ui)
  modifyIORef
    stateRef
    (
      \state ->
        state
          {
            media = media'
          , mediaPlayer = mediaPlayer'
          , playState = Initial
          }
    )

openMedia :: UI -> IORef State -> FilePath -> IO (Either BadMedia ())
openMedia ui stateRef path =
  go `catch` (\(e :: IOException) -> pure (Left BadMedia))
  where
    go = do
      state <- readIORef stateRef
      media <- makeMedia path (vlcInstance state)
      player <- makeMediaPlayer media
      setWindow player (windowHandle state)
      em <- makeEventManager player
      setInputs player
      registerEvents em (vlcEvents ui)
      cleanUp ui stateRef
      activate (controls ui)
      resetControls ui
      modifyIORef
        stateRef
        (
          \state ->
            state
              {
                media = Just media
              , mediaPlayer = Just player
              , eventManager = Just em
              }
        )
      pure (Right ())

resetControls :: UI -> IO ()
resetControls ui = do
  setLabel (playButton ui) "@>"
  bounds (volumeControl ui) 0 200
  setValue (volumeControl ui) 100
  return ()

onOpen :: UI -> IORef State -> Ref MenuItem -> IO ()
onOpen ui stateRef _ = do
  chooser <- nativeFileChooserNew Nothing
  _ <- showWidget chooser
  f <- getFilename chooser
  case f of
    Nothing -> pure ()
    (Just f) -> do
      res <- openMedia ui stateRef (T.unpack f)
      case res of
        Left BadMedia -> flMessage "Not a valid file format. Please choose a video file."
        Right () -> onPress (volumeControl ui) stateRef (playButton ui)

onPress :: Ref Slider -> IORef State -> Ref Button -> IO ()
onPress volumeControl stateRef b = do
  let newState s = modifyIORef
                    stateRef
                    (
                      \state ->
                        state
                          {
                            playState = s
                          }
                    )
  state <- readIORef stateRef
  case (playState state) of
    Initial -> do
       case ((,) <$> (mediaPlayer state) <*> (eventManager state)) of
         Just (mp, em) -> do
           setLabel b "@>"
           play mp
           to <- getValue volumeControl
           setVolume mp (truncate to)
           changeVolume stateRef volumeControl
           newState Playing
         Nothing -> flMessage "Not ready."
    Paused ->
      case (mediaPlayer state) of
        Just mp -> do
          changeVolume stateRef volumeControl
          play mp
          setLabel b "@>"
          newState Playing
        Nothing -> pure ()
    Playing ->
      case (mediaPlayer state) of
        Just mp -> do
          pause mp
          newState Paused
          setLabel b "@||"
        Nothing -> pure ()

changeVolume :: IORef State -> Ref Slider -> IO ()
changeVolume stateRef s = do
  state <- readIORef stateRef
  vol <- getValue s
  case (mediaPlayer state) of
    Just mp -> setVolume mp (truncate vol)
    Nothing -> pure ()

seek :: IORef State -> Ref Slider -> IO ()
seek stateRef s = do
  state <- readIORef stateRef
  to <- getValue s
  case (mediaPlayer state) of
    Nothing -> return ()
    Just mp -> Vlc.setPosition mp (truncate to)

onShow :: IORef State -> Ref Window -> IO ()
onShow stateRef win = do
  showWidgetSuper win
  wid <- flcXid win
  modifyIORef stateRef (\state -> state { windowHandle = wid })

main :: IO ()
main = do
  ui <- makeUI
  vlcI <- makeVlcInstance
  state <-
    newIORef
    (
      State
      {
        vlcInstance = vlcI
      , media = Nothing
      , mediaPlayer = Nothing
      , eventManager = Nothing
      , windowHandle = Nothing
      , playState = Initial
      }
    )
  (Rectangle position size) <- getRectangle (playbackContainer ui)
  begin (playbackContainer ui)
  playbackWindow <-
    windowCustom
      size
      (Just position)
      Nothing
      Nothing
      (
        defaultCustomWidgetFuncs
        {
          showCustom = (Just (onShow state))
        }
      )
      defaultCustomWindowFuncs
  end (playbackContainer ui)
  setCallback (playButton ui) (onPress (volumeControl ui) state)
  setCallback (volumeControl ui) (changeVolume state)
  setCallback (openMenu ui) (onOpen ui state)
  setCallback (playbackControl ui) (seek state)
  setCallback (quitMenu ui) (\_ -> exitSuccess)
  showWidget playbackWindow
  showWidget (mainWindow ui)
  FL.run
  FL.flush
  releaseVlcInstance vlcI
