module UI where
import Graphics.UI.FLTK.LowLevel.FLTKHS
import qualified Graphics.UI.FLTK.LowLevel.FL as FL
import Graphics.UI.FLTK.LowLevel.Fl_Types
import Graphics.UI.FLTK.LowLevel.Fl_Enumerations
import Vlc

data UI =
  UI
  {
    mainWindow :: Ref Window
  , openMenu :: Ref MenuItem
  , quitMenu :: Ref MenuItem
  , playbackContainer :: Ref Group
  , controls :: Ref Group
  , playButton :: Ref Button
  , playbackControl :: Ref Slider
  , volumeControl :: Ref Slider
  }

data PlayState = Initial | Playing | Paused deriving Show

data State =
  State
  {
    vlcInstance :: VlcInstance,
    media :: Maybe Media,
    mediaPlayer :: Maybe MediaPlayer,
    eventManager :: Maybe EventManager,
    windowHandle :: Maybe WindowHandle,
    playState :: PlayState
  }
