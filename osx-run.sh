#!/bin/sh
if [ ! -d "/Volumes/vlc-2.2.6" ]; then
   if [ ! -f "vlc-2.2.6.dmg" ]; then 
      wget http://download.videolan.org/pub/videolan/vlc/2.2.6/macosx/vlc-2.2.6.dmg
   fi
   hdiutil attach vlc-2.2.6.dmg
fi

VLC_PLUGIN_PATH=/Volumes/vlc-2.2.6/VLC.app/Contents/MacOS/plugins/ DYLD_LIBRARY_PATH=/Volumes/vlc-2.2.6/VLC.app/Contents/MacOS/lib:$DYLD_LIBRARY_PATH stack exec fltkhs-media-player-exe

if [ -d "/Volumes/vlc-2.2.6" ]; then
   hdiutil detach /Volumes/vlc-2.2.6
fi
