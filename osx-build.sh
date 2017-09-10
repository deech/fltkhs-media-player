#!/bin/bash
if [ ! -d "/Volumes/vlc-2.2.6" ]; then
   if [ ! -f "vlc-2.2.6.dmg" ]; then 
      wget http://download.videolan.org/pub/videolan/vlc/2.2.6/macosx/vlc-2.2.6.dmg
   fi
   hdiutil attach vlc-2.2.6.dmg
fi

stack build --flag fltkhs:bundled --extra-lib-dirs=/Volumes/vlc-2.2.6/VLC.app/Contents/MacOS/lib --extra-include-dirs=/Volumes/vlc-2.2.6/VLC.app/Contents/MacOS/include

if [ -d "/Volumes/vlc-2.2.6" ]; then
   hdiutil detach /Volumes/vlc-2.2.6
fi
