#!/bin/bash
if [ ! -d "vlc-2.2.6" ]; then
   wget http://download.videolan.org/pub/videolan/vlc/2.2.6/win64/vlc-2.2.6-win64.7z 
   p7zip -d vlc-2.2.6-win64
fi

stack build --flag fltkhs:bundled --extra-lib-dirs=$PWD/vlc-2.2.6/sdk/lib --extra-include-dirs=$PWD/vlc-2.2.6/sdk/include
