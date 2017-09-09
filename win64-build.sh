#!/bin/bash
if [ ! -d "vlc-2.2.6" ]; then
   wget http://download.videolan.org/pub/videolan/vlc/2.2.6/win64/vlc-2.2.6-win64.7z 
   /c/Program\ Files/7-Zip/7z e -y vlc-2.2.6-win64.7z
fi

stack build --flag fltkhs:bundled --extra-lib-dirs=$PWD/vlc-2.2.6 --extra-include-dirs=$PWD/vlc-2.2.6/sdk/include --force-dirty
