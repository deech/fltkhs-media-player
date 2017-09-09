#!/bin/bash
if [ ! -d "libvlc-sdk" ]; then
   git clone https://github.com/RSATom/libvlc-sdk
fi
if [ ! -d "webchimera.js" ]; then
   wget https://github.com/RSATom/WebChimera.js/releases/download/v0.2.7/WebChimera.js_v0.2.7_nw_v0.23.7_VLC_v2.2.4_x64_osx.tar.gz
   tar -zxf WebChimera.js_v0.2.7_nw_v0.23.7_VLC_v2.2.4_x64_osx.tar.gz
fi
stack build --flag fltkhs:bundled --extra-lib-dirs=./webchimera.js/lib --extra-include-dirs=./libvlc-sdk/include
