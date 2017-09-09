#!/bin/sh

VLC_PLUGIN_PATH=./webchimera.js/lib/plugins/ DYLD_LIBRARY_PATH=./webchimera.js/lib:$DYLD_LIBRARY_PATH stack exec fltkhs-media-player-exe
