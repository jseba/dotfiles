#!/bin/sh

run() {
    if ! pgrep $1 > /dev/null; then
        $@&
    fi
}

#run mpd ~/.config/mpd/mpd.conf
#run emacs --daemon
run compton -b

xrdb -merge
