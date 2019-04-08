#!/bin/sh

find_and_kill(){
    ps x | grep "${1}" | grep -v grep | awk '{print $1}' | xargs kill
}

# mpd widget
find_and_kill "mpc idleloop player"

# volume widget
find_and_kill "pactl subscribe"
