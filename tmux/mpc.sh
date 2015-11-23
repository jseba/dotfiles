#!/bin/bash

SONG=$(mpc current)
if [[ $(mpc status | awk 'NR==2' | cut -f1 -d ' ') == "[playing]" ]]; then
    echo "♫ ${SONG}"
else
    echo "▮▮ ${SONG}"
fi
