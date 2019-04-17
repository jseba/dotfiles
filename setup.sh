#!/bin/bash

## Check for Stow
type stow >/dev/null 2>&1 || { echo "Stow not found"; exit 1; }

## Add list of folders to be managed by Stow
STOWED_DIRS=( alacritty     \
              awesome       \
              bin           \
              compton       \
              dircolors     \
              emacs         \
              fonts         \
              git           \
              herbstluft    \
              i3            \
              kitty         \
              nvim          \
              polybar       \
              rofi          \
              terminfo      \
              tmux          \
              urxvt         \
              vim           \
              xorg          \
              zsh           \
            )

#
# Pre-Stow warm-up
#

## Load submodules
git submodule update --init --recursive || exit 1

# Make sure local shared folder exists
mkdir -p ${HOME}/.local/share

#
# IIIIIT'S STOW TIME!!
#

# using `-R' makes this safe to re-run
for dir in ${STOWED_DIRS[@]}; do
    stow -Rv $dir || exit 1
done

#
# Post-Stow after-party
#

exit 0
