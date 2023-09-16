#!/bin/bash

## Check for Stow
type stow >/dev/null 2>&1 || { echo "Stow not found"; exit 1; }

## Add list of folders to be managed by Stow
STOWED_DIRS=( alacritty     \
              bin           \
              dircolors     \
              emacs         \
              fish          \
              fonts         \
              git           \
              i3            \
              kitty         \
              nvim          \
              systemd       \
              terminfo      \
              tmux          \
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
if [[ $(ps -p 1 -o comm=) == "systemd" ]]; then
    systemctl --user enable --now ssh-agent.service
fi

exit 0
