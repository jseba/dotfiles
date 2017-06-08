#!/bin/bash

## Check for Stow
type stow >/dev/null 2>&1 || { echo "Stow not found"; exit 1; }

## Add list of folders to be managed by stow
STOWED_DIRS=( bin base16 emacs fonts fzf git herbstluft i3 nvim rofi terminfo tmux urxvt vim zsh )

#
# Pre-Stow warmup
#

## Load submodules
git submodule update --init --recursive || exit 1

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

## Download FZF
pushd ${HOME}/.fzf/
./install --bin --64 || exit 1
popd

exit 0
