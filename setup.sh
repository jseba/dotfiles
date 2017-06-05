#!/bin/bash

## Check for Stow
type stow >/dev/null 2>&1 || exit 1

## Add list of folders to be managed by stow
STOWED_DIRS=( bin emacs fonts git herbstluft i3 nvim polybar terminfo tmux vim zsh )

#
# Pre-Stow warmup
#

## Download FZF
pushd fzf
./install --bin --64 || exit 1
popd

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

# not cool enough for an after party yet :(

exit 0
