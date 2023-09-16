############################
#
# Environment Variables
#
############################

# set base PATH
set -x PATH /usr/local/bin /usr/local/sbin /usr/bin /usr/sbin /bin /sbin

# use UTF-8
set -x LANG "en_US.UTF-8"

# add our terminfo library
set -x TERMINFO "$HOME/.terminfo"

# vim as default editor
set -x EDITOR "vim"

# Enable ls colors
set -x LSCOLORS "Gxfxcxdxbxegedabagacad"

# Fancy ninja status
set -x NINJA_STATUS "[37m[[35m%u[37m/[33m%r[37m/[32m%f[37m]m [34m%o[m "

# set default options for less
set -x LESS -MIFXR

# path to SSH directory
set -x SSH "$HOME/.ssh"

# path to ssh-agent socket (managed by systemd)
set -x SSH_AUTH_SOCK "$XDG_RUNTIME_DIR/ssh-agent.sock"

# Go local path
set -x GOPATH "$HOME/go"

# add Go toolchain to path
set -x PATH $PATH /usr/local/go/bin $GOPATH/bin

# add Rust installation to path
set -x PATH $PATH "$HOME/.cargo/bin"

# add Doom bin to path
set -x PATH $PATH "$HOME/.config/emacs/bin"

# add local bin to path
set -x PATH $PATH "$HOME/.local/bin"

############################
#
# Aliases
#
############################

# List directory contents
alias lsa='ls -lah'
alias l='ls -lah'
alias ll='ls -lh'
alias la='ls -lAh'

# handy rsync shortcuts
alias rsync-cp="rsync -avz --progress -h"
alias rsync-mv="rsync -avz --progress -h --remove-source-files"
alias rsync-up="rsync -avzu --progress -h"
alias rsync-sync="rsync -avzu --delete --progress -h"

# cat images to the terminal
alias icat="kitty +kitten icat"

# better cat
if command -sq bat
    alias cat="bat"
else if command -sq batcat
    alias cat="batcat"
else
    alias cat="/bin/cat"
end

# quit using sudo
if command -sq doas
    alias sudo="doas"
end

############################
#
# Keybindings
#
############################
bind \cp history-search-backward
bind \cn history-search-forward

############################
#
# Emacs/TRAMP
#
############################
if test "$TERM" = "dumb"
    function fish_prompt
        echo "\$ "
    end

    function fish_right_prompt; end
    function fish_greeting; end
    function fish_title; end
end

set fish_greeting
