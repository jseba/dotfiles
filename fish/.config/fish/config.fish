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

# neovim as default editor
set -x EDITOR "nvim"

# Enable ls colors
set -x LSCOLORS "Gxfxcxdxbxegedabagacad"

# Fancy ninja status
set -x NINJA_STATUS "[37m[[35m%u[37m/[33m%r[37m/[32m%f[37m]m [34m%o[m "

# set default options for less
set -x LESS -MIFXR

# path to SSH directory
set -x SSH "$HOME/.ssh"

# path to ssh-agent environment
set -x SSH_ENV "$SSH/ssh-agent.conf"

# path to history file
set -x HISTFILE "$HOME/.zsh_history"

# amount of history to save (lots)
set -x HISTSIZE 30000

# Go path
set -x GOPATH "$HOME/go"

# set Go default version
set -x GOVER "1.17"

# add Go installation to path
set -x PATH $PATH /usr/local/go/$GOVER/bin $GOPATH/bin

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
    unalias cat
end

############################
#
# Keybindings
#
############################
bind \cp history-search-backward
bind \cn history-search-forward

