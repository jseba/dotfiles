############################
#
# Environment Variables
#
############################

# make path a unique array
typeset -u path

# set the completion cache file location
export ZSH_COMPDUMP="$ZSH"/.zcompdump

# use UTF-8
export LANG="en_US.UTF-8"

# get the short (not FQ) hostname
export SHORT_HOST=$(hostname -s)

# add our terminfo library
export TERMINFO="$HOME/.terminfo"

# vim as default editor
export EDITOR="vim"

# Enable ls colors
export LSCOLORS="Gxfxcxdxbxegedabagacad"

# Fancy ninja status
export NINJA_STATUS="[37m[[35m%u[37m/[33m%r[37m/[32m%f[37m]m [34m%o[m "

# set default options for less
export LESS=-MIFXR

# path to SSH directory
export SSH="$HOME/.ssh"

# path to ssh-agent environment
export SSH_ENV="$SSH/ssh-agent.conf"

# path to history file
export HISTFILE="$HOME/.zsh_history"

# amount of history to save (lots)
export HISTSIZE=30000

# Go path
export GOPATH="$HOME/go"

# add Go installation to path
export PATH="$PATH:/usr/local/go/bin"

############################
#
# Variables
#
############################

# add functions to fpath
fpath=($ZSH/functions $ZSH/completions $fpath)

# list of SSH identities
ssh_identities=()

############################
#
# Autoloads
#
############################

# load completion functions from fpath
autoload -Uz compaudit compinit

# load ls colors
autoload -U colors && colors

# load history search
autoload -U history-search-end

############################
#
# Functions
#
############################

function zsh_stats()
{
    fc -l 1 | awk '{CMD[$2]++;count++;}END { for (a in CMD)print CMD[a] " " CMD[a]/count*100 "% " a;}' | grep -v "./" | column -c 3 -s " " -t | sort -nr | nl |  head -n 20
}

function ttitle()
{
    # TODO: needs more support
    if [ -z ${1} ]; then
        echo "Please supply a string for the title"
    else
        echo -ne "\033]0;${1}\007"
    fi
}

# toggle sudo/sudoedit at beginning of command
function _sudo_command_line()
{
    [[ -z $BUFFER ]] && zle up-history
    if [[ $BUFFER == sudo\ * ]]; then
        LBUFFER="${LBUFFER#sudo }"
    elif [[ $BUFFER == $EDITOR\ * ]]; then
        LBUFFER="${LBUFFER#$EDITOR }"
        LBUFFER="sudoedit $LBUFFER"
    elif [[ $BUFFER == sudoedit\ * ]]; then
        LBUFFER="${LBUFFER#sudoedit }"
        LBUFFER="$EDITOR $LBUFFER"
    else
        LBUFFER="sudo $LBUFFER"
    fi
}

# wrapper function for tmux
function _tmux_wrapper()
{
    if [[ -n "$@" ]]; then
        # run tmux directly if any extra arguments
        \tmux $@
    else
        # else try to connect to running session
        \tmux attach || \tmux new-session
    fi
}

# colorize man pages
function _man_colored()
{
    LESS_TERMCAP_mb=$(printf "\e[1;31m") \
    LESS_TERMCAP_md=$(printf "\e[1;31m") \
    LESS_TERMCAP_me=$(printf "\e[0m") \
    LESS_TERMCAP_se=$(printf "\e[0m") \
    LESS_TERMCAP_so=$(printf "\e[1;44;33m") \
    LESS_TERMCAP_ue=$(printf "\e[0m") \
    LESS_TERMCAP_us=$(printf "\e[1;32m") \
    PAGER="${commands[less]:-$PAGER}" \
    _NROFF_U=1 \
        \man "$@"
}

############################
#
# Completions
#
############################

# enable and cache completions
compinit -i -d "${ZCOMPDUMP}"

# load the complist module
zmodload -i zsh/complist

# remove symbols from the word characters list
WORDCHARS=''

# fuzzy completion:
#  * default        (abc => abc)
#  * smart case     (abc => Abc)
#  * word flex      (abc => A-big-Car)
#  * full flex      (abc => AbraCadabra)
zstyle ':completion:*' matcher-list '' \
    'm:{a-z\-}={A-Z\_}' \
    'r:[^[:alpha:]]||[[:alpha:]]=** r:|=* m:{a-z\-}={A-Z\_}' \
    'r:[[:ascii:]]||[[:ascii:]]=** r:|=* m:{a-z\-}={A-Z\_}'

zstyle ':completion:*' auto-description 'Select: %d'

zstyle ':completion:*' completer _expand _complete _correct _approximate


zstyle ':completion:*' format 'Completing %d'

# default to menu selection for completions, if at least 2 results
zstyle ':completion:*:*:*:*:*' menu select=2

# complete . and .. special directories
zstyle ':completion:*' special-dirs true

# use GNU LS_COLORS by default
zstyle ':completion:*' list-colors ''

# colorize process list when killing
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#) ([0-9a-z-]#)*=01;34=0=01'

# format the list of processes
zstyle ':completion:*:*:*:*:processes' command 'ps -u $USER -o pid,%cpu,tty,cputime,cmd'

# disable named-directories autocompletion
zstyle ':completion:*:cd:*' tag-order local-directories directory-stack path-directories

# use caching so that commands like apt and dpkg complete are useable
zstyle ':completion::complete:*' use-cache 1
zstyle ':completion::complete:*' cache-path $ZSH_CACHE_DIR

# don't complete uninteresting users...
zstyle ':completion:*:*:*:users' ignored-patterns \
    adm amanda apache at avahi avahi-autoipd beaglidx bin cacti canna \
    clamav daemon dbus distcache dnsmasq dovecot fax ftp games gdm \
    gkrellmd gopher hacluster haldaemon halt hsqldb ident junkbust kdm \
    ldap lp mail mailman mailnull man messagebus  mldonkey mysql nagios \
    named netdump news nfsnobody nobody nscd ntp nut nx obsrun openvpn \
    operator pcap polkitd postfix postgres privoxy pulse pvm quagga radvd \
    rpc rpcuser rpm rtkit scard shutdown squid sshd statd svn sync tftp \
    usbmux uucp vcsa wwwrun xfs '_*'

# ... unless we really want to.
zstyle '*' single-ignored show

# complete ssh from our known hosts 
zstyle -e ':completion:*:(ssh|scp|sftp|rsync):hosts' hosts \
    'reply=(${=${${(f)"$(cat {/etc/ssh_,~/.ssh/known_}hosts(|2)(N) /dev/null)"}%%[#]*}//,/ })'

# use appropriate completions for wrapper functions
compdef _tmux _tmux_wrapper
compdef _man _man_colored

############################
#
# ZLE
#
############################

# create sudo widget
zle -N _sudo_command_line

# search history by already typed prefix
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end

############################
#
# Options
#
############################

# do not autoselect the first completion entry
unsetopt menu_complete

# instead, show completion menu on successive tab press
setopt auto_menu

# disable flow control (this isn't a serial terminal)
unsetopt flowcontrol

# complete both ends of the word
setopt complete_in_word

# move the cursor to the end of the word on completion
setopt always_to_end

# autocorrect typos on completion
setopt correct

# do not autocorrect the entire line, however
unsetopt correctall

# warn if no matches for filename generation
setopt nomatch

# allow '~', '#', and '^' to be used in filename generation
setopt extended_glob

# do not ask before executing 'rm *' or 'rm <path>/*' (that's what -i is for)
setopt rm_star_silent

# treat cd like pushd
setopt auto_pushd

# ignore duplicates on the directory stack
setopt pushd_ignore_dups

# treat pushd with no arguments like cd with no arguments
setopt pushd_to_home

# don't print the directory stack on pushd/popd
setopt pushd_silent

# invert the signs when using +/- with pushd/popd
setopt pushd_minus

# don't try to cd into a directory when failing to execute a command
unsetopt auto_cd

# automagically handle multiple redirections
setopt multios

# perform parameter and arithmetic expansions and command substitution on prompt first
setopt prompt_subst

# append history so multiple shells can run in parallel
setopt append_history

# remove duplicates first when trimming history
setopt hist_expire_dups_first

# ignore contiguous duplicates
setopt hist_ignore_dups

# trim superfluous spaces from history lines
setopt hist_reduce_blanks

# add history lines immediately rather than waiting for the shell to exit
setopt inc_append_history

# no beeps
unsetopt beep

############################
#
# Aliases
#
############################

# remove some commands from automatic autocorrect
alias cat='nocorrect cat'
alias cp='nocorrect cp'
alias find='nocorrect find'
alias ln='nocorrect ln'
alias man='nocorrect man'
alias mv='nocorrect mv'
alias rm='nocorrect rm'
alias rmdir='nocorrect rmdir'
alias su='nocorrect su'
alias sudo='nocorrect sudo'

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
if command -v bat &>/dev/null; then
    alias cat="bat"
else
    unalias cat
fi

# wrapper functions
alias tmux=_tmux_wrapper
alias man=_man_colored

############################
#
# Platform-specific
#
############################

if [[ "$OSTYPE" == darwin* ]]; then

    # this just works by default with $LSCOLORS
    ls -G . &>/dev/null && alias ls='ls -G'


    # homebrew aliases
    alias brews='brew list -1'
    alias bubo='brew update && brew outdated'
    alias bubc='brew upgrade && brew cleanup'
    alias bubu='bubo && bubc'
else

    # macOS's $HOST changes with dhcp, etc. Use ComputerName if possible.
    SHORT_HOST=$(scutil --get ComputerName 2>/dev/null) || SHORT_HOST=${HOST/.*/}

    # use default colors for GNU ls
    if [[ -z "$LS_COLORS" ]]; then
        (( $+commands[dircolors] )) && eval "$(dircolors -b)"
    fi

    ls --color -d . &>/dev/null && alias ls='ls --color=tty' || { ls -G . &>/dev/null && alias ls='ls -G' }

    # take advantage of $LS_COLORS for completion as well
    zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"
fi

############################
#
# Keybindings
#
############################

# use Emacs keybindings
bindkey -e

# double Esc to add/remove sudo from beginning of line
bindkey "\e\e" sudo-command-line

# navigate history by prefix
bindkey "\e[A" history-beginning-search-backward-end
bindkey "\e[B" history-beginning-search-forward-end

############################
#
# Prompt
#
############################

LAMBDA="%(?,%{$fg_bold[green]%}λ,%{$fg_bold[red]%}λ)"
if [[ "$USER" == "root" ]]; then USERCOLOR="red"; else USERCOLOR="yellow"; fi

PROMPT='
${LAMBDA}\
 %{$fg_bold[$USERCOLOR]%}%n@%M\
 %{$fg_no_bold[magenta]%}[%3~]\
 %{$fg_bold[cyan]%}→ %{$reset_color%} '

############################
#
# ssh-agent
#
############################

# load any existing config
if [[ -f "$SSH_ENV" ]]; then
    source "$SSH_ENV"
fi

# check if ssh-agent is already running
if grep ssh-agent =(ps x) | grep -q "$SSH_AGENT_PID"; then
    # start ssh-agent and reload config
    ssh-agent -s | sed -e '/^echo@/d' >! "$SSH_ENV"
    chmod 0600 "$SSH_ENV"
    ssh-add $SSH/${^ssh_identities}
fi

