function man -d "colorized man pages"
    set -lx LESS_TERMCAP_mb (printf "\e[1;31m")
    set -lx LESS_TERMCAP_md (printf "\e[1;31m")
    set -lx LESS_TERMCAP_me (printf "\e[0m")
    set -lx LESS_TERMCAP_se (printf "\e[0m")
    set -lx LESS_TERMCAP_so (printf "\e[1;44;33m")
    set -lx LESS_TERMCAP_ue (printf "\e[0m")
    set -lx LESS_TERMCAP_us (printf "\e[1;32m")
    set -lx PAGER (which less)
    set -lx _NROFF_U 1
    command man $argv
end
