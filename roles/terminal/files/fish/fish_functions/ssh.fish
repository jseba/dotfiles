function ssh
    set -lx TERM xterm-256color
    command ssh $argv
end
