function fish_prompt
    set -l green (set_color -o green)
    set -l red (set_color -o red)
    set -l yellow (set_color -o yellow)
    set -l magenta (set_color magenta)
    set -l cyan (set_color -o cyan)
    set -l normal (set_color normal)

    set -l last_status $status
    set -l status_color $green
    if test $last_status -ne 0
        set status_color $red
    end

    # TODO: port git information from original lambda theme

    set -l lambda "λ "
    set -l username (whoami)
    set -l host (hostname -s)
    set -l cwd (prompt_pwd)
    set -l arrow " →  "

    # newline before prompt
    echo -e ''
    echo -e -n -s $status_color $lambda $yellow $username "@" $host " " $normal $magenta "[" $cwd "]" $cyan $arrow $normal
end
