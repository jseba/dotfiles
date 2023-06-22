function tmux -d "Attaches to running tmux session if one exists, else starts a new one."
    if test -n "$argv"
        # run tmux directly if extra arguments
        command tmux $argv
    else
        command tmux attach; or command tmux new-session
    end
end
