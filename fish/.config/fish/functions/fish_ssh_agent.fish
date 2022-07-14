function _is_ssh_agent_running
    if begin; test -f $SSH_ENV; and test -z "$SSH_AGENT_PID"; end
        source $SSH_ENV >/dev/null
    end

    if test -z "$SSH_AGENT_PID"
        return 1
    end

    #ps -ef | grep $SSH_AGENT_PID | grep -v grep | grep -q ssh-agent
    pgrep ssh-agent >/dev/null
    return $status
end

function _ssh_agent_start
    ssh-agent -c | sed -e "/^echo /d" > $SSH_ENV
    chmod 0600 $SSH_ENV
    source $SSH_ENV >/dev/null
    true # suppress errors from setenv
end

function fish_ssh_agent -d "Starts ssh-agent if not already started, else loads running ssh-agent settings"
    if not _is_ssh_agent_running
        _ssh_agent_start
    end
end
