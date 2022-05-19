function ktz-spawn -d "run a command in background and disown immediately"
    if set -q $argv
        echo "ktz-spawn: no arguments provided!"
        return 2
    end

    eval "$argv >/dev/null 2>&1 &; disown"
end
