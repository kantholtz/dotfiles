function ktz-spawn -d "run a command in background and disown immediately"
    eval "$argv >/dev/null 2>&1 &; disown"
end
