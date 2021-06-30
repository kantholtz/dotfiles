function spawn
    eval "$argv >/dev/null 2>&1 &; disown"
end
