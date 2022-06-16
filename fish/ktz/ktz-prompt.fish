ktz-echo "ktz-prompt.fish" "loading file"


# delimiter
set -g ktz_prompt_delim ·


function __ktz-prompt-status -a laststatus
    if [ "$laststatus" -eq 0 ]
        set_color $ktz_clr_secondary $ktz_clr_secondary_fallback
    else
        set_color $ktz_clr_primary $ktz_clr_primary_fallback
    end

    printf "[%d] " $laststatus
    set_color normal
end

function __ktz-prompt-username
    printf '%s' $USER
end


function __ktz-prompt-hostname
    printf ' %s %s %s ' \
        $ktz_prompt_delim \
        (prompt_hostname) \
        $ktz_prompt_delim
end


function __ktz-prompt-git
    # try to recieve the branch and
    # cut away the "* "
    set -l branch (
    git branch 2>/dev/null |\
        grep -e '\*'           |\
        sed 's/..\(.*\)/\1/')

    printf ' '
    set_color $fish_color_command
    printf '[%s]' $branch
    set_color normal

end


function __ktz-prompt-python
    echo (python --version 2>&1 | sed -E 's/Python ([0-9.]+).*/\1/')
end


function __ktz-prompt-conda
    if [ $CONDA_DEFAULT_ENV = "base" ]
        return
    end

    set_color $ktz_clr_secondary_light
    printf " (%s:%s)" $CONDA_DEFAULT_ENV (__ktz-prompt-python)
    set_color normal
end


function __ktz-prompt-timestamp
    set_color $ktz_clr_subtle_light
    date +' %d.%m.%y %H:%M:%S' | xargs printf " %s"
    set_color normal
end


function __ktz-prompt-dumb
    set_color normal
    printf "\$ "
end


function ktz-prompt
    set -l laststatus $status
    echo

    if [ "$TERM" = dumb ];
        __ktz-prompt-dumb
        return
    end

    __ktz-prompt-status $laststatus
    __ktz-prompt-username
    __ktz-prompt-hostname

    # location
    # set_color $ktz_clr_primary
    set_color $ktz_clr_subtle
    printf '%s' (prompt_pwd)
    set_color normal

    # redirect for old fish versions that do not have "command"
    # added -s for old fish versions
    if command -qs git 2>/dev/null
        if git status >/dev/null 2>&1
            __ktz-prompt-git
        end
    end

    if set -q CONDA_DEFAULT_ENV
        __ktz-prompt-conda
    end

    __ktz-prompt-timestamp

    # prompt line
    if id -u >/dev/null
        set __ktz_prompt_marker \$
    else
        set __ktz_prompt_marker \#
    end

    echo -e "\n $__ktz_prompt_marker "
end
