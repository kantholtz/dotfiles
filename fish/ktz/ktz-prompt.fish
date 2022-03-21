ktz-echo "ktz-prompt.fish" "loading file"


# delimiter
set -g ktz_prompt_delim1 ·
set -g ktz_prompt_delim2 ⯈

function __ktz-prompt-username
    set_color $ktz_clr_primary
    set -l usr (whoami)
    printf '%s' "$usr"
    set_color normal
end


function __ktz-prompt-hostname
    set -l host (hostname|cut -d . -f 1)
    printf ' %s %s %s ' $ktz_prompt_delim1 $host $ktz_prompt_delim1
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
    set_color $ktz_clr_secondary_light
    printf " (%s:%s)" $CONDA_DEFAULT_ENV (__ktz-prompt-python)
    set_color normal
end


function __ktz-prompt-timestamp
    set_color $ktz_clr_subtle_light
    date +' %d.%m.%y %H:%M:%S' | xargs printf " %s"
    set_color normal
end


function ktz-prompt
    echo

    __ktz-prompt-username
    __ktz-prompt-hostname

    # location
    set_color $ktz_clr_primary
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

    # decoration
    echo -e "\n $ktz_prompt_delim2 "
end
