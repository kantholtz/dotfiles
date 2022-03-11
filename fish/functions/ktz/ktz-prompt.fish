# TODO can I rely on ktz-clr.fish to be loaded?
# . ktz-clr.fish

# delimiter
set -g ktz_prompt_delim1 ·

function __ktz-prompt-username
    set_color $ktz_clr_primary
    set -l usr (whoami)
    printf '%s' "$usr"
    set_color normal
    printf ' %s' $ktz_prompt_delim1
end


function __ktz-prompt-hostname
    printf ' %s' (hostname|cut -d . -f 1)
    printf ' %s' $ktz_prompt_delim1
end


function __ktz-prompt-pwd
    set_color $ktz_clr_primary
    printf ' %s' (prompt_pwd)
    set_color normal
    printf ' %s' $ktz_prompt_delim1
end


function __ktz-prompt-git-branch
    set_color $ktz_clr_primary_light
    # try to recieve the branch and
    # cut away the "* "
    set -l branch (
    git branch 2>/dev/null |\
        grep -e '\*'           |\
        sed 's/..\(.*\)/\1/')

    printf ' [%s]' $branch
    set_color normal
end


function __ktz-prompt-git
    # redirect for old fish versions that do not have "command"
    # added -s for old fish versions
    if command -qs git 2>/dev/null
        if git status >/dev/null 2>&1
            __ktz-prompt-git-branch
        end
    end
end


function __ktz-prompt-python
    echo (python --version 2>&1 | sed -E 's/Python ([0-9.]+).*/\1/')
end


function __ktz-prompt-conda
    if not set -q CONDA_DEFAULT_ENV
        return
    end

    set_color $ktz_clr_primary_light
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
    __ktz-prompt-pwd
    __ktz-prompt-git
    __ktz-prompt-conda
    __ktz-prompt-timestamp

    set -l prompt_char '$'
    if [ (id -u) -eq 0 ]
        set -l prompt_char '#'
    end

    # decoration
    echo -e "\n $prompt_char "
end
