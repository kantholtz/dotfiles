
set __fish_prompt_delim1 ' + '
set __fish_prompt_delim2 ' > '


if not set -q fish_color_user
  set -gx fish_color_user green
end


function __fish_prompt_display_username
  set_color $fish_color_user
  set -l usr (whoami)
  printf '%s' "$usr"
  set_color normal
end


function __fish_prompt_display_hostname
  set -l host (hostname|cut -d . -f 1)
  printf '%s' ' | '
  set_color $fish_color_user
  printf '%s' $host
  set_color normal
end


function __fish_prompt_parse_git_branch

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


function __parse_python_version
    echo (python --version 2>&1 | sed -E 's/Python ([0-9.]+).*/\1/')
end


function __fish_prompt_display_conda

    if set -q CONDA_DEFAULT_ENV
        printf ' '
        set_color $fish_color_command
        printf ' conda: (%s | %s) ' $CONDA_DEFAULT_ENV (__parse_python_version)
        set_color normal
    end

end



function fish_prompt
  echo

  __fish_prompt_display_username
  __fish_prompt_display_hostname

  # location
  set_color $fish_color_cwd
  printf '%s' (prompt_pwd)
  set_color normal

  if git status >/dev/null 2>&1
    __fish_prompt_parse_git_branch
  end

  __fish_prompt_display_conda

  # decoration
  echo -e '\n > '

end
