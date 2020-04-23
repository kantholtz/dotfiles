
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
  printf '%s' $__fish_prompt_delim1
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


function __fish_prompt_display_vf

  if set -q VIRTUAL_ENV
      printf ' '
      set_color $fish_color_command
      set -l py_env (basename "$VIRTUAL_ENV")
      printf ' vf: (%s | %s) ' $py_env (__parse_python_version)
      set_color normal
  end

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

  # username
  __fish_prompt_display_username

  # hostname
  __fish_prompt_display_hostname
  printf "%s" $__fish_prompt_delim1

  # location
  set_color $fish_color_cwd
  printf '%s' (prompt_pwd)
  set_color normal

  # git indicator
  if git status >/dev/null 2>&1
    __fish_prompt_parse_git_branch
  end

  # virtual environments
  __fish_prompt_display_vf
  __fish_prompt_display_conda

  # decoration
  echo
  printf "%s" $__fish_prompt_delim2

end
