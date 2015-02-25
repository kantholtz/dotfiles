

function __mode_toggle \
  -a var

  if set -q $var
    set -eg $var
    echo "deactivated $var"
  else
    set -g $var
    echo "activated $var"
  end

end


function mode \
  -d "set the mode for the prompt" \
  -a m_name

  switch $m_name

    # toggle language specific modes
    case rb
      __mode_toggle __MODE_RB
    case py
      __mode_toggle __MODE_PY

    case '*'
      echo "this mode does not exist"

  end

end


function __fish_prompt_display_username

  set_color magenta
	set -l usr (whoami)
  printf '%s' "$usr"
  set_color -b normal

end


function __fish_prompt_display_hostname

	set -l host (hostname|cut -d . -f 1)

  set_color yellow
	printf ' ⬝ '
	set_color magenta
  printf '%s' $host

end


function __fish_prompt_parse_git_branch

	# try to recieve the branch and
	# cut away the "* "
	set -l branch (
	  git branch 2>/dev/null |\
	  grep -e '\*'           |\
	  sed 's/..\(.*\)/\1/')

  set_color yellow
  printf ' [%s]' $branch
  set_color normal

end


function __fish_prompt_display_rvm

  set_color magenta
  if [ -n (which ruby | grep -e '.rvm') ]
    set -l rversion (ruby -v | sed 's/ruby \([0-9.p]*\) .*/\1/')
    printf '(%s)' $rversion
  end
  set_color normal

end


function __fish_prompt_display_vf

  set_color magenta
  if set -q VIRTUAL_ENV
    set -l py_env (basename "$VIRTUAL_ENV")
    set -l py_vers (python --version 2>&1 | sed 's/Python //')
    printf '(%s | %s)' $py_env $py_vers
  end
  set_color normal

end


function fish_prompt
  echo

  if not git status >/dev/null 2>&1
    # username
    __fish_prompt_display_username

    # hostname
    # __fish_prompt_display_hostname
    set_color yellow
    printf ' ⬝ '
  end

  # location
  set_color magenta
  printf '%s' (prompt_pwd)
  set_color normal

  # git indicator
  if git status >/dev/null 2>&1
    __fish_prompt_parse_git_branch
  end

  # language informations
  if begin
      set -q __MODE_RB
      or set -q __MODE_PY
    end

    set_color yellow
    printf ' ⬝ '

    if set -q __MODE_RB
      __fish_prompt_display_rvm
    end

    if set -q __MODE_PY
      __fish_prompt_display_vf
    end
  end

  # decoration
  echo
  set_color yellow
  printf ' ▸ '
  set_color normal

end
