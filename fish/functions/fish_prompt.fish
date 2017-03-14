
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

  set_color $fish_color_match
  printf ' [%s]' $branch
  set_color normal

end


function __fish_prompt_display_rvm
  return

  set_color $fish_color_match
  if [ -n (which ruby | grep -e '.rvm') ]
    set -l rversion (ruby -v | sed 's/ruby \([0-9.p]*\) .*/\1/')
    printf '(%s)' $rversion
  end
  set_color normal

end


function __fish_prompt_display_vf

  set_color $fish_color_match
  if set -q VIRTUAL_ENV
    set -l py_env (basename "$VIRTUAL_ENV")
    set -l py_vers (python --version 2>&1 | sed 's/Python //')
    printf ' (%s | %s)' $py_env $py_vers
  end
  set_color normal

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

  # language informations
  printf "%s" $__fish_prompt_delim1
  __fish_prompt_display_rvm
  __fish_prompt_display_vf

  # i don't remember why I explicitly forced myself
  # to settle for one environment...
  __fish_prompt_display_vf

  # decoration
  echo
  printf "%s" $__fish_prompt_delim2

end
