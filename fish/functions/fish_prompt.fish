
function display_username

  set_color magenta
	set -l usr (whoami)
  printf '%s' "$usr"
  set_color -b normal

end


function display_hostname

	set -l host (hostname|cut -d . -f 1)

  set_color yellow
	printf ' ⬝ '
	set_color magenta
  printf '%s' $host

end


function parse_git_branch

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


function display_rvm

  if [ -n (which ruby | grep -e '.rvm') ]
    set -l rversion (ruby -v | sed 's/ruby \([0-9.p]*\) .*/\1/')
    printf ' (%s)' $rversion
  end

end


function fish_prompt

  # username
  display_username

  # hostname
  display_hostname
  set_color yellow
  printf ' ⬝ '

  # location
  set_color magenta
  printf '%s' (prompt_pwd)
  set_color normal

  # git indicator
  git status >/dev/null 2>&1
  if [ 0 -eq "$status" ]
    parse_git_branch
    # display_rvm
  end


  # decoration
  set_color yellow
  printf ' ▸ '
  set_color normal

end
