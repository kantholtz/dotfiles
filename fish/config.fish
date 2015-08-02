
function __config_custom
  ulimit -n 1024

  # includes
  . ~/.config/fish/functions/virtual.fish
  . ~/.config/fish/functions/alias.fish

  # display hidden files first
  # set -gx LC_ALL "C.$LC_ALL"

  # syntax highlighting for "less"
  set -l less_script /usr/share/source-highlight/src-hilite-lesspipe.sh

  if [ -x $less_script ]
    set -gx LESSOPEN "| $less_script %s"
    set -gx LESS ' -R '
  end

end

__config_custom
