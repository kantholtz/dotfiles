
function __config_custom
  ulimit -n 1024

  . ~/.config/fish/functions/virtual.fish
  . ~/.config/fish/functions/alias.fish

  # syntax highlighting for "less"
  set -l less_script /usr/share/source-highlight/src-hilite-lesspipe.sh

  if [ -x $less_script ]
    set -gx LESSOPEN "| $less_script %s"
    set -gx LESS ' -R '
    end

    verses
end

__config_custom
