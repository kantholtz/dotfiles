
function fish_greeting

  if [ -z "$__custom_greeting" ]
    echo -e $__custom_greeting
  end

  # get list of available verses
  set -l pdir $HOME/.config/fish/functions/verses
  set -l verses (ls $pdir)

  # choose one poem randomly
  set -l i (expr (random) \% (count $verses) + 1)

  echo
  cat $pdir/$verses[$i] | sed 's/^/  /'

end
