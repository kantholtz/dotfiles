
function fish_greeting

  # get list of available verses
  set -l pdir $HOME/.config/fish/functions/verses
  set -l verses (ls $pdir)

  # choose one poem randomly
  set -l i (expr (random) \% (count $verses) + 1)

  echo
  cat $pdir/$verses[$i] | sed 's/^/  /'

end
