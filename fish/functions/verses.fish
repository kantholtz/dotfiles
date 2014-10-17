

function verses

  # get list of available verses
  set -l pdir $HOME/.config/fish/functions/verses
  set -l verses (ls $pdir)

  # choose one poem randomly
  set -l i (expr (random) \% (count $verses) + 1)

  # debug
  # echo "verses, found "(count $verses)" verses, chose "$i

  # preserve newlines...
  set greeting "
"(cat -s $pdir/$verses[$i])

  set fish_greeting (echo $greeting)"
  "

end
