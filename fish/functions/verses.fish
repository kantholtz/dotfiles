

function verses

  # get list of available verses
  set -l pdir $HOME/.config/fish/functions/verses
  set -l verses (ls $pdir)

  # choose one poem randomly
  set -l rand (od -A d -t u -N 2 /dev/urandom | head -c 25 | tail -c 4)
  set -l i (expr $rand \% (count $verses))

  # fish counts its arrays from 1 up
  set -l i (expr $i + 1)

  # debug
  # echo "verses, found "(count $verses)" verses, chose "$i

  # preserve newlines...
  set greeting "
"(cat -s $pdir/$verses[$i])

  set fish_greeting (echo $greeting)"
  "

end
