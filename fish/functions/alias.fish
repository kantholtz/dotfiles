#
#  exports all alias functions that serve as
#  proxy functions for ./lib/alias
#

function __alias_head -a head
  echo $head
end

function __alias_tail
  set -l tail
  if [ 1 -lt (count $argv) ]
    set tail $argv[2..-1]
  end

  echo $tail
end

function __alias_exec
  eval "~/.config/fish/functions/lib/alias $argv"
end


#
#  git shortcuts
#
function __g_gps
  for rt in (g rt)
    g ps $rt (__alias_tail $argv)
  end
end

function __g_gpl
  for rt in (g rt)
    g pl $rt (__alias_tail $argv)
  end    
end

function g -d "Aliases for git"
  switch (__alias_head $argv)
    case gps
      __g_gps $argv
    case gpl
      __g_gpl $argv
    case '*'
      __alias_exec g $argv
  end
end

#
#  rails shortcuts
#
function r -d "Aliases for rails"
  __alias_exec r $argv
end

#
#  tmux shortcuts
#
function t -d "Aliases for tmux"
  __alias_exec t $argv
end
