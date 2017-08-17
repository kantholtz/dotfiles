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
  ~/.config/fish/functions/lib/alias $argv
end

#
#  proxy functions
#
function a -d "Aliases for apt-*"
  __alias_exec a $argv
end

function c -d "Aliases for systemctl, journalctl"
  __alias_exec c $argv
end

function ll --description 'Alias for ls -lAp'
	 ls -lhAp $argv
end

function s -d "Alias for sudo"
  sudo fish -c "$argv"
end


function t -d "Aliases for tmux"
  __alias_exec t $argv
end


#
#  emacs shortcut
#  if you want to open a file called "-gui"
#  in terminal mode, you must type
#  `e -gui -nw -gui`:)
#
function e -d "Alias for emacs [-nw]"
  if [ (count $argv) -gt 0 -a "$argv[1]" = "-gui" ]
    emacs (__alias_tail $argv)
  else
    emacs -nw $argv
  end
end


#
#  git shortcuts
#
function __g_gps
  for rt in (g rt)
    echo "pushing to $rt"
    g ps $rt (__alias_tail $argv)
  end
end

function __g_gpl
  for rt in (g rt)
    echo "pulling from $rt"
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
