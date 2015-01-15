#
#  exports all alias functions that serve as
#  proxy functions for ./lib/alias
#

function __alias_exec
  eval "./lib/alias $argv"
end  


function g -d "Aliases for git"
  __alias_exec g $argv
end  


function t -d "Aliases for tmux"
  __alias_exec t $argv
end
