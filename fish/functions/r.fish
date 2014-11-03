function r \
  -a cmd   \
	-d 'shortcuts for rails'

  set -l args
  set -l vers

  # catch ruby _VERSION_ command args
  if echo "$cmd" | grep -E '_.*_' >/dev/null
    set vers $cmd
    set cmd $argv[2]
    
    if [ 2 -lt (count $argv) ]
      set args $argv[3..-1]
    end
    
  else
    if [ 1 -lt (count $argv) ]
	    set args $argv[2..-1]
    end
  end


	if test -z $cmd
		echo "you must provide a command"
	end


	switch $cmd
    case c
      rails $vers console $args
		case g
			rails $vers generate $args
    case gs
      rails $vers generate scaffold $args
    case n
      rails $vers new $args
		case '*'
			echo "r: could not find command $cmd"
	end

end
