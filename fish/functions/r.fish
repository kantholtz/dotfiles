

function r_map_action \
  -a cmd action

  switch $action
    case c
      echo controller

    case it
      echo integration_test

    case md
      echo model

    case mg
      echo migration

    case v
      echo view

    case '*'
      echo $action
  end
end


function r \
	-d 'shortcuts for rails' \
  -a cmd

  set -l args
  set -l vers

  # catch rails _VERSION_ command args
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
      set args[1] (r_map_action $cmd $args[1])
      if [ -n $args[1] ]
			  rails $vers generate $args
      end

    case d
      set args[1] (r_map_action $cmd $args[1])
      if [ -n $args[1] ]
        rails $vers destroy $args
      end

    case gs
      rails $vers generate scaffold $args

    case n
      rails $vers new $args

    case s
      rails $vers server $args

		case '*'
			echo "r: could not find command $cmd" 1>&2
	end

end
