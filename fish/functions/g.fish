function g \
	-d 'shortcuts for git'

	set -l cmd $argv[1]
	set -l args

	if test 1 -lt (count $argv)
		set args $argv[2..-1]
	end

	if test -z cmd
		echo "you must provide a command"
	end

	switch $cmd
		case a
			git add $args
		case b
			git branch $args
		case s
			git status $args
		case c
			git commit $args
		case cl
			git clone $args
		case co
			git checkout $args
		case l
			git log $args

		case ps
			git push $args
    case gps
      for rt in (g rt)
        g ps $rt $args
      end

		case pl
			git pull $args
    case gpl
      for rt in (g rt)
        g pl $rt $args
      end

		case d
			git diff $args
		case mg
			git merge $args
		case rm
			git rm $args
    case rt      
      git remote $args      
		case mv
			git mv $args
		case '*'
			echo "g: could not find command $args[1]"
	end

end
