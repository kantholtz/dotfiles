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
			git status
		case c
			git commit $args
		case co
			git checkout $args
		case l
			git log
		case r
			git rm $args
		case ps
			git push $args
		case pl
			git pull $args
		case d
			git diff $args
		case m
			git mv $args
		case mg
			git merge $args
		case rm
			git rm $args
		case mv
			git mv $args
	end

end
