
set tabwidth "  "

#
#  function that makes
#  a recursive directory listing
#
function r

  set -l tw $argv[1]$tabwidth
  for x in (ls)
#    echo $tw$x

	# descend and recurse
	# if it's a dir
	if test -d $x
	  echo $tw$x
	  cd $x
	  r $tw
	end

  end
  cd ../
end

function lsrec
  set curdir (pwd)

  if test -d $argv[1]
    cd $argv[1]
  end

  r $tabwidth
  cd $curdir
end

