function xing \
	 -d "helper stuff for xing projects"


  switch $argv

  case 'riak'
    cd ~/Complex/riak-dev-cluster

    echo "starting the cluster"
    sudo rake start

    echo "current state"
    g s

  case 'activities'
	  echo 'enabling rvm'
		rvm >/dev/null

    echo "starting riak"
    riak start

    echo "starting memcached"
    /usr/local/opt/memcached/bin/memcached &
    # launchctl load ~/Library/LaunchAgents/homebrew.mxcl.memcached.plist
    echo "sent memcached to background"

    echo "current state"
    cd ~/Complex/activities
    g s

  case '*'
    echo "Error: '$argv' was not found"

  end


end
