function ktz-ssh -a host port hop -d "autossh tunneling"
    if [ -z "$host" -o -z "$port" -o -z "$hop" ]
        echo "provide host, port and a hop host"
        echo "usage: ktz-ssh HOST PORT HOP"
        return 2
    end

    autossh -fNL $port:$host:$port $hop
    or begin
        echo "did not work!"
        return 2
    end

    sleep 1
    echo "currently active connections to $host:"
    lsof -i | grep LISTEN | grep $port

end
