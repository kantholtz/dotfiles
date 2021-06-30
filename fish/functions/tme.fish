# [t]unnel [m]ount [e]macs

function tme -a localport user destination
    if [ -z "$localport" -o -z "$destination" -o -z "$user" ]
        echo "usage: tme LOCALPORT USER DESTINATION"
        echo " e.g. tme 2222 hamann braco"
        return 2
    end

    set -l mountpoint /mnt/sshfs/$destination
    sudo mkdir -p $mountpoint

    autossh -f -N -M 0 -o "ServerAliveInterval 30" -o "ServerAliveCountMax 3" -NL localhost:$localport:$destination:22 hop.lavis
    sleep 1

    echo
    echo "created tunnel"
    lsof -i | grep localhost:$localport
    echo

    sudo sshfs \
        -p $localport \
        -o reconnect,allow_other,default_permissions,uid=(id -u),gid=(id -g) \
        $user@localhost: $mountpoint

    echo
    echo "currently mounted via sshfs:"
    mount | grep sshfs

    emacs $mountpoint &
    disown

    echo
    echo "you may close this window now"
end
