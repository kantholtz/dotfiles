# [t]unnel [m]ount [e]macs

function tme -a localport user destination hop
    if [ -z "$localport" -o -z "$destination" -o -z "$user" -o -z "$hop" ]
        echo "usage: tme LOCALPORT USER DESTINATION HOP"
        echo " e.g. tme 2222 hamann braco hop.lavis"
        return 2
    end

    set -l mountpoint /mnt/sshfs/$destination
    sudo mkdir -p $mountpoint

    autossh -f -N -M 0 -o "ServerAliveInterval 30" -o "ServerAliveCountMax 3" -NL localhost:$localport:$destination:22 $hop
    sleep 3

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

    # deactivating proxy for emacs as otherwise ein: will
    # not work with locally tunneled connections.
    # customize-variables/url-proxy-service is not working reliably
    begin
        set -e https_proxy
        set -e HTTPS_PROXY
        set -e http_proxy
        set -e HTTP_PROXY
        set -x KTZ_LIGHT
        emacs $mountpoint &
        disown
    end

    echo
    echo "you may close this window now"
end
