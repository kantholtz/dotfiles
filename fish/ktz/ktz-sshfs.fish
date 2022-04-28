ktz-echo "ktz-sshfs.fish" "loading file"

function ktz-sshfs -a destination target
    if [ -z "$destination" ]
        echo "usage: ktz-tm DESTINATION [TARGET]"
        echo " e.g. ktz-tm someserver /remote/directory"
        return 2
    end

    # default settings

    set -l userinfo uid=(id -u),gid=(id -g)
    set -l options -o reconnect,allow_other,default_permissions,$userinfo
    set -l mountpoint /mnt/sshfs/$destination
    set -l remote "$destination:$target"

    # --

    mkdir -p $mountpoint

    echo "mounting $remote to $mountpoint..."
    sshfs -o $userinfo $options $remote $mountpoint

    echo
    echo "currently mounted via sshfs:"
    mount | grep sshfs

    echo
    echo "you may close this window now"
end
