ktz-echo "ktz-tm.fish" "loading file"

# [t]unnel and [m]ount

function ktz-tm -a destination target hop localport
    if [ -z "$destination" ]
        echo "usage: ktz-tm DESTINATION [TARGET HOP LOCALPORT]"
        echo " e.g. ktz-tm someserver /remote/directory"
        echo " e.g. ktz-tm anotherserver '' hopserver 2222"
        return 2
    end

    # default settings

    set -l userinfo uid=(id -u),gid=(id -g)
    set -l options -o reconnect,allow_other,default_permissions,$userinfo
    set -l mountpoint /mnt/sshfs/$destination
    set -l remote "$destination:$target"


    # optional tunneling using autossh

    if [ -n "$hop" -a -n "$localport" ]

        autossh -f -N -M 0 -o "ServerAliveInterval 30" -o "ServerAliveCountMax 3" -NL localhost:$localport:$destination:22 $hop
        echo "starting autossh..."
        sleep 3

        echo
        echo "maybe created tunnel:"

        set -l tunnel (lsof -i | grep localhost:$localport)

        if [ -z "$tunnel" ]
            echo "it seems as if this did not work..."
            exit 2
        end

        echo $tunnel

        set options -p $localport $options
        set remote localhost:$target
    end

    # --

    sudo mkdir -p $mountpoint

    echo "mounting $remote to $mountpoint..."
    sudo sshfs -o $userinfo $options $remote $mountpoint

    echo
    echo "currently mounted via sshfs:"
    mount | grep sshfs

    echo
    echo "you may close this window now"
end
