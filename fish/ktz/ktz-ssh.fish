ktz-echo "ktz-ssh.fish" "loading file"


function ktz-sshfs \
    -a name \
    -d "provide an alias ()"

    if [ -z "$name" ]
        echo "no alias provided"
        return 2
    end

    # expects an array [src, dst]
    set -l args (ktz-local-sshfs-name $name)

    if not set -q args[1]; or not set -q args[2]:
        echo "alias could not be resolved"
        return 2
    end

    set -l src $args[1]
    set -l dst $args[2]

    echo "mounting $name: from=$src to=$dst"

    set -l cmd sudo sshfs \
        -o reconnect,allow_other,default_permissions,uid=(id -u),gid=(id -g) \
        $src $dst

    echo "$cmd"
    eval $cmd

    echo -e "done\n"
    echo -e "mounted $dst:\n"
    ls -l $dst

end
