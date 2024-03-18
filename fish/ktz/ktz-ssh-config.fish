ktz-echo "ktz-ssh-config.fish" "loading file"


#  $ ll ~/.ssh
# (...)
# lrwxrwxrwx 1 felix felix  30 Mai 19 11:00 config -> /home/felix/.ssh/config.normal
# lrwxrwxrwx 1 felix felix  39 Mai 19 16:28 config.normal -> /home/felix/Complex/psi/Conf/ssh.normal
# lrwxrwxrwx 1 felix felix  37 Mai 19 16:28 config.hsrm -> /home/felix/Complex/psi/Conf/ssh.hsrm
# (...)
# $ ktz-ssh-config normal
# activating: /home/felix/.ssh/config.normal


function ktz-ssh-config -a name -d "switch ssh config"
    set -l conf_dir "$HOME"/.ssh
    set -l conf "$conf_dir"/config

    # exists but is not a symbolic link
    test -e "$conf" -a ! -L "$conf"
    set -l invalid $status

    # no arg: print helpful message

    if [ -z "$name" ]
        echo "Usage: ktz-ssh-config NAME"
        if [ $invalid -eq 0 ]
            echo "No config active."
        else
            echo "Current state:"
            ll $conf_dir | grep -E config | cut -d ' ' -f 10-
        end
        return 0
    end

    # TODO see https://fishshell.com/docs/current/cmds/argparse.html
    if [ "$name" = "-q" ]
        return ([ -L "$conf" ])
    end

    if [ "$name" = "-n" ]
        echo (basename (readlink -f ~/.ssh/config) | cut -d '.' -f 2)
        return $status
    end

    # conf in undesirable state

    if [ $invalid -eq 0 ]
        echo "$conf exists and is not a symbolic link"
        echo "will not overwrite existing config!"
        return 2
    end

    # set new config

    set -l target "$conf_dir/config.$name"

    if [ ! -f "$target" -o ! -L "$target" ]
        echo "cannot find $target"
        return 2
    end

    echo "activating: $target"

    if [ -L "$conf" ]
        rm "$conf"
    end

    ln -s "$target" "$conf"
end
