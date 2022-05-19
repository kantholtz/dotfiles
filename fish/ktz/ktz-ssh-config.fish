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
    set conf_dir "$HOME"/.ssh
    set conf "$conf_dir"/config

    if [ -e "$conf" -a ! -L "$conf" ]
        echo "$conf exists or is not a symbolic link"
        echo "will not overwrite existing config!"
        return 2
    end

    set target "$conf_dir/config.$name"
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
