
function hsrm
    echo "restarting window manager"
    spawn plasmashell --replace

    fproxy on
    sudo mount -a

    echo "adding ssh key"
    eval (ssh-agent -c)
    ssh-add

    # echo "swapping opt/cmd"
    # echo 1 | sudo tee /sys/module/hid_apple/parameters/swap_opt_cmd

    echo "dig a tunnel"
    tme 2222 hamann braco hop.lavis

    echo "spawning applications"
    spawn thunderbird
    spawn firefox
end
