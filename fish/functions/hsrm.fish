
function hsrm
    fproxy on
    sudo mount -a

    echo "adding ssh key"
    eval (ssh-agent -c)
    ssh-add

    echo "swapping opt/cmd"
    echo 1 | sudo tee /sys/module/hid_apple/parameters/swap_opt_cmd

    echo "spawning applications"
    spawn whatsdesk
    spawn telegram-desktop
    spawn thunderbird
    spawn firefox
end
