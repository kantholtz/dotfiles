
function hsrm
    fproxy on

    echo "adding ssh key"
    eval (ssh-agent -c)
    ssh-add

    echo "swapping opt/cmd"
    echo 1 | sudo tee /sys/module/hid_apple/parameters/swap_opt_cmd
end
