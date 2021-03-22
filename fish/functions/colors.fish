function colors
    for color in (set -n | grep fish_color)
       printf '%40s |' $color
       set_color $$color
       echo $color
       set_color normal
    end
    echo
end
