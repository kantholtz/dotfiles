
function usage
  echo "usage: terminator[.fish] dark|bright"
end


function terminator -a theme
  if [ -z "$theme" ]
    usage
    exit 2
  end

  #
  #   -------------------- BRIGHT
  #
  if [ "$theme" = bright ]
    echo "set fish colors for bright theme"
    set -x fish_color_user           blue
    set -x fish_color_cwd            blue
    set -x fish_color_command        blue
    set -x fish_color_autosuggestion orange
    set -x fish_color_error          red
    set -x fish_color_comment        grey
    set -x fish_color_quote          green

  #
  #   -------------------- DARK
  #
  else if [ "$theme" = dark ]
    echo "set fish colors for dark theme"
    set -x fish_color_user           green
    set -x fish_color_cwd            green
    set -x fish_color_command        yellow
    set -x fish_color_autosuggestion brblue
    set -x fish_color_error          red
    set -x fish_color_comment        grey
    set -x fish_color_quote          yellow

  else
    echo "theme '$theme' not supported"
    exit 2
  end

  set -l cmd (which terminator)
  eval $cmd -p $theme
end
