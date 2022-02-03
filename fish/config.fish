
function __config_custom

  set -l fish_dir ~/.config/fish

  # not under version control for machine specific configuration
  set -l local_conf $fish_dir/config.local.fish

  if [ -f $local_conf ]  # -f implies -L
    source $local_conf
  end

  # includes
  source $fish_dir/functions/alias.fish
  for file in $fish_dir/functions/ktz/*.fish
      source $file
  end

end

__config_custom
