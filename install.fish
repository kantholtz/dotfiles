#!/usr/bin/env fish

#
#  Auto install all the dotfiles.
#  The original files will be saved
#  in a backup directory .backup.
#


function __install_fish \
  -a home srcdir backdir

  set -l tardir "$home/.config/fish"
  set -l targets          \
    "$tardir/config.fish" \
    "$tardir/functions"   \

  echo "installing fish dotfiles"
  mkdir -p "$backdir/fish"
  and cp -r $targets "$backdir/fish/" 2>/dev/null
  and rm -rf $targets

  mkdir -p "$tardir"
  and ln -s                    \
    "$srcdir/fish/config.fish" \
    "$srcdir/fish/functions"   \
    "$tardir/"
  or return 2

  echo "done"
end


function __install_tmux \
  -a home srcdir backdir

  echo "installing tmux dotfiles"
  mkdir -p "$backdir/tmux"
  and cp "$home/.tmux.conf" "$backdir/tmux/" 2>/dev/null
  and rm "$home/.tmux.conf"

  ln -s "$srcdir/tmux/.tmux.conf" "$home/"
  or exit 2

  echo "done"
end


function __create_backupdir -a srcdir
  set -l timestamp (date "+%Y-%m-%d_%H_%M_%S")
  set -l backdir "$srcdir/.backup/$timestamp"
  mkdir -p "$backdir"
  echo "$backdir"
end


function __install -a kind
  set -l home $HOME
  set -l srcdir (pwd)

  set -l backdir (__create_backupdir "$srcdir")

  if [ ! -h "$home/.config/fish/functions" ]
    __install_fish $home $srcdir $backdir
  else
    echo ".config/fish/functions already installed"
  end

  if [ ! -h "$home/.tmux.conf" ]
    and __install_tmux $home $srcdir $backdir
  else
    echo ".tmux.conf already installed"
  end

end


function __uninstall
  echo "removing installed files"
  set -l home $HOME

  # todo : backup files
  # legacy: .emacs.d/mod.d

  for tbr in \
    ".emacs.d/mod.d" \
    ".config/fish/config.fish" \
    ".config/fish/functions" \
    ".tmux.conf"

    set -l f "$home/$tbr"

    if [ -d "$f" -o -f "$f" ]
      echo "uninstalling $f"
      rm -r "$f"
    else
      echo "skipping $f"
    end
  end
end


if [ (count $argv) -eq 0 ]
  __install
else if [ "$argv[1]" = "uninstall" ]
  __uninstall
end
