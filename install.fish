#!/usr/bin/env fish

#
#  Auto install all the dotfiles.
#  The original files will be saved
#  in a backup directory .backup.
#


function __install_emacs \
  -a home srcdir backdir

  set -l dotemacs "$home/.emacs"

  echo "installing emacs dotfiles"
  mkdir -p "$backdir/emacs"
  and cp -r          \
    "$dotemacs"      \
    "$home/.emacs.d" \
    "$backdir/emacs" \
    2>/dev/null
  and rm -r "$home/.emacs.d/mod.d"

  mkdir -p "$home/.emacs.d"
  and touch "$dotemacs"
  and echo '(load "~/.emacs.d/mod.d/init")' >> "$dotemacs"
  and ln -s \
    "$srcdir/emacs/mod.d"  \
    "$home/.emacs.d"
  or return 2

  echo "done"
end


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


function __install
  set -l home $HOME
  set -l srcdir (pwd)

  set -l timestamp (date "+%Y-%m-%d_%H_%M_%S")
  set -l backdir "$srcdir/.backup/$timestamp"

  mkdir -p "$backdir"

  if [ ! -h "$home/.emacs.d/mod.d" ]
    __install_emacs $home $srcdir $backdir
  else
    echo ".emacs.d/mod.d already installed"
  end

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


__install
