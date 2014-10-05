#!/usr/bin/env fish

#
#  Auto install all the dotfiles.
#  The original files will be saved
#  in a backup directory .backup.
#

# paths are defined relative to $HOME
# format: <backupdir> <backupfile> [<sourcedir>]
# note: sourcedir must have trailing slashes
set -l srcs \
  "emacs .emacs" \
  "fish config.fish .config/fish/" \
  "fish functions .config/fish/"


set -l cdir (pwd)
set -l home "$HOME"

set -l timestamp (date "+%Y-%m-%d_%H_%M_%S")
set -l backdir "$cdir/.backup/$timestamp"

mkdir -p "$backdir"
echo "created backup directory"

for tup in $srcs
  echo $tup | read bdir bfile srcdir

  echo "creating backup for $bdir: $bfile"
  set -l target "$srcdir$bfile"
  set -l src "$home/$target"
  set -l backup "$backdir/$bdir/$srcdir"

  mkdir -p "$backup"
  cp -a "$src" "$backup"

  echo "linking $bfile"
  rm -fr "$src"
  ln -s "$cdir/$bdir/$bfile" "$src"
  
end

