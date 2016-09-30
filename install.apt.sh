#!/bin/bash
#
# Install script to initialize new machines
# that use apt for package management


#
#   exits the scripts with an error code
#
function quit_error {
    if [ -d .tmp ]; then
        rm -rf .tmp
    fi

    if [ -n "$1" ]; then
        echo "$1"
    fi

    echo "exiting"
    exit 2
}


#
#   checks if a program is installed
#   exits if not
#
function check_prog {
    command -v $1 >/dev/null 2>&1 || return 0
    return 2
}


#
#   ask user a y/n question
#   returns positive value for "n"
#
function ask_user {
    read yn
    if [ "$yn" = "y" ]; then
       return 0
    fi

    return 1
}


#
#   install a package from apt and check
#   if it is found afterwards
#
function safe_install {
    if check_prog git; then
        $APT install "$1" && \
            check_prog "$1" && \
            quit_error "git was not installed"
    fi
}


#
#   compile emacs (optional x support)
#
function install_emacs {
    echo "installing emacs..."
    VERSION=25.1
    FLAGS=


    echo "X support required? [yN]"
    if ask_user; then
       $APT install libtiff-dev libgif-dev libjpeg-dev libpng-dev libxpm-dev libgtk2.0-dev libncurses5-dev
    else
       FLAGS=--without-x
    fi

    mkdir .tmp
    pushd .tmp

    wget http://ftp.halifax.rwth-aachen.de/gnu/emacs/emacs-$VERSION.tar.xz && \
        tar xvf emacs-$VERSION.tar.xz && \
        pushd emacs-$VERSION && \
        ./configure $PREFIX $FLAGS && \
        make && \
        $S make install && \
        popd || \
            quit_error "could not install emacs"

    popd
    rm -rf .tmp

    if check_prog emacs; then
        quit_error "emacs was not installed properly"
    fi
}


function install_dotfiles {
  echo install dotfiles and commonly used programs
  #
  echo "installing dotfiles... where will they stay?"
  echo "relative paths are allowed, creates folder 'dotfiles'"
  read PTH

  kind=

  echo "use server configuration? [yN]"
  if ask_user; then
      kind=server
  fi

  $APT install tmux python3

  mkdir -p "$PTH" && \
      pushd "$PTH" || \
          quit_error "could not create $PTH"

  git clone https://github.com/dreadworks/dotfiles && \
      pushd dotfiles && \
      ./install.fish $kind && \
      popd || \
          "could not install the dotfiles"
}


#
#   initialize
#
S=
echo "sudo needed? (e.g. make install or apt-get) [yN]"
if ask_user; then
    S="sudo -s"
fi

if check_prog apt-get; then
    quit_error "apt-get is not installed"
fi

APT="$S apt-get"
$APT update

safe_install git
safe_install fish

echo "custom prefix (for configure)? (leave empty if undesired)"
read __prefix
PREFIX=--prefix="$__prefix"

if [ -n "$__prefix" ]; then
    echo "add prefix to PATH?"
    if ask_user; then
        PATH="$prefix:$PATH"
        export PATH
    fi
fi


#
#  install programs and configs
#
if check_prog emacs
    then install_emacs
    else echo "skipping installation of emacs"
fi

if [ ! -d ~/.emacs.d/mod.d ]
    then install_dotfiles
    else echo "skipping installation of dotfiles"
fi

popd
echo "done!"
