#!/bin/bash
#
# Install script to initialize new machines
# that use apt for package management


#
#   exits the scripts with an error code
#
function quit_error {
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
#   initialize
#
S=
echo "sudo needed? (e.g. make install or apt-get) [yn]"
if ask_user; then
    S="sudo -s"
fi

if check_prog apt-get; then
    quit_error "apt-get is not installed"
fi

APT="$S apt-get"
$APT update && $APT upgrade

if check_prog git; then
    $APT install git && \
        check_prog git && \
        quit_error "git was not installed"
fi


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
#   install fish
#
echo "installing fish"
$APT install build-essential autoconf libncurses5-dev

mkdir .tmp
pushd .tmp

git clone https://github.com/fish-shell/fish-shell && \
    pushd fish-shell && \
    autoconf && \
    ./configure $PREFIX && \
    make && \
    $S make install && \
    popd || \
        quit_error "could not install fish"

popd
rm -rf .tmp

if check_prog fish; then
    quit_error "fish was not installed properly"
fi


#
#   install emacs
#
echo "installing emacs"
VERSION=24.5
FLAGS=


echo "X support required?"
if ! ask_user; then
    FLAGS=--without-x
fi

$APT install

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


#
#   install dotfiles and commonly used programs
#
echo "installing dotfiles, where shall they stay?"
echo "relative paths are allowed, will create folder 'dotfiles'"
read PTH

$APT install tmux python3

mkdir -p "$PTH" && \
    pushd "$PTH" || \
        quit_error "could not create $PTH"

git clone https://github.com/dreadworks/dotfiles && \
    pushd dotfiles && \
    ./install.fish && \
    popd || \
        "could not install the dotfiles"

popd
echo "done!"
