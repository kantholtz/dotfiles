#!/bin/bash

sudo apt-get install terminator

echo 'install configuration'
mkdir -p ~/.config/terminator
ln -s $(pwd)/config ~/.config/terminator/


echo 'installing .desktop file'
mkdir -p ~/.local/share/applications
ln -s $(pwd)/terminator-dark.desktop ~/.local/share/applications/
