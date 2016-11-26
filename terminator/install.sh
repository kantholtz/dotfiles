#!/bin/bash

echo 'install configuration'
mkdir -p ~/.config/terminator
ln -s (pwd)/config ~/.config/terminator/


echo 'installing .desktop file'
mkdir -p ~/.local/share/applications
ln -s (pwd)/terminator-dark.desktop ~/.local/share/applications/
