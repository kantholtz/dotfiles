#!/usr/bin/env fish

#
#  Install python, pip, virtualenv and virtualfish
#

echo "installing python packages"
s a i python python3 python-pip python3-pip
and pip install virtualfish

echo "enter the location of the virtual envs"
echo "location is created unless it exists"
read venv

if [ ! -d "$venv" ]
  mkdir -p "$venv"
end

set -U VIRTUALFISH_HOME "$venv"
