#!/usr/bin/env bash
#
#  emulates fish/functions/fish_greeting.fish
#  for systems without fish

from=$1

if [ -z "$from" -o ! -d "$from" ]; then
    echo "usage: $0 dir"
    echo "  where dir: directory with verse files"
    exit 2
fi

selection=$(ls $from | shuf | head -n 1)

if [ -z "$selection" ]; then
    echo "no verse found"
    exit 2
fi

echo
cat $from/$selection
