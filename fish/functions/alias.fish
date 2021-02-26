# -*- coding: utf-8 -*-
#
#  Aliases work by parsing the ./alias/*.alias files
#  and translating them into their proper commands.
#  The translated command is then executed by fish.

set __CC ~/.config/fish/functions/lib/alias


function __alias_resolve
    echo (eval "$__CC" $argv)
end

function __alias_exec
    eval (__alias_resolve $argv)
end


# APT
# --------------------
function a -d 'apt aliases'
    __alias_exec a $argv
end


# SYSTEMD
# --------------------
function c -d 'systemd aliases'
    __alias_exec c $argv
end


# GIT
# --------------------
function __g_gps
  for rt in (g rt)
    echo "pushing to $rt"
    g ps $rt $argv[2..-1]
  end
end

function __g_gpl
    for rt in (g rt)
        echo "pulling from $rt"
        g pl $rt $argv[2..-1]
    end
end

function g -d "Aliases for git"
    switch ($argv[1])
        case gps
            __g_gps $argv
        case gpl
            __g_gpl $argv
        case '*'
            __alias_exec g $argv
    end
end


# TMUX
# --------------------
function t -d 'git aliases'
    __alias_exec t $argv
end


# SUDO
# --------------------
function s -d "sudo prefix"
    if [ (count $argv) -gt 0 ]
        eval sudo (__alias_resolve $argv)
    else
        eval sudo
    end
end
