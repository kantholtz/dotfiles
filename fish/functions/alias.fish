# -*- coding: utf-8 -*-
#
#  Aliases work by parsing the ./alias/*.alias files
#  and translating them into their proper commands.
#  The translated command is then executed by fish.


set -g __KTZ_ALIAS ~/.config/fish/functions/lib/alias


function __ktz_fail -a msg --on-event evt_fail
    echo "alias error: $msg"
end


function __ktz_alias_resolve
    set -l cmd (eval "$__KTZ_ALIAS" $argv)
    if [ $status -eq 2 ]
        emit evt_fail "$cmd"
    else
        echo "$cmd"
    end
end

function __ktz_alias_exec
    # eval without arguments does nothing
    # so if the resolver outputs nothing
    # it will not execute anything
    eval (__ktz_alias_resolve $argv)
end


# APT
# --------------------
function a -d 'apt aliases'
    __ktz_alias_exec a $argv
end


# SYSTEMD
# --------------------
function c -d 'systemd aliases'
    __ktz_alias_exec c $argv
end


# GIT
# --------------------
function __ktz_g_gps
  for rt in (g rt)
    echo "pushing to $rt"
    g ps $rt $argv[2..-1]
  end
end

function __ktz_g_gpl
    for rt in (g rt)
        echo "pulling from $rt"
        g pl $rt $argv[2..-1]
    end
end

# function g -d "Aliases for git"
#     switch ($argv[1])
#         case gps
#             __ktz_g_gps $argv
#         case gpl
#             __ktz_g_gpl $argv
#         case '*'
#             echo __ktz_alias_exec g $argv
#     end
# end


function g
    __ktz_alias_exec g $argv
end


# TMUX
# --------------------
function t -d 'git aliases'
    __ktz_alias_exec t $argv
end


# SUDO
# --------------------
function s -d "sudo prefix"
    if [ (count $argv) -gt 0 ]
        eval sudo (__ktz_alias_resolve $argv)
    else
        eval sudo
    end
end
