 # -*- coding: utf-8 -*-
#
#  Aliases work by parsing the ./alias/*.alias files
#  and translating them into their proper commands.
#  The translated command is then executed by fish.


set -g __KTZ_ALIAS ~/.config/fish/functions/lib/alias


function __ktz_alias_resolve
    set -l cmd (eval "$__KTZ_ALIAS" $argv)
    if [ $status -eq 2 ]
        echo "alias error: $cmd" >&2
        return 1
    end

    echo "$cmd"
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


# EMACS
# --------------------
function e -d 'systemd aliases'
    emacs $argv
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
    if [ (count $argv) -eq 0 ]
        sudo
        return
    end

    # check whether the suffix string is resolvable
    # echo if __ktz_alias_resolve $argv
    if not __ktz_alias_resolve $argv &>/dev/null
        sudo $argv
        return
    end

    eval sudo (__ktz_alias_resolve $argv)
end
