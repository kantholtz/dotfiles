# -*- coding: utf-8 -*-
#
#  Aliases work by parsing the ./alias/*.alias files
#  and translating them into their proper commands.
#  The translated command is then executed by fish.
ktz-echo "ktz-alias" "loading file"


function __ktz-alias-resolve
    set -l params (string escape -- $argv)
    set -l cmd (eval "$ktz_dir_fish"/lib/alias $params)

    if [ $status -eq 2 ]
        echo "alias error: $cmd" >&2
        return 1
    end

    echo $cmd
end


function __ktz-alias-exec
    # eval without arguments does nothing
    # so if the resolver outputs nothing
    # it will not execute anything
    eval (__ktz-alias-resolve $argv)
end


function a -d 'apt aliases'
    __ktz-alias-exec a $argv
end


function c -d 'systemd aliases'
    __ktz-alias-exec c $argv
end


function e -d 'emacs aliases'
    __ktz-alias-exec e $argv
end

function t -d 'tmux aliases'
    __ktz-alias-exec t $argv
end

function k -d "ktz aliases"
    __ktz-alias-exec k $argv
end


# not exec'ed but aliased

function ll -d 'list dirs verbose'
    ls -lAFvh $argv
end


# git

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

function g
    __ktz-alias-exec g $argv
end


# sudo
function s -d "sudo prefix"
    set -l args --preserve-env=APT_CONFIG

    if [ (count $argv) -eq 0 ]
        sudo $args
        return
    end

    # check whether the suffix string is resolvable
    # echo if __ktz-alias-resolve $argv
    if not __ktz-alias-resolve $argv >/dev/null 2>&1
        sudo $args $argv
        return
    end

    eval sudo $args (__ktz-alias-resolve $argv)
end
