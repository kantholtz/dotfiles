# -*- coding: utf-8 -*-

# the f. proxy switcher
# --------------------------


function __ktz_fp_usage
    echo "usage: fproxy on|off"
end


# if the company thought that setting up a http/socks proxy was
# a good idea and you struggle with your laptop that needs to
# access stuff both inside/outside the proxy realm
function fproxy -a state -d "activate or deactivate the proxy"
    if [ -z "$state" ]
        __ktz_fp_usage
        return 2
    end

    if [ $state = 'on' ]
        __ktz_fp_on
        return $status

    else if [ $state = 'off' ]
        __ktz_fp_off
        return $status

    else
        __ktz_fp_usage
        return 2
    end
end


function __ktz_fp_on
    echo 'activating proxy configuration'
    and __ktz_fp_env_on
    and __ktz_fp_ssh_on ~/.ssh
    and __ktz_fp_apt_on
    and __ktz_fp_snap_on
    or echo 'aborting'
end

function __ktz_fp_off
    echo 'deactivating proxy configuration'
    and __ktz_fp_env_off
    and __ktz_fp_ssh_off ~/.ssh
    and __ktz_fp_apt_off
    and __ktz_fp_snap_off
    or echo 'aborting'
end


# ENV


function __ktz_fp_env_on
    echo 'setting environment variables'

    if [ -z "$KTZ_PROXY" ]
        echo 'you need to set KTZ_PROXY and KTZ_NO_PROXY' >&2
        return 2
    end

    # both lower-cased and uppercased as some
    # programs are expecting one or the other
    set -Ux HTTP_PROXY $KTZ_PROXY
    set -Ux http_proxy $KTZ_PROXY

    set -Ux HTTPS_PROXY $KTZ_PROXY
    set -Ux https_proxy $KTZ_PROXY

    set -Ux NO_PROXY $KTZ_NO_PROXY
    set -Ux no_proxy $KTZ_NO_PROXY
end


function __ktz_fp_env_off
    echo 'unsetting enviroment variables'

    set -e HTTP_PROXY
    set -e http_proxy

    set -e HTTPS_PROXY
    set -e https_proxy

    set -e NO_PROXY
    set -e no_proxy
end

# SSH


function __ktz_fp_ssh_check -a confd
    if [ ! -e "$confd"/config.proxy -o ! -e "$confd"/config.normal ]
        echo "missing config.proxy or config.normal in $confd" >&2
        return 2
    end
end


function __ktz_fp_ssh_link -a src tar
    if not [ -L "$tar" ]
        echo "$tar is not a symbolic link!"
        return 2
    end

    rm "$tar"
    ln -s "$src" "$tar"
end


function __ktz_fp_ssh_on -a confd
    echo "using proxy ssh configurations"
    if not __ktz_fp_ssh_check "$confd"
        return 2
    end

    __ktz_fp_ssh_link "$confd"/config.proxy "$confd"/config
    return $status
end

function __ktz_fp_ssh_off -a confd
    echo "using remote ssh configurations"
    if not __ktz_fp_ssh_check "$confd"
        return 2
    end

    __ktz_fp_ssh_link "$confd"/config.normal "$confd"/config
    return $status
end


# APT


function __ktz_fp_apt_on
    set -l conf_dir ~/.config/apt/conf.d
    set -l conf_file 99proxy

    set -l conf_path "$conf_dir/$conf_file"

    # apt is _very_ picky, that the proxy declaration
    # follows exactly the format described in their manpage
    # (e.g. it won't work without the trailing slash...)

    if [ ! -e "$conf_path" ]
        echo "setting up apt config in $conf_dir/$conf_file"
        mkdir -p "$conf_dir"
        echo -e 'Acquire::http::Proxy "'$KTZ_PROXY'/";\n' > "$conf_path"
    end

    echo "activating apt configuration"
    set -Ux APT_CONFIG "$conf_path"
end


function __ktz_fp_apt_off
    echo "deactivating apt configuration"
    set -e APT_CONFIG
end


# SNAP


function __ktz_fp_snap_on
    echo "setting snap system configuration"
    sudo snap set system proxy.http=$HTTP_PROXY
    sudo snap set system proxy.https=$HTTP_PROXY
end


function __ktz_fp_snap_off
    echo "unsetting snap system configuration"
    sudo snap unset system proxy.http
    sudo snap unset system proxy.https
end
