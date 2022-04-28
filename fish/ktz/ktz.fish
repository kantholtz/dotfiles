# general environment preps
# this file is guaranteed to be executed first

function ktz-echo \
    -a from msg exitcode \
    -d "echo things if KTZ_DEBUG is set or exit with an error message"

    if [ -n "$exitcode" ];
        echo $msg
        exit $exitcode
    end

    if set -q KTZ_DEBUG
        set -l timestamp (date +'%H:%M:%S')
        echo -e "[$timestamp] $from: $msg"
    end
end


ktz-echo "ktz.fish" "KTZ_DEBUG is set, will log debug messages"


#
#    initialize enviroment
#
ktz-echo "ktz.fish" "initializing environment"

set -gx ktz_dir_fish (realpath $ktz_dir_root/..)
set -gx ktz_dir_dotfiles (realpath $ktz_dir_fish/..)

ktz-echo "ktz.fish" (echo \
    "current environment:\n" \
    "  ktz_dir_fish $ktz_dir_fish\n" \
    "  ktz_dir_root $ktz_dir_root\n" \
    "  ktz_dir_dotfiles $ktz_dir_dotfiles\n" \
    | string collect)


# own color definitions

# red
set -g ktz_clr_primary         'c2185b'
set -g ktz_clr_primary_light   'fa5788'
set -g ktz_clr_primary_dark    '8c0032'

set -g ktz_clr_primary_fallback   magenta
set -g ktz_clr_primary_brfallback brmagenta

# green
set -g ktz_clr_secondary        '00695c'
set -g ktz_clr_secondary_light  '439688'
set -g ktz_clr_secondary_dark   '003d32'

set -g ktz_clr_secondary_fallback    green
set -g ktz_clr_secondary_brfallback  brgreen

# steel blue
set -g ktz_clr_subtle           '455a64'
set -g ktz_clr_subtle_light     '9ea7aa'
set -g ktz_clr_subtle_dark      '1c313a'

set -g ktz_clr_subtle_fallback    brgreen
set -g ktz_clr_subtle_brfallback  brblack


# to autoload ktz functions
set fish_function_path $ktz_dir_root $fish_function_path
ktz-echo "ktz.fish" "aded to fish_function_path - $ktz_dir_root"


# aliases must be sourced as they are not resolved
# by fish using fish_function_path
source $ktz_dir_root/ktz-alias.fish


# fish overwrites

set fish_color_command \
    $ktz_clr_secondary \
    $ktz_clr_secondary_fallback

set fish_color_param   \
    $ktz_clr_secondary_light \
    $ktz_clr_secondary_brfallback

set fish_color_error \
    $ktz_clr_primary \
    $ktz_clr_primary_fallback

set fish_color_quote \
    $ktz_clr_secondary_light \
    $ktz_clr_secondary_brfallback


alias fish_prompt ktz-prompt
alias fish_greeting "$ktz_dir_dotfiles/bash/verses.sh $ktz_dir_dotfiles/assets/verses"
