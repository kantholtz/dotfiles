
# to print debugging information
# it is looked whether this variable is set at all
# and its value is irrelevant... may introduce log
# levels at some point, however
# set -gx KTZ_DEBUG


#
#    bootstrapping
#

# determine file paths:
# config.fish MUST be a symbolic link pointing
# to the dotfiles repository
set -g ktz_dir_root (dirname (status filename))/ktz
source $ktz_dir_root/ktz.fish
