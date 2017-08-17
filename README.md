# dotfiles #

Here I gather all stuff I need to replicate my setup to new Maschines -
both server without any Window Manager and Desktops.

## Description ##

General configurations and fish functions. The installer script
automatically installs these files:

* `~/.emacs`
* `~/.tmux.conf`
* `.config/fish/config.fish`
* `.config/fish/functions/*`


## Installation ##

Most non-ui installation can be done automated. Full installation:
`wget https://raw.githubusercontent.com/dreadworks/dotfiles/master/install.apt.sh -O i && chmod u+x i && ./i; rm i`

### Provisioning ###

I use `./install.apt.sh [server]` if I need a basic set of programs.

### Local configuration ###

Run `./install.fish` from a fish instance.  This creates symlinks for
fish related stuff in `~/.config`, the emacs configuration in
`~/.emacs.d/mod.d` and the tmux configuration `~/.tmux.conf`. This
script is invoked by `install.apt.sh`.

### Desktop Tweaks Diary ###

I use terminator as my preferred terminal emulator. A configuration
containing both a dark theme and a light theme can be found under
terminator/config. I wrote a fish proxy function to adjust colors set
by the fish environment. To start from a launcher: `fish -c 'terminator dark|bright`

I use [Source Code
Pro](https://github.com/adobe-fonts/source-code-pro) for monospaced
fonts and [Source Sans
Pro](https://github.com/adobe-fonts/source-sans-pro) for Menus etc.

## Documentation ##

### alias.fish - Alias framework ###

#### Miscellaneous ####

| Command          | Equivalent                 |
|------------------|----------------------------|
| `ll <args>`      | `ls -lhAp <args>`          |
| `e <args>`       | `emacs -nw <args>`         |
| `e -gui <args>`  | `emacs <args>`             |
| `s <args>`       | `sudo fish -c "<args>"`    |
| `q <host> <user>`| `ssh user@host`            |

#### a - apt-* shortcuts ####

| Command        |                              |
|----------------|------------------------------|
| `ar`           | `apt autoremove`             |
| `c`            | `apt-get clean`              |
| `dg`           | `apt full-upgrade`           |
| `i [0]`        | `apt install [0]`            |
| `pg [0]`       | `apt purge [0]`              |
| `r [0]`        | `apt remove [0]`             |
| `ug`           | `apt upgrade`                |
| `up`           | `apt update`                 |
| `d [0]`        | `apt-get download [0]`       |
| `s [0]`        | `apt-cache search [0]`       |
| `fs [0]`       | `apt-file search [0]`        |
| `sh [0]`       | `apt show [0]`               |
| `l [0]`        | `dpkg -l | grep [0]`         |

#### g - Git shortcuts ####

Shortcuts for `git`. `args` may be optional.

| Command        | Equivalent              | Description           |
|----------------|-------------------------|-----------------------|
| `g a   <args>` | `git add <args>`        | `git help add`        |
| `g b   <args>` | `git branch <args>`     | `git help branch`     |
| `g c   <args>` | `git commit <args>`     | `git help commit`     |
| `g cl  <args>` | `git clone <args>`      | `git help clone`      |
| `g co  <args>` | `git checkout <args>`   | `git help checkout`   |
| `g d   <args>` | `git diff <args>`       | `git help diff`       |
| `g gpl <args>` | -                       | pull from all remotes |
| `g gps <args>` | -                       | push to all remotes   |
| `g h   <args>` | `git help <args>`       | `git help`            |
| `g l   <args>` | `git log <args>`        | `git help log`        |
| `g mg  <args>` | `git merge <args>`      | `git help merge`      |
| `g mv  <args>` | `git mv <args>`         | `git help mv`         |
| `g ps  <args>` | `git push <args> <args>`| `git help push <args>`|
| `g pl  <args>` | `git pull <args>`       | `git help pull`       |
| `g plr <args>` | `git pull --rebase <a>` | `git help pull`       |
| `g rm  <args>` | `git rm <args>`         | `git help rm`         |
| `g rt  <args>` | `git remote <args>`     | `git help remote`     |
| `g s   <args>` | `git status <args>`     | `git help status`     |
| `g st  <args>` | `git stash <args>`      | `git help stash`      |
| `g v`          | `git version`           | -                     |


#### t - tmux shortcuts ####

Shortcuts for `tmux`. `args` may be optional.

| Command                         | Equivalent                                     | Description                |
|---------------------------------|------------------------------------------------|----------------------------|
| `t ls`                          | `tmux list-sessions`                           | List current tmux sessions |
| `t a [<session-name>] [<args>]` | `tmux attach-session <args> -t <session-name>` | Create new client          |
| `t n [<session-name>] [<args>]` | `tmux new-session <args> -s <session-name>`    | Create new sessions        |


### crypt.fish - Encrypt files ###

Small and rudimentary shortcut to easily encrypt and decrypt single
files. Uses openssl's 256bit aes encryption.

```
crypt -(d|e|q|z) input [output]
  switches:
    -d decrypt
    -e encrypt
    -q quiet mode
    -z delete source file

  -d and -e exclude each other

  input: Input file
  output: (Optional) output file
```

If no output file is given the program assigns the output file a name
automatically based on the operation. It adds .aes256 to the encrypted
file and removes this suffix for decrypted files.

`crypt -e foo.tar.gz` produces `foo.tar.gz.aes256` while
`crypt -d foo.tar.gz.aes256` produces `foo.tar.gz`.


## Notes ##

__Some handy commands__

* Switch alt and cmd when using Apple™ Keyboards: `setxkbmap -option altwin:swap_alt_win`

__Environment__

To change the $PATH environmental variable simply set/change
`$fish_user_paths`:

```
set -U fish_user_paths your/own/path $fish_user_paths
```

Further information can be found with `help set`.  If you need a
persistent PATH variable change `/etc/paths` or your `~/.bashrc` or
`~/.profile` files.
