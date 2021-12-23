# dotfiles #

> Then world behind and home ahead,
> We'll wander back to home and bed.


## Description ##

General configurations and fish functions. The installer script
`install.fish` automatically links these files:

* `~/.tmux.conf`
* `~/.emacs.d/mod.d`
* `.config/fish/config.fish`
* `.config/fish/functions/*`


## Installation ##

Run `./install.fish`. This creates symlinks for fish related stuff in
`~/.config`, the emacs configuration in `~/.emacs.d/mod.d`, and the
tmux configuration `~/.tmux.conf`.


Example of how to use the emacs configurations:

```elisp
(defvar ktz/is-server nil
  "this is a desktop configuration")
(defvar ktz/mod-dir "~/.emacs.d/mod.d"
  "where the ktz configurations can be found")
(defvar ktz/org-dir "~/Complex/nxt/Roam"
  "org roam directory")
(load (concat ktz/mod-dir "/init"))
```

For the server configuration use `(defvar ktz/is-server t)`. The org
configuration is optional.

### Windows

They support symbolic links - great: `mklink mod.d
..\..\..\Complex\scm\dotfiles\emacs\mod.d`. The org-roam setup is
outlined in the [official
docs](https://www.orgroam.com/manual.html#C-Compiler). I've installed
msys2, ran:

``` shell
pacman -Syu
# new tty
pacman -Su
pacman -S --needed base-devel mingw-w64-x86_64-toolchain
pacman -S gcc
```

and added `<msys2 install dir>/usr/bin` to the windows `%PATH%` (Win+R
`systempropertiesadvanced` - Environment Variables - System Variables)

Problems with gpg keys and elpa if msys2 is installed: `gpg --receive-keys 066DAFCB81E42C40` (do not set --homedir to the `.emacs.d/elpa/gnupg` thing as emacs fucks the path up anyway; the keys are installed to the msys2 home directory). In `.emacs` add `'(package-gnupghome-dir nil)` to use the default dir.


## Documentation ##

### alias.fish - Alias framework ###

#### Miscellaneous ####

| Command          | Equivalent                 |
|------------------|----------------------------|
| `ll <args>`      | `ls -lhAp <args>`          |
| `e <args>`       | `emacs -nw <args>`         |
| `e -gui <args>`  | `emacs <args>`             |
| `s <args>`       | `sudo fish -c "<args>"`    |

#### c - systemctl and journalctl ####

| Command     |                             |
|-------------|-----------------------------|
| `c`         | `systemctl`                 |
| `c dis [0]` | `systemctl disable [0]`     |
| `c dr`      | `systemctl daemon-reload    |
| `c en [0]`  | `systemctl enable [0]`      |
| `c j [0]`   | `journalctl -u [0] -b`      |
| `c jf [0]`  | `journalctl -u [0] -b -f`   |
| `c m [0]`   | `systemctl mask [0]`        |
| `c rl [0]`  | `systemctl reload [0]`      |
| `c rs [0]`  | `systemctl restart [0]`     |
| `c sa [0]`  | `systemctl start [0]`       |
| `c so [0]`  | `systemctl stop [0]`        |
| `c st [0]`  | `systemctl status [0]`      |
| `c u [0]`   | `systemctl unmask [0]`      |


#### a - apt-* shortcuts ####

| Command    |                        |
|------------|------------------------|
| `a`        | `apt`                  |
| `a ar`     | `apt autoremove`       |
| `a c`      | `apt-get clean`        |
| `a dg`     | `apt full-upgrade`     |
| `a i [0]`  | `apt install [0]`      |
| `a pg [0]` | `apt purge [0]`        |
| `a r [0]`  | `apt remove [0]`       |
| `a ug`     | `apt upgrade`          |
| `a up`     | `apt update`           |
| `a d [0]`  | `apt-get download [0]` |
| `a s [0]`  | `apt-cache search [0]` |
| `a fs [0]` | `apt-file search [0]`  |
| `a sh [0]` | `apt show [0]`         |

#### g - Git shortcuts ####

Shortcuts for `git`. `args` may be optional.

| Command        | Equivalent               | Description            |
|----------------|--------------------------|------------------------|
| `g `           | `git`                    | `git`                  |
| `g a   <args>` | `git add <args>`         | `git help add`         |
| `g b   <args>` | `git branch <args>`      | `git help branch`      |
| `g c   <args>` | `git commit <args>`      | `git help commit`      |
| `g f   <args>` | `git fetch <args>`       | `git help fetch`       |
| `g cl  <args>` | `git clone <args>`       | `git help clone`       |
| `g co  <args>` | `git checkout <args>`    | `git help checkout`    |
| `g d   <args>` | `git diff <args>`        | `git help diff`        |
| `g gpl <args>` | -                        | pull from all remotes  |
| `g gps <args>` | -                        | push to all remotes    |
| `g h   <args>` | `git help <args>`        | `git help`             |
| `g l   <args>` | `git log <args>`         | `git help log`         |
| `g mg  <args>` | `git merge <args>`       | `git help merge`       |
| `g mv  <args>` | `git mv <args>`          | `git help mv`          |
| `g ps  <args>` | `git push <args> <args>` | `git help push <args>` |
| `g pl  <args>` | `git pull <args>`        | `git help pull`        |
| `g plr <args>` | `git pull --rebase <a>`  | `git help pull`        |
| `g rm  <args>` | `git rm <args>`          | `git help rm`          |
| `g rt  <args>` | `git remote <args>`      | `git help remote`      |
| `g s   <args>` | `git status <args>`      | `git help status`      |
| `g st  <args>` | `git stash <args>`       | `git help stash`       |
| `g u   <args>` | `git reset HEAD <args>`  | unstage file           |
| `g v`          | `git version`            | -                      |


#### t - tmux shortcuts ####

Shortcuts for `tmux`. `args` may be optional.

| Command                         | Equivalent                                     | Description                |
|---------------------------------|------------------------------------------------|----------------------------|
| `t ls`                          | `tmux list-sessions`                           | List current tmux sessions |
| `t a [<session-name>] [<args>]` | `tmux attach-session <args> -t <session-name>` | Create new client          |
| `t n [<session-name>] [<args>]` | `tmux new-session <args> -s <session-name>`    | Create new sessions        |


## Notes ##

__Some handy commands__

* Switch alt and cmd when using Apple™ Keyboards: `setxkbmap -option altwin:swap_alt_win` or `echo 1 | sudo tee /sys/module/hid_apple/parameters/swap_opt_cmd`

__Environment__

To change the $PATH environmental variable simply set/change
`$fish_user_paths`:

```
set -U fish_user_paths your/own/path $fish_user_paths
```

Further information can be found with `help set`.  If you need a
persistent PATH variable change `/etc/paths` or your `~/.bashrc` or
`~/.profile` files.
