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
I don't think that it's interesting for anyone besides me but for the
sake of documentation: Compiles fish from master and emacs 24.5 (and
that takes a while)...

### Local configuration ###

Run `./install.fish` from a fish instance.  This creates symlinks for
fish related stuff in `~/.config`, the emacs configuration in
`~/.emacs.d/mod.d` and the tmux configuration `~/.tmux.conf`. This
script is invoked by `install.apt.sh`.

### Desktop Tweaks Diary ###

I use terminator as my preferred terminal emulator. A configuration
containing both a dark theme (based on Cobalt Neon but heavily
adjusted) and a light theme (based on Material) can be found under
terminator/config.

I wrote a fish proxy function to adjust colors set by the fish
environment. `TODO: launcher config`

The icons I use are a modified version of
[Compass](http://gnome-look.org/content/show.php/Compass?content=160629). For
dark panels the status icons are not suitable - but they are just a
bunch of svg files. For my current theme a simple invokation of `sed`
did the trick:

`find . -name "*.svg" | xargs sed -i 's/304050/c1c6cc/'`

I use [Source Code
Pro](https://github.com/adobe-fonts/source-code-pro) for monospaced
fonts and [Source Sans
Pro](https://github.com/adobe-fonts/source-sans-pro) for Menus etc.


## Documentation ##

### alias.fish - Alias framework ###

#### Miscellaneous ####

| Command         | Equivalent                 |
|-----------------|----------------------------|
| `ll <args>`     | `ls -lhAp <args>`          |
| `e <args>`      | `emacs -nw <args>`         |
| `e -gui <args>` | `emacs <args>`             |
| `s <args>`      | `sudo -s <args>`           |


#### a - apt-* shortcuts ####

| Command        |                              |
|----------------|------------------------------|
| `a ar`         | `apt-get autoremove`         |
| `a c           | `apt-get clean               |
| `a dg          | `apt-get dist-upgrade        |
| `a i <args>`   | `apt-get install <args>`     |
| `a pg <args>`  | `apt-get purge <args>`       |
| `a r <args>`   | `apt-get remove <args>`      |
| `a s <args>`   | `apt-cache search <args>`    |
| `a ug`         | `apt-get upgrade`            |
| `a up`         | `apt-get update`             |


### r - Ruby shortcuts ###

Shortcut for `rails`, `<args>` may be optional

| Command          | Equivalent                               |
|------------------|------------------------------------------|
| `r v`            | `rails --version`                        |
| `r h`            | `rails --help`                           |
| `r c    <args>`  | `rails console <args>`                   |
| `r g    <args>`  | `rails generate <args>`                  |
| `r g c  <args>`  | `rails generate controller <args>`       |
| `r g it <args>`  | `rails generate integration_test <args>` |
| `r g md <args>`  | `rails generate model <args>`            |
| `r g mg <args>`  | `rails generate migration <args>`        |
| `r g v  <args>`  | `rails generate view <args>`             |
| `r d    <args>`  | `rails destroy <args>`                   |
| `r d c  <args>`  | `rails destroy controller <args>`        |
| `r d it <args>`  | `rails destroy integration_test <args>`  |
| `r d md <args>`  | `rails destroy model <args>`             |
| `r d mg <args>`  | `rails destroy migration <args>`         |
| `r d v  <args>`  | `rails destroy view <args>`              |
| `r gs   <args>`  | `rails generate scaffold <args>`         |
| `r n    <args>`  | `rails new <args>`                       |
| `r s    <args>`  | `rails server <args>`                    |


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

Used instance of fish is the brewed one.  To change the $PATH
environmental variable simply set/change `$fish_user_paths`:

```
set -U fish_user_paths your/own/path $fish_user_paths
```

Further information can be found with `help set`.  If you need a
persistent PATH variable change `/etc/paths` or your `~/.bashrc` or
`~/.profile` files.
