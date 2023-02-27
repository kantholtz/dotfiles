# dotfiles #

> Then world behind and home ahead,
> We'll wander back to home and bed.


## Installation ##

For the *fish* shell, you simply add the following to your local `fish.config`:

```
source path/to/dotfiles/fish/config.fish
```

To configure *emacs* add the following to your `.emacs` and run `M-x
ktz-customize` (an alias for `M-x customize-group RET ktz RET`) and
choose a mode:

```elisp
;; configure it with M-x ktz-customize
(add-to-list 'load-path "~/path/to/dotfiles/emacs")
(require 'ktz)
```

For *tmux*, you can simply link the configuration file in `tmux` to
your home directory.


### Windows

The org-roam setup is outlined in the [official
docs](https://www.orgroam.com/manual.html#C-Compiler). This worked for
me: install msys2 and run:

``` shell
pacman -Syu
# new tty
pacman -Su
pacman -S --needed base-devel mingw-w64-x86_64-toolchain
pacman -S gcc
```

Then add `<msys2 install dir>/usr/bin` to the windows `%PATH%` (Win+R
`systempropertiesadvanced` - Environment Variables - System Variables)


**Exec Format Error:** If `C-c n f` does not work and the stacktrace complains about an `Exec
format error` when spawning the emacsql-sqlite subprocess, evaluate
`(emacsql-sqlite-compile)`.

**GPG Keys:** Problems with gpg keys and elpa if msys2 is installed: `gpg
--receive-keys 066DAFCB81E42C40` (do not set --homedir to the
`.emacs.d/elpa/gnupg` thing as emacs fucks the path up anyway; the
keys are installed to the msys2 home directory). In `.emacs` add
`'(package-gnupghome-dir nil)` to use the default dir.

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
