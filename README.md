# ktz dotfiles #

> Back already? No matter.
> Lay out your arms.


## Installation ##

```console
# see example ansible/inventory.yaml
ansible-playbook -i <inventory-file> ansible/playbook.yaml
```


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


**GPG Keys:** Problems with gpg keys and elpa if msys2 is installed: `gpg
--receive-keys 066DAFCB81E42C40` (do not set --homedir to the
`.emacs.d/elpa/gnupg` thing as emacs is confused with the paths; the
keys are installed to the msys2 home directory). In `.emacs` add
`'(package-gnupghome-dir nil)` to use the default dir.

