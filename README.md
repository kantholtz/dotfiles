# dotfiles #

## Description ##

General configurations and fish functions.


## Installation ##

Run `./install.fish` from a fish instance.


## Documentation (incomplete) ##


### be.fish ###

Shortcuts for `bundle exec` and `bundle exec rake`.
If there is a local `./bin/rake` it is used instead.

| Command        | Equivalent                |
----------------------------------------------
| `be <args>`    | `bundle exec <args>`      |
| `be r <args>`  | `bundle exec rake <args>` |



### crypt.fish ###

Small and rudimentary shortcut to easily encrypt and decrypt single
files. Uses openssl's 256bit aes encryption.

```
crypt -(d|e|q|z) input [output]
  switches:
    -d decrypt
    -e encrypt
    -q quiet mode
    -z delete source file

  -d and -e are mutually exclusive

  input: Input file
  output: (Optional) output file
```

If no output file is given the program assigns the output file a name
automatically based on the operation. It adds .aes256 to the encrypted
file and removes this suffix for decrypted files.

`crypt -e foo.tar.gz` produces `foo.tar.gz.aes256` while
`crypt -d foo.tar.gz.aes256` produces `foo.tar.gz`.


### ll.fish ###

Shortcut for `ls -lAp`


### r.fish ###

Shortcut for `rails`. The version number `<vers>` is always
optional. `<args>` may be optional

| Command                 | Equivalent                                    |
|-------------------------|-----------------------------------------------|
| r <vers> c <args>       | rails <vers> console <args>                   |
| r <vers> g <args>       | rails <vers> generate <args>                  |
| r <vers> g c <args>     | rails <vers> generate controller <args>       |
| r <vers> g it <args>    | rails <vers> generate integration_test <args> |
| r <vers> g md <args>    | rails <vers> generate model <args>            |
| r <vers> g mg <args>    | rails <vers> generate migration <args>        |
| r <vers> g v <args>     | rails <vers> generate view <args>             |
| r <vers> d <args>       | rails <vers> destroy <args>                   |
| r <vers> d c <args>     | rails <vers> destroy controller <args>        |
| r <vers> d it <args>    | rails <vers> destroy integration_test <args>  |
| r <vers> d md <args>    | rails <vers> destroy model <args>             |
| r <vers> d mg <args>    | rails <vers> destroy migration <args>         |
| r <vers> d v <args>     | rails <vers> destroy view <args>              |
| r <vers> gs <args>      | rails <vers> generate scaffold <args>         |
| r <vers> n <args>       | rails <vers> new <args>                       |
| r <vers> s <args>       | rails <vers> server <args>                    |


### g.fish ###

Shortcuts for `git`. `args` may be optional.

| Command        | Equivalent              | Description           |
|----------------|-------------------------|-----------------------|
| `g a <args>`   | `git add <args>`        | `git help add`        |
| `g b <args>`   | `git branch <args>`     | `git help branch`     |
| `g s <args>`   | `git status <args>`     | `git help status`     |
| `g c <args>`   | `git commit <args>`     | `git help commit`     |
| `g c <args>`   | `git clone <args>`      | `git help clone`      |
| `g co <args>`  | `git checkout <args>`   | `git help checkout`   |
| `g l <args>`   | `git log <args>`        | `git help log`        |
| `g ps <args>`  | `git push <args> <args>`| `git help push <args>`|
| `g gps <args>` | -                       | push to all remotes   |
| `g pl <args>`  | `git pull <args>`       | `git help pull`       |
| `g gpl <args>` | -                       | pull from all remotes |
| `g d <args>`   | `git diff <args>`       | `git help diff`       |
| `g mg <args>`  | `git merge <args>`      | `git help merge`      |
| `g rm <args>`  | `git rm <args>`         | `git help rm`         |
| `g rt <args>`  | `git remote <args>`     | `git help remote`     |
| `g mv <args>`  | `git mv <args>`         | `git help mv`         |


Notes:
------

Used instance of fish is the brewed one.  To change the $PATH
environmental variable simply set/change `$fish_user_paths`:

```
set -U fish_user_paths your/own/path $fish_user_paths
```

Further information can be found with `help set`.  If you need a
persistent PATH variable change `/etc/paths` or your `~/.bashrc` or
`~/.profile` files.
