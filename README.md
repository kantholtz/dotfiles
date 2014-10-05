dotfiles
========

configurational things


Notes:
------

Used instance of fish is the brewed one.

To change the $PATH environmental variable simply
set/change `$fish_user_paths`:

```
set -U fish_user_paths your/own/path $fish_user_paths
```

Further information can be found with `help set`.

If you need a persistent PATH variable change `/etc/paths` or
your `~/.bashrc` or `~/.profile` files.
