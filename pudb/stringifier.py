# -*- coding: utf-8 -*-

from pudb.var_view import default_stringifier as pudb_str

# fmt: off
try:
    import torch
    _torch = True
except ImportError:
    _torch = False
# fmt: on


# To better render tensors and to avoid cluttering
# the logfile with len() of zero-length tensor exceptions
# Register with Ctl-P -> Variable Stringifier -> Custom
# and enter the path to this file or use a XDG spec config
# file place (e.g. ~/.config/pudb)
def pudb_stringifier(obj):
    if _torch and isinstance(obj, torch.Tensor):
        dtype = str(obj.dtype).split(".")[1]

        if not obj.numel():
            suffix = "empty"
        elif obj.numel() == 1:
            suffix = f"value: {obj.item()}"
        else:
            suffix = "dims: " + " x ".join(map(str, obj.shape))

        return f"Tensor({dtype}) {suffix}"

    return pudb_str(obj)
