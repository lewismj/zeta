import numpy as np

def to_list(x):
    """Convert any iterable to Python list; wrap scalars in list."""
    try:
        iter(x)
        # avoid string iteration
        if isinstance(x, (str, bytes)):
            return [x]
        return list(x)
    except TypeError:
        return [x]

def to_bool(x):
    """Convert numpy or Python types to a single boolean."""
    if hasattr(x, 'any'):
        return bool(x.any())
    return bool(x)
