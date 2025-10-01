from zeta.types.errors import ZetaArityError, ZetaInvalidSymbol
from zeta.types.nil import Nil
from zeta.types.symbol import Symbol


def defstruct_form(tail, env, macros, evaluate_fn, _):
    if not tail or not isinstance(tail[0], Symbol):
        raise ZetaArityError("defstruct requires a struct name")
    struct_name = tail[0]
    fields = tail[1:]

    def make_struct(env_inner, args):
        if len(args) != len(fields):
            raise ZetaArityError(f"{struct_name} constructor expects {len(fields)} args")
        return {"__type__": struct_name, **dict(zip(fields, args))}

    # Mapping with reader macro #s:
    # The reader expands literals of the form #s(<name> <field1> v1 <field2> v2 ...)
    # into a constructor call (make-<name> v1 v2 ...). Therefore, make-<name>
    # must accept positional arguments in the same order as fields were declared
    # in this defstruct form.
    constructor_name = Symbol(f"make-{struct_name}")
    env.define(constructor_name, make_struct)

    for field in fields:
        if not isinstance(field, Symbol):
            raise ZetaInvalidSymbol(f"defstruct field must be Symbol, got {field}")
        accessor_name = Symbol(f"{struct_name}-{field}")

        def make_accessor(f):
            return lambda env_inner, args: args[0][f]

        env.define(accessor_name, make_accessor(field))
    return Nil