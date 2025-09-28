from zeta.types.errors import ZetaInvalidSymbol, ZetaArityError, ZetaTypeError
from zeta.types.symbol import Symbol
from zeta.types.lambda_fn import Lambda
from zeta.types.environment import Environment
from zeta.types.nil import Nil
from zeta import SExpression
from zeta.types.macro_environment import _substitute



def defmacro_form(tail, env, macros, evaluate_fn):
        if len(tail) < 2:
            raise ZetaArityError("defmacro requires a name and parameter list")

        # Extract macro name, parameters, and body
        macro_name = tail[0]
        if not isinstance(macro_name, Symbol):
            raise ZetaInvalidSymbol(f"Macro name must be a Symbol, got {macro_name}")

        params = tail[1]
        body = tail[2:]

        if not isinstance(params, list):
            raise ZetaTypeError("Macro parameter list must be a list")
        if not body:
            raise ZetaArityError("Macro body cannot be empty")

        # Wrap multiple expressions in a progn
        macro_body = body[0] if len(body) == 1 else [Symbol("progn")] + body
        macro_lambda = Lambda(params, macro_body, env)

        # Create a TransformerFunction that performs syntactic substitution
        def transformer(args: list[SExpression], call_env: Environment) -> SExpression:
            if len(args) != len(macro_lambda.formals):
                raise ZetaArityError(
                    f"Macro {macro_name} expected {len(macro_lambda.formals)} args, got {len(args)}"
                )
            # Create a temporary environment for substitution
            subst_env = Environment(outer=macro_lambda.env)
            for param, arg in zip(macro_lambda.formals, args):
                subst_env.define(param, arg)
            # _substitute is assumed to perform syntactic replacement without evaluating
            return _substitute(macro_lambda.body, subst_env, set(macro_lambda.formals))

        # Register the macro TransformerFunction
        macros.define_macro(macro_name, transformer)

        return Nil


