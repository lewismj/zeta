import json
from typing import Optional
from zeta.types.symbol import Symbol
from zeta.types.lambda_fn import Lambda
from zeta.types.macro_environment import MacroEnvironment

# ----------------- ANSI colors -----------------
RESET = "\033[0m"
COLOR_SYMBOL = "\033[94m"
COLOR_LAMBDA = "\033[92m"
COLOR_PY_CALLABLE = "\033[95m"
COLOR_QUASIQUOTE = "\033[96m"
COLOR_UNQUOTE = "\033[91m"
COLOR_UNQUOTE_SPLICING = "\033[93m"
COLOR_SPECIAL_FORM = "\033[90m"
COLOR_MACRO_LAMBDA = "\033[92m"
COLOR_MACRO_PY = "\033[95m"
COLOR_EXPANDED_MACRO = "\033[36m"

# ----------------- Defaults -----------------
DEFAULT_OPTIONS = {
    "max_line_length": 80,
    "max_depth": 5,
    "display_legend": False,
    "color_symbols": True,
    "color_lambda": True,
    "color_callables": True,
    "color_special_forms": True,
    "color_quasiquote": True,
    "color_unquote": True,
    "color_unquote_splicing": True,
    "color_macro_lambda": True,
    "color_macro_py": True,
    "color_expanded_macro": True,
}

SPECIAL_FORMS = {"define", "lambda", "if", "let", "defmacro", "quote"}


# ----------------- Colorize utility -----------------
def colorize(
    obj,
    macro_env: Optional[MacroEnvironment] = None,
    expanded: bool = False,
    in_quasiquote=False,
    in_unquote=False,
    in_unquote_splicing=False,
    options: dict = DEFAULT_OPTIONS,
):
    if isinstance(obj, Symbol):
        name = str(obj)
        if expanded and options.get("color_expanded_macro", True):
            return f"{COLOR_EXPANDED_MACRO}{name}{RESET}"
        if macro_env and obj in macro_env.macros:
            transformer = macro_env.macros[obj]
            if isinstance(transformer, Lambda) and options.get(
                "color_macro_lambda", True
            ):
                return f"{COLOR_MACRO_LAMBDA}{name}{RESET}"
            elif callable(transformer) and options.get("color_macro_py", True):
                return f"{COLOR_MACRO_PY}{name}{RESET}"
        if in_quasiquote and options.get("color_quasiquote", True):
            return f"{COLOR_QUASIQUOTE}{name}{RESET}"
        if in_unquote and options.get("color_unquote", True):
            return f"{COLOR_UNQUOTE}{name}{RESET}"
        if in_unquote_splicing and options.get("color_unquote_splicing", True):
            return f"{COLOR_UNQUOTE_SPLICING}{name}{RESET}"
        if name in SPECIAL_FORMS and options.get("color_special_forms", True):
            return f"{COLOR_SPECIAL_FORM}{name}{RESET}"
        if options.get("color_symbols", True):
            return f"{COLOR_SYMBOL}{name}{RESET}"
        return name
    if isinstance(obj, Lambda) and options.get("color_lambda", True):
        return f"{COLOR_LAMBDA}<Lambda {obj.formals}>{RESET}"
    if callable(obj) and options.get("color_callables", True):
        return f"{COLOR_PY_CALLABLE}<callable {obj.__name__}>{RESET}"
    return str(obj)


# ----------------- Pretty printer -----------------
def pprint_expr(
    expr,
    indent: int = 0,
    macro_env: Optional[MacroEnvironment] = None,
    expanded_symbols: Optional[set[Symbol]] = None,
    options: dict = DEFAULT_OPTIONS,
    _current_depth: int = 0,
    in_quasiquote=False,
    in_unquote=False,
    in_unquote_splicing=False,
) -> str:
    if expanded_symbols is None:
        expanded_symbols = set()

    pad = "  " * indent
    legend_str = ""
    if options.get("display_legend", True) and indent == 0:
        legend_items = [
            f"{COLOR_SYMBOL}Symbol{RESET}",
            f"{COLOR_LAMBDA}Lambda{RESET}",
            f"{COLOR_PY_CALLABLE}Callable Python function{RESET}",
            f"{COLOR_QUASIQUOTE}quasiquote`{RESET}",
            f"{COLOR_UNQUOTE}unquote,{RESET}",
            f"{COLOR_UNQUOTE_SPLICING}unquote-splicing,@{RESET}",
            f"{COLOR_SPECIAL_FORM}Special Form{RESET}",
            f"{COLOR_MACRO_LAMBDA}Lambda Macro{RESET}",
            f"{COLOR_MACRO_PY}Python Macro{RESET}",
            f"{COLOR_EXPANDED_MACRO}Already Expanded Macro{RESET}",
        ]
        legend_str = "Color Key: " + " | ".join(legend_items) + "\n"

    if _current_depth >= options.get("max_depth", 5):
        return legend_str + "…"

    is_expanded = False
    if isinstance(expr, Symbol) and macro_env and expr in macro_env.macros:
        is_expanded = True

    if isinstance(expr, Symbol) or isinstance(expr, Lambda) or callable(expr):
        return legend_str + colorize(
            expr,
            macro_env,
            is_expanded,
            in_quasiquote,
            in_unquote,
            in_unquote_splicing,
            options,
        )

    if isinstance(expr, tuple) and len(expr) == 2:
        lst, tail = expr
        lst_strs = [
            pprint_expr(
                x,
                indent + 1,
                macro_env,
                expanded_symbols,
                options,
                _current_depth + 1,
                in_quasiquote,
                in_unquote,
                in_unquote_splicing,
            )
            for x in lst
        ]
        tail_str = pprint_expr(
            tail,
            indent + 1,
            macro_env,
            expanded_symbols,
            options,
            _current_depth + 1,
            in_quasiquote,
            in_unquote,
            in_unquote_splicing,
        )
        inner = "\n".join("  " * (indent + 1) + s for s in lst_strs + [tail_str])
        return legend_str + f"(\n{inner}\n{pad})"

    if isinstance(expr, list):
        if not expr:
            return legend_str + "()"

        head = expr[0] if expr else None
        qq_flag, uq_flag, uqs_flag = in_quasiquote, in_unquote, in_unquote_splicing

        if isinstance(head, Symbol):
            if macro_env and head in macro_env.macros:
                expanded_symbols.add(head)
            if head.id == "quasiquote":
                qq_flag = True
            elif head.id == "unquote":
                uq_flag = True
            elif head.id == "unquote-splicing":
                uqs_flag = True

        parts = [
            pprint_expr(
                e,
                indent + 1,
                macro_env,
                expanded_symbols,
                options,
                _current_depth + 1,
                qq_flag,
                uq_flag,
                uqs_flag,
            )
            for e in expr
        ]

        single_line = "(" + " ".join(parts) + ")"
        if len(single_line) + indent * 2 <= options.get("max_line_length", 80):
            return legend_str + single_line

        aligned_lines = ["(" + parts[0]]
        for part in parts[1:]:
            aligned_lines.append("  " * (indent + 1) + part)
        aligned_lines[-1] += ")"
        return legend_str + "\n".join(aligned_lines)

    return legend_str + str(expr)


# ----------------- Load JSON config -----------------
def load_options_from_json(json_str: str) -> dict:
    try:
        user_opts = json.loads(json_str)
        merged_opts = {**DEFAULT_OPTIONS, **user_opts}
        return merged_opts
    except Exception:
        return DEFAULT_OPTIONS


# ----------------- Example usage -----------------
if __name__ == "__main__":
    macro_env = MacroEnvironment()
    macro_env.define_macro(Symbol("let-bindings"), lambda args, env: args)
    macro_env.define_macro(
        Symbol("inc"), Lambda([Symbol("x")], [Symbol("+"), Symbol("x"), 1])
    )

    # Example JSON configuration string
    json_config = (
        '{"max_line_length": 50, "display_legend": false, "color_symbols": false}'
    )
    options = load_options_from_json(json_config)

    xs = [
        Symbol("let-bindings"),
        [Symbol("&rest"), Symbol("bindings")],
        [
            Symbol("quasiquote"),
            [
                Symbol("let"),
                [Symbol("unquote-splicing"), Symbol("bindings")],
                [
                    Symbol("list"),
                    [
                        Symbol("unquote-splicing"),
                        [Symbol("map"), Symbol("car"), Symbol("bindings")],
                    ],
                ],
            ],
        ],
    ]

    print(pprint_expr(xs, macro_env=macro_env, options=options))
