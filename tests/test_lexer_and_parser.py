import pytest
from hypothesis import given, strategies as st

from zeta.types.nil import Nil
from zeta.types.symbol import Symbol
from zeta.reader.parser import lex, TokenStream

# Convert nested list to Lisp source string
def _to_lisp_source(expr):
    if isinstance(expr, list):
        return f"({' '.join(_to_lisp_source(e) for e in expr)})"
    elif isinstance(expr, str):
        if expr.startswith('"') and expr.endswith('"'):
            return expr
        return expr
    else:
        return str(expr)


@pytest.mark.parametrize(
    "source,expected",
    [
        ("a", [("symbol", "a")]),
        ("'a", [("quote", "'"), ("symbol", "a")]),
        ("(a b c)", [("lparen", "("), ("symbol", "a"), ("symbol", "b"), ("symbol", "c"), ("rparen", ")")]),
        ('"hello"', [("string", '"hello"')]),
        ("#b1010", [("radix", "#b1010")]),
        ("#1=(a b c) #1#", [("shared_def", "#1="), ("lparen", "("), ("symbol", "a"), ("symbol", "b"), ("symbol", "c"), ("rparen", ")"), ("shared_ref", "#1#")]),
        ("#C(1 2)", [("complex", "#C"), ("lparen", "("), ("symbol", "1"), ("symbol", "2"), ("rparen", ")")]),
        (" ; comment\n a b", [("symbol", "a"), ("symbol", "b")]),
        ("'x", [("quote", "'"), ("symbol", "x")]),
        ("`y", [("quote", "`"), ("symbol", "y")]),
        (",z", [("unquote", ","), ("symbol", "z")]),
        (",@w", [("unquote", ",@"), ("symbol", "w")]),
        ('"hello"', [("string", '"hello"')]),
        ("#C(1 2)", [("complex", "#C"), ("lparen", "("), ("symbol", "1"), ("symbol", "2"), ("rparen", ")")]),
        ("#b1010 #o12 #xA", [("radix", "#b1010"), ("radix", "#o12"), ("radix", "#xA")]),
        ("#*1101", [("bitstring", "#*1101")]),
        ("#s(point :x 1 :y 2)",[    ('symbol', '#s'),
                                    ('lparen', '('),
                                    ('symbol', 'point'),
                                    ('symbol', ':x'),
                                    ('symbol', '1'),
                                    ('symbol', ':y'),
                                    ('symbol', '2'),
                                    ('rparen', ')'),]),
    ]
)
def test_lexer_basic(source, expected):
    tokens = list(lex(source))
    assert tokens == expected

@pytest.mark.parametrize(
    "source, expected",
    [
        ("nil", Nil),
        ("123", 123),
        ("-45", -45),
        ("3.14", 3.14),
        ("'a",  [Symbol('quote'), Symbol('a')]),
        ("(a b c)", [Symbol('a'), Symbol('b'), Symbol('c')]),
        ("(a . b)", ([Symbol('a')], Symbol('b'))),  # dotted list
        ("#C(1 2)", complex(1, 2)),
        ('"hello"', "hello"),
        ("#*1010", [1, 0, 1, 0]),
        ("#s(point :x 1 :y 2)", [Symbol('make-point'), 1, 2]),
        ("#.(+ 1 2)", {"read-eval": [Symbol("+"), 1, 2]}),
        ("#'my-func", ["function", Symbol("my-func")]),
        ("#b1010", 10),
        ("#o12", 10),
        ("#xA", 10),
    ]
)
def test_parser(source, expected):
    stream = TokenStream(lex(source))
    result = list(stream.parse_all())
    assert result[0] == expected     # Parser yields one expression

def test_nested_lists():
    source = "((a b) (c d))"
    expected = [[Symbol('a'), Symbol('b')], [Symbol('c'), Symbol('d')]]
    stream = TokenStream(lex(source))
    result = list(stream.parse_all())
    assert result[0] == expected


@pytest.mark.parametrize(
    "source,expected",
    [
        ("#\\a", "a"),           # single char
        ("#\\space", " "),       # named char
        ("#\\newline", "\n"),
        ("#\\tab", "\t"),
        ("#\\return", "\r"),
    ]
)
def test_char_parsing(source, expected):
    """Test that TokenStream parses character literals and normalizes them."""
    stream = TokenStream(lex(source))
    result = list(stream.parse_all())
    assert result[0] == expected

@pytest.mark.parametrize(
    "source,expected",
    [
        ("#\\a", [("char", "#\\a")]),  # lexer returns raw token
        ("#\\space", [("char", "#\\space")]),
        ("#\\newline", [("char", "#\\newline")]),
        ("#\\tab", [("char", "#\\tab")]),
    ]
)
def test_char_lexer_tokens(source, expected):
    """Test that lexer yields the correct raw tokens for chars."""
    tokens = list(lex(source))
    assert tokens == expected

@pytest.mark.parametrize(
    "source,expected",
    [
        ("#'my-func", ["function", Symbol("my-func")]),
        ("#.(+ 1 2)", {"read-eval": [Symbol("+"), 1, 2]}),
    ]
)
def test_reader_macro_parsing(source, expected):
    """Test function shorthand (#') and read-time eval (#.) reader macros."""
    stream = TokenStream(lex(source))
    result = list(stream.parse_all())
    assert result[0] == expected

@pytest.mark.parametrize(
    "source",
    [
        "",             # empty string
        "    ",         # spaces only
        "; comment",    # comment only
        "#| multi-line \n comment |#",  # multiline comment
    ]
)
def test_lexer_edge_cases_no_crash(source):
    try:
        tokens = list(lex(source))
    except Exception as e:
        assert False, f"Lexer crashed on {source!r}: {e}"

@pytest.mark.parametrize("source,expected", [
    ("(a b c)", [Symbol('a'), Symbol('b'), Symbol('c')]),
    ("#(1 2 3)", [1, 2, 3]),
    ("(a . b)", ([Symbol('a')], Symbol('b'))),
    ("#C(1 2)", complex(1, 2)),
])
def test_parse_roundtrip(source, expected):
    stream = TokenStream(lex(source))
    result = list(stream.parse_all())
    assert result[0] == expected

def test_vector_simple():
    source = "#(1 2 3)"
    expected = [1, 2, 3]
    stream = TokenStream(lex(source))
    result = list(stream.parse_all())
    assert result[0] == expected

def test_vector_nested():
    source = "#(1 #(2 3) 4)"
    expected = [1, [2, 3], 4]
    stream = TokenStream(lex(source))
    result = list(stream.parse_all())
    assert result[0] == expected

# -------------------------------
# Helpers
# -------------------------------
def _escape_string(s: str) -> str:
    escaped = s.encode('unicode_escape').decode('ascii')
    escaped = escaped.replace('"', '\\"')
    return f'"{escaped}"'

def _get_src_from_sexpr(sexpr):
    if isinstance(sexpr, tuple):  # dotted list
        lst, tail = sexpr
        return "(" + " ".join(map(str, lst)) + " . " + str(tail) + ")"
    elif isinstance(sexpr, list):
        return "(" + " ".join(map(str, sexpr)) + ")"
    else:
        return str(sexpr)

# -------------------------------
# Strategies
# -------------------------------
symbol_strat = st.text(
    st.characters(whitelist_categories=("Ll", "Lu", "Nd", "Pc"),
                  whitelist_characters="-_"),
    min_size=1, max_size=10
).filter(lambda s: s.lower() != "nil")

string_strat = st.text(min_size=0, max_size=20).map(_escape_string)

number_strat = st.one_of(
    st.integers(min_value=-1000, max_value=1000),
    st.floats(min_value=-1e6, max_value=1e6, allow_infinity=False, allow_nan=False)
)

list_strat = st.lists(st.one_of(symbol_strat, number_strat, string_strat), max_size=5)

dotted_list_strat = st.tuples(
    list_strat.filter(lambda l: len(l) > 0),
    st.one_of(symbol_strat, number_strat, string_strat)
)

sexpr_strat = st.one_of(symbol_strat, string_strat, number_strat, list_strat, dotted_list_strat)

# -------------------------------
# Hypothesis tests
# -------------------------------
@given(sexpr_strat)
def test_lexer_no_crash(sexpr):
    source = _get_src_from_sexpr(sexpr)
    try:
        tokens = list(lex(source))
    except Exception as e:
        assert False, f"Lexer crashed on {source!r}: {e}"

@given(sexpr_strat)
def test_parser_no_crash(sexpr):
    source = _get_src_from_sexpr(sexpr)
    try:
        stream = TokenStream(lex(source))
        parsed = list(stream.parse_all())
    except Exception as e:
        assert False, f"Parser crashed on {source!r}: {e}"