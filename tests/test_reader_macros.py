import pytest
from zeta.parser import lex


# ----------------------------------------
# 1. Character reader macro tests
# ----------------------------------------
@pytest.mark.parametrize(
    "source, expected",
    [
        ("#\\a", [('char', '#\\a')]),
        ("#\\Z", [('char', '#\\Z')]),
        ("#\\space", [('char', '#\\space')]),
        ("#\\newline", [('char', '#\\newline')]),
        ("#\\tab", [('char', '#\\tab')]),
        ("#\\return", [('char', '#\\return')]),
        ("#\\\"", [('char', '#\\"')]),      # escaped double quote
        ("#\\n", [('char', '#\\n')]),       # literal backslash-n
    ]
)
def test_char_reader_macros(source, expected):
    tokens = list(lex(source))
    assert tokens == expected

# ----------------------------------------
# 2. Quote / quasiquote / unquote
# ----------------------------------------
@pytest.mark.parametrize(
    "source, expected",
    [
        ("'x", [('quote', "'"), ('symbol', 'x')]),
        ("`y", [('quote', "`"), ('symbol', 'y')]),
        (",z", [('unquote', ","), ('symbol', 'z')]),
        (",@w", [('unquote', ",@"), ('symbol', 'w')]),
        # Nested quoting
        ("`'(a b)", [('quote', "`"), ('quote', "'"), ('lparen', '('), ('symbol', 'a'), ('symbol', 'b'), ('rparen', ')')]),
        ("`',x", [('quote', "`"), ('unquote', ','), ('symbol', 'x')]),
        ("`,@y", [('quote', "`"), ('unquote', ',@'), ('symbol', 'y')]),
    ]
)
def test_quote_reader_macros(source, expected):
    tokens = list(lex(source))
    assert tokens == expected

# ----------------------------------------
# 3. Vector reader macro tests
# ----------------------------------------
@pytest.mark.parametrize(
    "source, expected",
    [
        ("#()", [('vector', '#('), ('rparen', ')')]),
        ("#(1 2 3)", [('vector', '#('), ('symbol', '1'), ('symbol', '2'), ('symbol', '3'), ('rparen', ')')]),
        ("#(a b c)", [('vector', '#('), ('symbol', 'a'), ('symbol', 'b'), ('symbol', 'c'), ('rparen', ')')]),
        ("#(#\\a #\\b #\\space)", [('vector', '#('), ('char', '#\\a'), ('char', '#\\b'), ('char', '#\\space'), ('rparen', ')')]),
        ("#(#() #(1 2))", [('vector', '#('), ('vector', '#('), ('rparen', ')'), ('vector', '#('), ('symbol', '1'), ('symbol', '2'), ('rparen', ')'), ('rparen', ')')]),
    ]
)
def test_vector_reader_macros(source, expected):
    tokens = list(lex(source))
    assert tokens == expected

# ----------------------------------------
# 4. Set reader macro tests
# ----------------------------------------
@pytest.mark.parametrize(
    "source, expected",
    [
        ("#{}", [('set_macro', '#{'), ('rbrace', '}')]),                   # empty set
        ("#{1 2 3}", [('set_macro', '#{'), ('symbol', '1'), ('symbol', '2'), ('symbol', '3'), ('rbrace', '}')]),
        ("#{a b c}", [('set_macro', '#{'), ('symbol', 'a'), ('symbol', 'b'), ('symbol', 'c'), ('rbrace', '}')]),
        ("#{#\\a #\\space}", [('set_macro', '#{'), ('char', '#\\a'), ('char', '#\\space'), ('rbrace', '}')]),
        ("#{#() #(1 2)}", [('set_macro', '#{'), ('vector', '#('), ('rparen', ')'), ('vector', '#('), ('symbol', '1'), ('symbol', '2'), ('rparen', ')'), ('rbrace', '}')]),
    ]
)
def test_set_reader_macros(source, expected):
    tokens = list(lex(source))
    assert tokens == expected

# ----------------------------------------
# 5. Nested / complex reader macro combinations
# ----------------------------------------
@pytest.mark.parametrize(
    "source, expected",
    [
        # Quoted list of symbols
        ("'(1 2 3)", [('quote', "'"), ('lparen', '('), ('symbol', '1'), ('symbol', '2'), ('symbol', '3'), ('rparen', ')')]),
        # Quasiquoted vector
        ("`#(a b)", [('quote', "`"), ('vector', '#('), ('symbol', 'a'), ('symbol', 'b'), ('rparen', ')')]),
        # Unquote with list
        (",@(1 2)", [('unquote', ",@"), ('lparen', '('), ('symbol', '1'), ('symbol', '2'), ('rparen', ')')]),
        # Nested quotes inside quasiquote
        ("`'(x y)", [('quote', "`"), ('quote', "'"), ('lparen', '('), ('symbol', 'x'), ('symbol', 'y'), ('rparen', ')')]),
        # Nested quasiquote/unquote inside vector
        ("`#(,x ,@y)", [('quote', "`"), ('vector', '#('), ('unquote', ','), ('symbol', 'x'), ('unquote', ',@'), ('symbol', 'y'), ('rparen', ')')]),
        # Vector containing sets and chars
        ("#(#{} #{} #\\a)", [('vector', '#('), ('set_macro', '#{'), ('rbrace', '}'),
                             ('set_macro', '#{'), ('rbrace', '}'), ('char', '#\\a'), ('rparen', ')')]),
        # Quoted empty set
        ("'#{ }", [('quote', "'"), ('set_macro', '#{'), ('rbrace', '}')]),
        # Nested vector inside set
        ("#{#(1 2) #(3 4)}", [('set_macro', '#{'), ('vector', '#('), ('symbol', '1'), ('symbol', '2'), ('rparen', ')'),
                               ('vector', '#('), ('symbol', '3'), ('symbol', '4'), ('rparen', ')'), ('rbrace', '}')]),
        # Deeply nested combo
        ("`#(1 'a ,b #{c #(d)} #\\newline)", [('quote', "`"), ('vector', '#('), ('symbol', '1'), ('quote', "'"), ('symbol', 'a'),
                                              ('unquote', ','), ('symbol', 'b'), ('set_macro', '#{'), ('symbol', 'c'), ('vector', '#('), ('symbol', 'd'), ('rparen', ')'), ('rbrace', '}'),
                                              ('char', '#\\newline'), ('rparen', ')')]),
    ]
)
def test_nested_reader_macros(source, expected):
    tokens = list(lex(source))
    assert tokens == expected
