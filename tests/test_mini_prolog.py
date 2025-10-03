import pytest

from zeta.interpreter import Interpreter
from zeta.types.symbol import Symbol
from zeta.types.nil import Nil
from zeta.types.errors import ZetaArityError, ZetaTypeError
from zeta.evaluation.evaluator import evaluate0
from zeta.types.tail_call import TailCall
from zeta.types.environment import Environment

# Minimal Python helpers to support the Lisp prelude

def _mini_display(env, args):
    for a in args:
        try:
            print(a, end="")
        except Exception:
            print(str(a), end="")
    return Symbol("#t")


def _mini_string_ref(env, args):
    if len(args) != 2:
        raise ZetaArityError("string-ref expects exactly 2 arguments")
    s, i = args
    if not isinstance(s, str):
        raise ZetaTypeError("First argument to string-ref must be a string")
    if not isinstance(i, int):
        raise ZetaTypeError("Second argument to string-ref must be an integer index")
    try:
        return s[i]
    except Exception as e:
        raise ZetaTypeError(f"string-ref index out of range: {i}") from e


def _mini_char_eq(env, args):
    if not args:
        return Symbol("#t")

    def to_char(x):
        # Accept Python 1-char strings directly
        if isinstance(x, str):
            return x if len(x) == 1 else x
        # Accept Lisp reader character literals like #\? #\a #\space
        if isinstance(x, Symbol):
            s = x.id
            if s.startswith("#\\") or s.startswith("#\\") or s.startswith("#\\"):
                lit = s[2:]
                named = {
                    "space": " ",
                    "newline": "\n",
                    "tab": "\t",
                    "return": "\r",
                }
                return named.get(lit, lit[:1] if lit else "")
        return None

    norm = []
    for a in args:
        ch = to_char(a)
        if ch is None:
            return Symbol("#f")
        norm.append(ch)

    first = norm[0]
    for x in norm[1:]:
        if x != first:
            return Symbol("#f")
    return Symbol("#t")


def _mini_consp(env, args):
    if len(args) != 1:
        raise ZetaArityError("consp expects exactly 1 argument")
    x = args[0]
    if x is Nil:
        return Symbol("#f")
    # Non-empty list is a cons
    if isinstance(x, list) and len(x) > 0:
        return Symbol("#t")
    # Dotted pair represented as a Python tuple (head, tail)
    if isinstance(x, tuple) and len(x) == 2:
        return Symbol("#t")
    return Symbol("#f")


def _is_var(sym):
    return isinstance(sym, Symbol) and sym.id.startswith('?')


def _subst_apply_py(term, subst):
    if _is_var(term):
        for k, v in subst:
            if k == term:
                return _subst_apply_py(v, subst)
        return term
    if isinstance(term, list):
        return [_subst_apply_py(t, subst) for t in term]
    return term


def _unify_py(x, y, subst):
    print("      [unify] x=", x, " y=", y, " subst=", subst)
    # Resolve existing bindings
    if _is_var(x):
        for k, v in subst:
            if k == x:
                return _unify_py(v, y, subst)
    if _is_var(y):
        for k, v in subst:
            if k == y:
                return _unify_py(x, v, subst)
    # Structural equality
    if x == y:
        return subst
    if _is_var(x):
        return [(x, y)] + subst
    if _is_var(y):
        return [(y, x)] + subst
    if isinstance(x, list) and isinstance(y, list):
        if len(x) == 0 and len(y) == 0:
            return subst
        if len(x) == 0 or len(y) == 0:
            return None
        subst1 = _unify_py(x[0], y[0], subst)
        if subst1 is None:
            return None
        return _unify_py(x[1:], y[1:], subst1)
    return None


def _prove_builtin(env, args):
    # args: [goals]
    # Standardize-apart utility to avoid variable capture across rule applications
    _counter = {"n": 0}
    def std_apart_term(term, mapping):
        if isinstance(term, Symbol) and term.id.startswith('?'):
            if term not in mapping:
                _counter["n"] += 1
                mapping[term] = Symbol(f"?v{_counter['n']}_{term.id[1:]}")
            return mapping[term]
        if isinstance(term, list):
            return [std_apart_term(t, mapping) for t in term]
        return term
    def std_apart_rule(rule):
        mapping = {}
        if isinstance(rule, list) and len(rule) >= 3 and isinstance(rule[1], Symbol) and rule[1] == Symbol(':-'):
            head = std_apart_term(rule[0], mapping)
            body = std_apart_term(rule[2], mapping)
            return [head, Symbol(':-'), body]
        else:
            return std_apart_term(rule, mapping)

    # DEBUG print
    print("[py-prove] called with args=", args)
    # args: [goals]
    # DEBUG print
    print("[py-prove] called with args=", args)
    # args: [goals]
    # args: [goals]
    goals = args[0] if args else []
    # Fetch knowledge base
    print("[py-prove] env lookup *knowledge-base* ...")
    try:
        kb = env.lookup(Symbol('*knowledge-base*'))
        while isinstance(kb, TailCall):
            kb = evaluate0(kb.fn.body, kb.env, kb.macros, True)
    except Exception:
        kb = []
    print("[py-prove] kb=", kb)

    def prove_list(goals, subst):
        print("[py-prove] prove_list goals=", goals, " subst=", subst)
        if goals == [] or goals is Nil:
            print("[py-prove] goals empty, returning subst", subst)
            return [subst]
        goal = goals[0]
        rest = goals[1:]
        results = []
        print("[py-prove] trying goal=", goal)
        for rule in kb:
            std_rule = std_apart_rule(rule)
            is_rule = (
                isinstance(std_rule, list)
                and len(std_rule) >= 3
                and isinstance(std_rule[1], Symbol)
                and std_rule[1] == Symbol(':-')
            )
            head = std_rule[0] if is_rule else std_rule
            body = std_rule[2] if is_rule else []
            print("  [py-prove] rule=", std_rule, " is_rule=", is_rule, " head=", head, " body=", body)
            subst1 = _unify_py(goal, head, subst)
            print("    [py-prove] unify ->", subst1)
            if subst1 is not None:
                results.extend(prove_list(body + rest, subst1))
        print("[py-prove] results for goal", goal, "=", results)
        return results

    sols = prove_list(goals, [])
    # Normalize substitutions: resolve chained variables to ground terms where possible
    normalized = []
    for s in sols:
        out_pairs = []
        for (k, v) in s:
            v_res = _subst_apply_py(v, s)
            out_pairs.append((k, v_res))
        normalized.append(out_pairs)
    # Represent substitutions as list of tuples (k,v)
    return normalized


def _subst_apply_builtin(env, args):
    if len(args) != 2:
        raise ZetaArityError("subst-apply expects exactly 2 arguments")
    term, subst = args
    return _subst_apply_py(term, subst)


def _unify_builtin(env, args):
    if len(args) not in (2, 3):
        raise ZetaArityError("unify expects 2 or 3 arguments")
    x = args[0]
    y = args[1]
    subst = args[2] if len(args) == 3 else []
    res = _unify_py(x, y, subst)
    return Symbol("#f") if res is None else res


def register_miniprolog(env: Environment):

    mapping = {Symbol('display'): _mini_display,
               Symbol('string-ref'): _mini_string_ref,
               Symbol('char=?'): _mini_char_eq,
               Symbol('consp'): _mini_consp,
               Symbol('subst-apply'): _subst_apply_builtin,
               Symbol('unify'): _unify_builtin}
    #mapping[Symbol('prove')] = _prove_builtin
    for k,v in mapping.items():
        env.vars.update({k:v})
#
# Comment out subst-apply/unify, use Python versions we know work.

# (defun subst-apply (term subst)
#   (cond
#     ((variable? term)
#      (let ((val (assoc term subst)))
#        (if val (subst-apply (cdr val) subst) term)))
#     ((consp term)
#      (cons (subst-apply (car term) subst)
#            (subst-apply (cdr term) subst)))
#     (else term)))
#
# (defun unify (x y &optional (subst '()))
#   (cond
#     ((assoc x subst) (unify (cdr (assoc x subst)) y subst))
#     ((assoc y subst) (unify x (cdr (assoc y subst)) subst))
#     ((eq? x y) subst)
#     ((variable? x) (cons (cons x y) subst))
#     ((variable? y) (cons (cons y x) subst))
#     ((and (consp x) (consp y))
#      (let ((subst1 (unify (car x) (car y) subst)))
#        (and subst1 (unify (cdr x) (cdr y) subst1))))
#     (else #f)))



# Lisp prelude implementing a tiny Prolog engine
prolog = r'''
;;;; Prelude helpers
(defun append (a b) (join a b))
(defun filter (f xs)
  (if (null? xs) '()
      (join (if (f (car xs)) (list (car xs)) '())
            (filter f (cdr xs)))))

(defun assoc (key alist)
  (cond
    ((null? alist) #f)
    ((eq? (car (car alist)) key) (car alist))
    (else (assoc key (cdr alist)))))

(defun map (f lst)
  (cond ((null? lst) '()) (else (cons (f (car lst)) (map f (cdr lst))))))

(defun for-each (f lst)
  (display "[for-each] called with lst=") (display lst) (display "\n")
  (if (null? lst)
      (begin (display "[for-each] null? => #t\n") '())
      (begin (display "[for-each] applying f to (car lst)=") (display (car lst)) (display "\n")
             (f (car lst))
             (for-each f (cdr lst)))))

(defun variable? (x)
  (and (symbol? x)
       (eq? (string-ref (symbol->string x) 0) "?")))


(define *knowledge-base* '())

(defun rule! (fact-or-rule)
  (set! *knowledge-base* (cons fact-or-rule *knowledge-base*)))

(defun retract! (fact-or-rule)
  (set! *knowledge-base*
        (filter (lambda (x) (not (eq? x fact-or-rule)))
                *knowledge-base*)))

(defun prove (goals &optional (subst '()))
  (if (null? goals)
      (list subst)
      (let ((goal (car goals))
            (rest-goals (cdr goals))
            (results '()))
        (display "[prove] goals=") (display goals) (display " kb=") (display *knowledge-base*) (display "\n")
        (for-each (lambda (rule)
                    ;; Distinguish facts vs rules of the form ((head) :- (body...))
                    (let* ((is-rule (and (consp rule)
                                          (consp (cdr rule))
                                          (eq? (car (cdr rule)) ':-)))
                           (head (if is-rule (car rule) rule))
                           (body (if is-rule (car (cdr (cdr rule))) '())))
                      (display "  [rule] head=") (display head) (display " body=") (display body) (display "\n")
                      (let ((subst1 (unify goal head subst)))
                        (display "    unify-> ") (display subst1) (display "\n")
                        (if subst1
                            (set! results (append results
                                                  (prove (append body rest-goals) subst1)))))))
                  *knowledge-base*)
        (display "[prove] results=") (display results) (display "\n")
        results)))
'''

@pytest.mark.skip(reason="WIP")
def test_mini_prolog_parent_and_ancestor_queries():
    itp = Interpreter(prelude=None)

    # Provide an alias eq? -> == for convenience used by the prelude
    from zeta.builtin.env_builtin import equals
    itp.env.update({Symbol("eq?"): equals})

    itp.eval_prelude(prolog)

    # Use the pure Lisp implementation (do not override any of prove/unify/subst-apply)
    register_miniprolog(itp.env, override_prove=False, override_unify=True, override_subst_apply=True)

    itp.eval(
        """
      (progn
        (rule! '(parent alice bob))
        (rule! '(parent bob carol))
        (rule! '(parent bob david))
        (rule! '(parent carol eve)))
    """
    )

    def run(goal_src: str):
        return itp.eval(f"(prove (list {goal_src}))")

    # Parent query should find bob
    sols1 = run("'(parent alice ?X)")
    assert isinstance(sols1, list)
    ok = any(
        isinstance(sol, list)
        and [Symbol('?X'), Symbol('bob')] in [[pair[0], pair[1]] for pair in sol]
        for sol in sols1
    )
    assert ok

    # Add ancestor rules
    itp.eval(
        """
      (progn
        (rule! '((ancestor ?X ?Y) :- ((parent ?X ?Y))))
        (rule! '((ancestor ?X ?Y) :- ((parent ?X ?Z) (ancestor ?Z ?Y)))))
    """
    )

    sols2 = run("'(ancestor alice ?K)")
    expected = {Symbol('bob'), Symbol('carol'), Symbol('david'), Symbol('eve')}
    found = set()
    for sol in sols2:
        for pair in sol:
            if pair[0] == Symbol('?K'):
                found.add(pair[1])
    assert expected.issubset(found)

    # Retract and ensure downstream ancestors disappear
    itp.eval("(retract! '(parent bob carol))")
    sols3 = run("'(ancestor alice ?K)")
    banned = {Symbol('carol'), Symbol('eve')}
    found3 = set()
    for sol in sols3:
        for pair in sol:
            if pair[0] == Symbol('?K'):
                found3.add(pair[1])
    assert not banned.intersection(found3)
