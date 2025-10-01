import traceback
from zeta.interpreter import Interpreter

# Use a single interpreter; no separate env/macros needed
interp = Interpreter(prelude='''
                (defun map (f xs)
                (if (== xs Nil)
                    Nil
                    (join (list (f (head xs))) (map f (tail xs)))
                ))''')

a_code = '''
(defmacro let-bindings-variant (bindings &rest body)
  `(let ,bindings ,@body))

(let-bindings-variant ((a 1) (b 2)) (list a b))
'''

b_code = '''
(progn
(defmacro unless (cond body) `(if (not ,cond) ,body))
(define x 10)
(define y 20)
(unless (> x y) (define z 100))
z
)
'''

def main():
    try:
        print('Code:\n', b_code)
        res = interp.eval(b_code)
        print('Eval result:', res)
        # Parse and macro-expand the second form to inspect shape
        # from zeta.reader.parser import lex, TokenStream
        # toks = TokenStream(lex("(let-bindings-variant ((a 1) (b 2)) (list a b))"))
        # exprs = list(toks.parse_all())
        # print('Parsed expr:', exprs[0])
        # from zeta.evaluation.evaluator import evaluate0
        # expanded_head = interp.macros.expand_1(exprs[0], evaluate0, interp.env)
        # print('Expanded head:', expanded_head)
        # expanded_all = interp.macros.macro_expand_all(exprs[0], evaluate0, interp.env)
        # print('Macro-expanded:', expanded_all)
        # print('Final eval:', interp.eval("(let-bindings-variant ((a 1) (b 2)) (list a b))"))
    except Exception as e:
        print('Exception:', e)
        traceback.print_exc()

if __name__ == '__main__':
    main()