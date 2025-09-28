from zeta.types import Environment, MacroEnvironment
from zeta.builtins import register
from zeta.macro_builtins import register as register_macros
from zeta.interpreter import Interpreter

def main():
    env = Environment()
    register(env)
    macros = MacroEnvironment()
    register_macros(macros)

    prelude = '''
    ;; -----------------------
    ;; List utilities
    ;; -----------------------
    
    (defun tail (xs) (cdr xs))
        
    (defun head (xs) (car xs))
    
    (defun len (xs)
        (if (== xs Nil) 
            0 
            (+ 1 (len (tail xs)))
        ))
    
    (defun drop (n xs)
        (if (== n 0)
            xs
            (drop (- n 1) (tail xs))
        ))
    
    (defun foldl (f z xs)
        (if (== xs Nil)
            z
            (foldl f (f z (head xs)) (tail xs))
        ))
    
    (defun map (f xs)
        (if (== xs Nil)
            Nil
            (join (list (f (head xs))) (map f (tail xs)))
        ))
        
    
    (defun apply (f xs)
        (foldl (lambda (acc x) (f x)) Nil xs)
    )
    
    (defun filter (f xs)
        (if (== xs Nil)
            Nil
            (join (if (f (head xs)) (list (head xs)) Nil) (filter f (tail xs)))
        ))
    
    (defun is_empty (xs)
        (== xs Nil))
    
    (defun is_empty1 (xs)
        (== (len xs) 0))
    
    (defun sort (xs)
        (if (<= (len xs) 1)
            xs
            (let ((pivot (head xs)))
                (join
                    (sort (filter (lambda (n) (> pivot n)) xs))
                    (list pivot)
                    (sort (filter (lambda (n) (<= pivot n)) (tail xs)))
                )
            )
        )
    )
    '''

    interp = Interpreter(prelude=prelude)

    # Test expressions
    tests = [
        '(define xs (1 7 -3 4)) ;; -> Nil',
        '(is_empty xs) ;; -> #f',
        '(sort xs) ;; -> (-3 4 7)',
        '(filter (lambda (x) (> x 0)) xs) ;; -> (1 7 4)',
        '(+ (* 3 (+ (* 2 4) (+ 3 5))) (+ (- 10 7) 6))',
        '(defun foo (x &rest xs) (sort xs))',
        '(foo 1 7 -1 2)',
        "(apply 'foo '(1 7 -1 2))   ;; => (-1 2 7)",
        "(map (lambda (x) (* 2 x)) (1 2 3 4 5 6 7 8 9 10))",
        '''
        (defun bar (n)
            (let ((f (lambda (y x) (+ 1 x))))
            (f n n)))
        ''',
        '''
        (bar 4)
        '''
    ]

    for code in tests:
        result = interp.eval(code)
        print(code, "=>", result)


#  Example use-age:
if __name__ == "__main__":
  main()