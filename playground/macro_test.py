from zeta.interpreter import Interpreter


def main():
    # Use a single interpreter instance with empty prelude
    interp = Interpreter(prelude='')

    # Evaluate macro definition and usage in one go to ensure shared state
    code = (
        "(defmacro let-bindings-variant (bindings &rest body) `(let ,bindings ,@body))\n"
        "(let-bindings-variant ((a 1) (b 2)) (list a b))"
    )

    try:
        print("Code:\n", code)
        result = interp.eval(code)
        print("Result:", result)
    except Exception as ex:
        print("Error:", ex)


# Example usage
if __name__ == "__main__":
    main()