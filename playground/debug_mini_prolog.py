from zeta.interpreter import Interpreter
from zeta.types.symbol import Symbol
from tests.test_mini_prolog import register_miniprolog, prolog


def scenario(override_subst_apply=False, override_unify=False, override_prove=False):
    itp = Interpreter(prelude=None)
    from zeta.builtin.env_builtin import equals
    itp.env.update({Symbol("eq?"): equals})
    itp.eval_prelude(prolog)
    register_miniprolog(
        itp.env,
        override_prove=override_prove,
        override_unify=override_unify,
        override_subst_apply=override_subst_apply,
    )
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
    sols1 = run("'(parent alice ?X)")
    ok = any(
        isinstance(sol, list)
        and [Symbol('?X'), Symbol('bob')] in [[pair[0], pair[1]] for pair in sol]
        for sol in sols1
    )
    return ok

if __name__ == "__main__":
    print("Pure Lisp:", scenario())
    print("Override subst-apply:", scenario(override_subst_apply=True))
    print("Override unify:", scenario(override_unify=True))
    print("Override both:", scenario(override_subst_apply=True, override_unify=True))
    print("Override prove:", scenario(override_prove=True))
