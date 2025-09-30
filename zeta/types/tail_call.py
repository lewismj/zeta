from zeta import SExpression
from zeta.types.lambda_fn import Lambda
from zeta.types.environment import Environment
from zeta.types.macro_environment import MacroEnvironment

class TailCall:
    def __init__(self, fn: Lambda, args: SExpression, env: Environment, macros: MacroEnvironment):
        self.fn = fn
        self.args = args
        self.env = env
        self.macros = macros