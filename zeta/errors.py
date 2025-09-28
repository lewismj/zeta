
class ZetaError(Exception):
    """ Base class for all Zeta errors"""
    pass

class ZetaInvalidSymbol(ZetaError):
    """ Raised when an invalid symbol is used"""
    pass

class ZetaUnboundSymbol(ZetaError):
    """ Raised when a symbol is used before it is bound"""
    pass

class ZetaNameError(ZetaError):
    """ Raised when a name is used before it is defined"""

class ZetaSyntaxError(ZetaError):
    """ Raised when there is a syntax error"""

class ZetaArityError(ZetaError):
    """ Raised when the number of arguments passed to a function is incorrect"""

class ZetaTypeError(ZetaError):
    """ Raised when the types of arguments passed to a function are incorrect"""

# Should be:
# class LispError(Exception):
#     """Base class for all Lisp-level errors."""
#     def __init__(self, tag: str, value: Any = None):
#         super().__init__(f"LispError(tag={tag}, value={value})")
#         self.tag = tag
#         self.value = value