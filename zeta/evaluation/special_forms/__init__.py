"""Registry of special forms for the Zeta evaluator.

Maps Symbols to handler functions that implement non-standard evaluation rules.
The evaluator consults this table to dispatch special forms before ordinary
function application.
"""

from zeta.types.symbol import Symbol
from zeta.evaluation.special_forms.set_form import set_form
from zeta.evaluation.special_forms.progn_form import progn_form
from zeta.evaluation.special_forms.eval_form import eval_form
from zeta.evaluation.special_forms.defmacro_form import defmacro_form
from zeta.evaluation.special_forms.defstruct_form import defstruct_form
from zeta.evaluation.special_forms.import_form import import_form
from zeta.evaluation.special_forms.quote_forms import quote_form, quasiquote_form, unquote_form, unquote_splice_form
from zeta.evaluation.special_forms.lambda_form import lambda_form
from zeta.evaluation.special_forms.define_form import define_form
from zeta.evaluation.special_forms.if_form import if_form
from zeta.evaluation.special_forms.do_loop_forms import do_loop_form, do_list_loop_form, do_times_n_loop_form
from zeta.evaluation.special_forms.condition_case_form import condition_case_form
from zeta.evaluation.special_forms.throw_catch_form import throw_form, catch_form
from zeta.evaluation.special_forms.apply_form import apply_form

SPECIAL_FORMS = {
    Symbol("set"): set_form,
    Symbol("progn"): progn_form,
    Symbol("begin"): progn_form,
    Symbol("eval"): eval_form,
    Symbol("defmacro"): defmacro_form,
    Symbol("defstruct"): defstruct_form,
    Symbol("import"): import_form,
    Symbol("quote"): quote_form,
    Symbol("quasiquote"): quasiquote_form,
    Symbol("unquote"): unquote_form,
    Symbol("unquote-splicing"): unquote_splice_form,
    Symbol("lambda"): lambda_form,
    Symbol("define"): define_form,
    Symbol("if"): if_form,
    Symbol("do"): do_loop_form,
    Symbol("dotimes"): do_times_n_loop_form,
    Symbol("dolist"): do_list_loop_form,
    Symbol("condition-case"): condition_case_form,
    Symbol("cond"): condition_case_form,
    Symbol("throw"): throw_form,
    Symbol("catch"): catch_form,
    Symbol("apply"): apply_form,
}