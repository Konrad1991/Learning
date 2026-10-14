#include <Rinternals.h>
#include "NAMED_Testing.h"
#include "Eval_Testing.h"

extern SEXP c_test_named(SEXP, SEXP, SEXP);

extern SEXP c_test_eval(SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"c_test_named", (DL_FUNC) &c_test_named, 3},
    {"c_test_eval", (DL_FUNC) &c_test_eval, 2},
    {NULL, NULL, 0}
};

void R_init_RCAPI(DllInfo* dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
