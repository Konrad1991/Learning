
#ifndef TESTING_EVAL_H
#define TESTING_EVAL_H

#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <stdbool.h>
#include <R_ext/Parse.h>

SEXP c_test_eval(SEXP expr_str, SEXP env);

#endif
