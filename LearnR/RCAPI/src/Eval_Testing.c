#include "Eval_Testing.h"
// A simple function that evaluates an R expression passed as a string
SEXP c_test_eval(SEXP expr_str, SEXP env) {
    // Parse the string expression into an R expression
    SEXP expr, parsed_expr;
    ParseStatus status;

    PROTECT(expr = Rf_mkString(CHAR(STRING_ELT(expr_str, 0))));
    PROTECT(parsed_expr = R_ParseVector(expr, -1, &status, R_NilValue));

    if (status != PARSE_OK) {
        UNPROTECT(2);
        Rf_error("Parsing failed");
    }

    // Evaluate the parsed expression (first element)
    SEXP result;
    PROTECT(result = Rf_eval(VECTOR_ELT(parsed_expr, 0), env));

    UNPROTECT(3);
    return result;
}
