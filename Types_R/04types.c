#include <R.h>
#include <Rinternals.h>

SEXP add(SEXP x, SEXP y) {
  double* x_ptr = REAL(x); // get the raw pointer of x
  double* y_ptr = REAL(y);

  const R_xlen_t x_len = Rf_xlength(x);
  const R_xlen_t y_len = Rf_xlength(y);
  if (x_len != y_len) {
    Rf_error("x and y length differ");
  }
  SEXP result = PROTECT(Rf_allocVector(REALSXP, y_len));
  double* result_ptr = REAL(result);
  for (R_xlen_t i = 0; i < x_len; ++i) {
    result_ptr[i] = x_ptr[i] + y_ptr[i];
  }
  UNPROTECT(1);
  return result;
}

SEXP modify(SEXP x) {
  double* x_ptr = REAL(x);
  const R_xlen_t x_len = Rf_xlength(x);
  for (R_xlen_t i = 0; i < x_len; ++i) {
    x_ptr[i] = (double)(i);
  }
  return R_NilValue;
}

SEXP what_type_do_we_have(SEXP obj) {
  return Rf_mkString(Rf_type2char(TYPEOF(obj)));
}

typedef enum {
  OWN_NILSXP = 0,
  OWN_SYMSXP = 1,
  OWN_CLOSXP = 3,
  OWN_ENVSXP = 4,
  OWN_LANGSXP = 6,
  OWN_SPECIALSXP = 7,
  OWN_BUILTINSXP = 8,
  OWN_LGLSXP = 10,
  OWN_INTSXP = 13,
  OWN_REALSXP = 14,
  OWN_STRSXP = 16,
  OWN_VECSXP = 19
} Types;

SEXP what_type_do_we_have_in_c(SEXP obj) {
  int temp = TYPEOF(obj);
  switch (temp) {
    case OWN_NILSXP: Rprintf("NULL\n"); break;
    case OWN_SYMSXP: Rprintf("Symbol\n"); break;
    case OWN_CLOSXP: Rprintf("Function\n"); break;
    case OWN_ENVSXP: Rprintf("environment\n"); break;
    case OWN_LANGSXP: Rprintf("language\n"); break;
    case OWN_SPECIALSXP: Rprintf("special\n"); break;
    case OWN_BUILTINSXP: Rprintf("builtin\n"); break;
    case OWN_LGLSXP: Rprintf("logicals\n"); break;
    case OWN_INTSXP: Rprintf("ints\n"); break;
    case OWN_REALSXP: Rprintf("numeric\n"); break;
    case OWN_STRSXP: Rprintf("character\n"); break;
    case OWN_VECSXP: Rprintf("list at c level they are called vector\n"); break;
    default: printf("Whatever\n"); break;
  }
  return R_NilValue;
}

SEXP forgot_protect(void) {
  SEXP a = Rf_allocVector(REALSXP, 3); // no PROTECT
  for (int i = 0; i < 3; ++i) REAL(a)[i] = 1.0;
  // this allocation may run the gc, which frees a
  SEXP b = Rf_allocVector(REALSXP, 3);
  for (int i = 0; i < 3; ++i) REAL(b)[i] = 2.0;
  SEXP result = PROTECT(Rf_allocVector(VECSXP, 2));
  SET_VECTOR_ELT(result, 0, a);
  SET_VECTOR_ELT(result, 1, b);
  UNPROTECT(1);
  return result;
}

SEXP protect_overflow(SEXP n) {
  const int count = Rf_asInteger(n);
  for (int i = 0; i < count; ++i) {
    PROTECT(Rf_ScalarInteger(i));
  }
  UNPROTECT(count);
  return R_NilValue;
}

SEXP forgot_unprotect(void) {
  return PROTECT(Rf_ScalarInteger(1));
}

// R_ClosureBody instead of BODY: BODY is non-API since R 4.5
SEXP modify_body(SEXP f) {
  SEXP body = R_ClosureBody(f);
  REAL(body)[0] = 0.0;
  return R_NilValue;
}
