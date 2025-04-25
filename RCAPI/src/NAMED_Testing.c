#include "NAMED_Testing.h"

SEXP c_test_named(SEXP a, SEXP b, SEXP c) {
  // Testing a SEXP object which is not bound to a variable
  SEXP unbound_object = PROTECT(Rf_allocVector(INTSXP, 3));
  int test = NAMED(unbound_object);
  Rprintf("NAMED if the SEXP object only exists within C = %d\n", test);

  // Define a SEXP object and assign it to a name (= my_named_object)
  // This is a variable definition at R level
  SEXP named_object = PROTECT(Rf_allocVector(INTSXP, 3));
  Rf_defineVar(Rf_install("my_named_object"), named_object, R_GlobalEnv);
  test = NAMED(named_object);
  Rprintf("NAMED if only one variable holds the SEXP object = %d\n", test);

  // Define a SEXP object and assign it to two variables
  SEXP named_2_object = PROTECT(Rf_allocVector(INTSXP, 3));
  Rf_defineVar(Rf_install("my_named_object"), named_2_object, R_GlobalEnv);
  Rf_defineVar(Rf_install("my_named_object2"), named_2_object, R_GlobalEnv);
  test = NAMED(named_2_object);
  Rprintf("NAMED if two variables holds the same SEXP object = %d\n", test);

  UNPROTECT(3);

  // Testing input
  test = NAMED(a);
  Rprintf("NAMED for a = %d\n", test);
  test = NAMED(b);
  Rprintf("NAMED for b = %d\n", test);
  test = NAMED(c);
  Rprintf("NAMED for c = %d\n", test);

  return R_NilValue;
}
