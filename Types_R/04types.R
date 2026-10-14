if (!grepl("Types_R", getwd())) {
  setwd("./Types_R")
}
dyn.unload("04types.so")
system("R CMD SHLIB 04types.c")
dyn.load("04types.so")

.Call("add", 1, 2)
# .Call("add", 1L, 2) # fails as x has to be a real and not an integer
# .Call("add", 1, c(1, 2)) # Error: x and y length differ

# Everything is a SEXP
# ===============================================
.Internal(inspect(c(1, 2, 3))) # @... 14 REALSXP ... 1,2,3
.Internal(inspect(list(1, "a")))
# @... 19 VECSXP ...
#   @... 14 REALSXP ... 1
#   @... 16 STRSXP ...
#     @... 09 CHARSXP ... "a"
# removeSource drops the srcref attribute, which otherwise floods the output
.Internal(inspect(removeSource(function(x) x + 1)))
# @... 03 CLOSXP ...
# FORMALS: ... 02 LISTSXP
# BODY:    ... 06 LANGSXP
# CLOENV:  ... 04 ENVSXP <R_GlobalEnv>
.Internal(inspect(new.env())) # @... 04 ENVSXP ...

# Copy-on-modify? Not in C
# ===============================================
x <- c(1, 2, 3)
y <- x
.Internal(inspect(x)) # @5c18a7cf9108 14 REALSXP ... [REF(2)] 1,2,3
.Internal(inspect(y)) # @5c18a7cf9108 -> same address, shared
.Call("modify", x)
x # 0 1 2
y # 0 1 2 -> y changed too

# Functions are just data
# ===============================================
f <- function() 42
f() # 42
.Call("modify_body", f)
f() # 0
f # still shows 42 if the srcref is kept, the body is 0 though
body(f) # 0

# The PROTECT stack
# ===============================================
.Call("forgot_protect") # list(c(1, 1, 1), c(2, 2, 2)) -> looks fine, by luck
gctorture(TRUE) # gc on every allocation
r <- .Call("forgot_protect")
gctorture(FALSE)
r # list(c(2, 2, 2), c(2, 2, 2)) -> a was freed and its memory reused for b

.Call("protect_overflow", 1e5) # Error: protect(): protection stack overflow
.Call("forgot_unprotect") # Warning: stack imbalance in '.Call', 2 then 3

.Call("what_type_do_we_have", x)
.Call("what_type_do_we_have", 1L)
.Call("what_type_do_we_have", TRUE)
.Call("what_type_do_we_have", list())
.Call("what_type_do_we_have", function() {})
.Call("what_type_do_we_have", new.env())
.Call("what_type_do_we_have", quote(x <- 1))
.Call("what_type_do_we_have", quote(x))
.Call("what_type_do_we_have", NULL)
.Call("what_type_do_we_have", sum)

.Call("what_type_do_we_have_in_c", x)
.Call("what_type_do_we_have_in_c", 1L)
.Call("what_type_do_we_have_in_c", TRUE)
.Call("what_type_do_we_have_in_c", list())
.Call("what_type_do_we_have_in_c", function() {})
.Call("what_type_do_we_have_in_c", new.env())
.Call("what_type_do_we_have_in_c", quote(x <- 1))
.Call("what_type_do_we_have_in_c", quote(x))
.Call("what_type_do_we_have_in_c", NULL)
.Call("what_type_do_we_have_in_c", sum)
