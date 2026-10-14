# Types_R: difficulty levels

## 1. Basics (done)
- atomic vectors, coercion
- matrices, arrays, recycling, attributes
- lists
- subsetting, preserved vs simplified, `drop`

## 2. Environments (done)
- walking the environment tree
- R6-like classes built from environments
- doubly linked list (`push_node`, `push_front`)

## 3. SEXP / C level (done)
- `.c` + `R CMD SHLIB` + `.Call`, `add`, `TYPEOF`
- everything is a SEXP: `.Internal(inspect())`
- bypassing copy-on-modify via `REAL(x)`
- modifying a function body in C
- PROTECT stack: `gctorture`, overflow, imbalance
