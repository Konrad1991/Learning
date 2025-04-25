# NAMED | Meaning
# 0 | No names (temporary).
# 1 | One name (modifiable).
# 2 | Multiple names (copy-on-modify).
# 3 | Function argument (special case—acts like ≥2).
# 4 | NAMEDMAX (shared constants, literals—never modify).
# 5 | Shared function argument: same object passed to multiple arguments.
# 6 | Shared function argument + multiple names in env (extra protection).
devtools::load_all("~/Documents/Learning/RCAPI/")

a <- 1
shared <- 2
c <- shared
RCAPI:::test_named(a, 1, 2)
RCAPI:::test_named(a, shared, shared)
