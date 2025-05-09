test_named <- function(a, b, c) {
  .Call(c_test_named, a, b, c)
}
test_eval <- function(a, b) {
  .Call(c_test_eval, a, b)
}
