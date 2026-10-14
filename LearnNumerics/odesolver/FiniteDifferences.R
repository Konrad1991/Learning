finite_differences <- function(f, h, x, ...) {
  fx <- f(x, ...)
  jac <- matrix(0, length(fx), length(x))
  stopifnot(length(fx) == length(x))
  for (i in seq_along(x)) {
    xh <- x
    xh[i] <- xh[i] + h
    jac[i, ] <- (f(xh, ...) - fx) / h
  }
  return(jac)
}
