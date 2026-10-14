source("FiniteDifferences.R")

newton_raphson <- function(f, x, tol = 1e-7, max_iter = 100, ...) {
  for (iter in 1:max_iter) {
    fx <- f(x, ...)
    if (sqrt(sum(fx^2)) < tol) {
      cat("Converged in", iter, "iterations\n")
      return(x)
    }
    fx_deriv <- finite_differences(f, 0.01, x, ...)
    x <- x - solve(fx_deriv, fx)
  }
  stop("Did not converge")
}
