source("FiniteDifferences.R")
source("NewtonSolver.R")

f <- function(x) {
  c(
    x[1]^2 + x[2]^2 - 4,
    x[1] - x[2] - 1
  )
}
x_init <- c(1, 1)

solution <- newton_raphson(f, x_init)
print(solution)

# Using rootSolve::multiroot
library(rootSolve)
result_multiroot <- multiroot(f, x_init)
print(result_multiroot$root)
