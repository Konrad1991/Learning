finite_differences <- function(g, ode, x, xnew, h, tcurrent) {
  fx <- g(x, xnew, ode, h, tcurrent)
  jac <- matrix(0, length(fx), length(xnew))
  stopifnot(length(fx) == length(xnew))
  for (i in seq_along(xnew)) {
    xh <- xnew
    xh[i] <- xh[i] + h
    jac[i, ] <- (g(x, xh, ode, h, tcurrent) - fx) / h
  }
  return(jac)
}

newton_raphson <- function(g, ode,
                           x, xnew, h = 0.1,
                           tol = 1e-7,
                           max_iter = 100, tcurrent) {
  for (iter in 1:max_iter) {
    fx <- g(x, xnew, ode, h, tcurrent)
    if (sqrt(sum(fx^2)) < tol) {
      return(xnew)
    }
    fx_deriv <- finite_differences(g, ode, x, xnew, h, tcurrent)
    xnew <- xnew - solve(fx_deriv, fx)
  }
  stop("Did not converge")
}

implicit_euler <- function(ode, yinit, tstart, tend, h = 0.0001) {
  tcurrent <- tstart
  ycurrent <- yinit
  yres <- c(tstart, yinit)

  g <- function(ycurrent, ynew, ode, h, tcurrent) {
    ynew - h * ode(ynew, tcurrent) - ycurrent
  }

  while (tcurrent < tend) {
    ynew_guess <- ycurrent + h * ode(ycurrent, tcurrent)
    ynew <- newton_raphson(g, ode, ycurrent, ynew_guess, h = h, tol = 0.1, max_iter = 100, tcurrent)
    tcurrent <- tcurrent + h
    yres <- rbind(yres, c(tcurrent, ynew))
  }
  return(yres)
}



ode <- function(y, t) {
  -1000 * (y - cos(t))
}

# Initial condition and time span
yinit <- 0
tstart <- 0
tend <- 2
step_sizes <- c(0.001, 0.005)

# Solve the ODE
solution <- implicit_euler(ode, yinit, tstart, tend, 0.1)

print(head(solution))
plot(solution[, 1], solution[, 2], type = "l", main = "ImplicitEuler")

# Solve the ODE
source("ExplicitEuler.R")
solution <- explicit_euler(ode, yinit, tstart, tend, 0.1)

print(head(solution))
plot(solution[, 1], solution[, 2], type = "l", main = "ExplicitEuler")

# Define the stiff ODE function
ode <- function(t, y, params) {
  dydt <- -1000 * (y - cos(t))
  return(list(dydt))
}
yinit <- 0
tspan <- seq(0, 2, by = 0.01)
solution <- deSolve::ode(yinit, tspan, ode)
print(head(solution))
# Plot the solution
plot(solution, type = "l", col = "blue", xlab = "Time t", ylab = "y", main = "Stiff ODE Solution")
