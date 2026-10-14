# Define the ODE function
ode <- function(y, t) {
  -2 * y
}

# Initial condition and time span
yinit <- 1
tstart <- 0
tend <- 2
step_sizes <- c(0.001, 0.005)

# Solve the ODE
source("ExplicitEuler.R")
solution <- explicit_euler(ode, yinit, tstart, tend)

# Solve the ODE
source("ImplicitEuler.R")
solution <- implicit_euler(ode, yinit, tstart, tend, 0.1)

stop("bla")

results <- data.frame()

for (h in step_sizes) {
  solution_implicit <- implicit_euler(ode, yinit, tstart, tend, h)
  solution_explicit <- explicit_euler(ode, yinit, tstart, tend, h)

  df_implicit <- data.frame(time = solution_implicit[, 1], value = solution_implicit[, 2], method = "Implicit Euler", step_size = h)
  df_explicit <- data.frame(time = solution_explicit[, 1], value = solution_explicit[, 2], method = "Explicit Euler", step_size = h)

  results <- rbind(results, df_implicit, df_explicit)
}


# Exact solution for comparison
time_exact <- seq(tstart, tend, length.out = 1000)
exact_solution <- data.frame(time = time_exact, value = exp(-2 * time_exact), method = "Exact", step_size = NA)

# Combine exact solution with results
results <- rbind(results, exact_solution)

library(ggplot2)
# Plot the stability of the solvers
ggplot(results, aes(x = time, y = value, color = method, linetype = as.factor(step_size))) +
  geom_line() +
  facet_wrap(~step_size, scales = "free_y") +
  labs(
    title = "Stability of Explicit and Implicit Euler Methods",
    x = "Time t",
    y = "y",
    color = "Method",
    linetype = "Step Size"
  ) +
  theme_minimal()
