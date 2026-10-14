library(ggplot2)
library(cowplot)

pso <- function(lb, ub, loss, ngen, npop, error_threshold,
                k = 3, num_gen_new_neighberhood = 1,
                global = FALSE, save_swarm = FALSE, add_info = NULL) {
  loss_fct <- function(params) {
    e <- try(loss(params, add_info))
    if (inherits(e, "try-error")) {
      stop("Could not evaluate the loss function")
    }
    return(e)
  }

  npar <- length(lb)
  swarm <- matrix(0, nrow = npop, ncol = npar)
  v <- matrix(0, nrow = npop, ncol = npar)
  swarm_bests <- numeric(npop)
  swarm_errors <- numeric(npop)
  initial_cog <- 2.5
  final_cog <- 0.5
  initial_soc <- 0.5
  final_soc <- 2.5
  w <- 0.5
  w_max <- 0.9
  w_min <- 0.4

  memory <- array(0, dim = c(ngen, npop, npar))
  error_memory <- array(0, dim = c(ngen, npop))
  best_parameters_memory <- array(0, dim = c(ngen, npar))

  if (any(lb < 0) || any(ub < 0)) {
    for (i in seq(npop)) {
      swarm[i, ] <- runif(npar, min = lb, max = ub)
      swarm_errors[i] <- loss_fct(swarm[i, ])
      swarm_bests[i] <- swarm_errors[i]
    }
  } else {
    lb <- ifelse(lb <= 0, 10^-15, lb)
    ub <- ifelse(ub <= 0, 10^-15, ub)
    lb <- log(lb)
    ub <- log(ub)

    for (i in seq(npop)) {
      swarm[i, ] <- runif(npar, min = lb, max = ub)
      swarm_errors[i] <- loss_fct(exp(swarm[i, ]))
      swarm_bests[i] <- swarm_errors[i]
    }

    swarm <- exp(swarm)
    lb <- exp(lb)
    ub <- exp(ub)
  }
  global_best <- which.min(swarm_bests)
  global_best_vec <- swarm[global_best, ]
  global_best_error <- swarm_bests[global_best]
  swarm_best_params <- swarm

  correct_below_lb <- function(target, threshold) {
    for (i in seq_len(length(target))) {
      if (target[i] < threshold[i]) target[i] <- threshold[i]
    }
    return(target)
  }

  correct_above_ub <- function(target, threshold) {
    for (i in seq_len(length(target))) {
      if (target[i] > threshold[i]) target[i] <- threshold[i]
    }
    return(target)
  }

  calc_neighberhood <- function() {
    neighberhood <- matrix(0L, ncol = k, nrow = npop)
    for (i in seq(npop)) {
      nneighbour <- sample(1:k, 1)
      neighbours <- sample(1:npop, nneighbour)
      if (length(neighbours) < k) {
        diff <- k - length(neighbours)
        neighbours <- c(neighbours, rep(NA, diff))
      }
      neighberhood[i, ] <- neighbours
    }
    neighberhood
  }
  neighberhood <- calc_neighberhood()
  convergence_check <- 0
  no_improvement <- 0

  if (save_swarm) {
    memory[1, 1:npop,  ] <- swarm
    error_memory[1, 1:npop] <- swarm_bests
    best_parameters_memory[1, ] <- global_best_vec
  }

  iter <- 1
  while (iter < ngen) {
    if (iter == 1 || convergence_check > num_gen_new_neighberhood) {
      neighberhood <- calc_neighberhood()
      convergence_check <- 0
    }

    if (save_swarm) {
      memory[iter + 1, 1:npop, ] <- swarm
    }

    w <- w_max - iter * (w_max - w_min) / ngen
    cog <- initial_cog - (initial_cog - final_cog) * (iter + 1) / ngen
    soc <- initial_soc - (initial_soc - final_soc) * (iter + 1) / ngen

    for (i in seq(npop)) {
      current_neighberhood <- neighberhood[i, ]
      current_neighberhood <- current_neighberhood[!is.na(current_neighberhood)]
      local_best <- which.min(swarm_bests[current_neighberhood])
      local_best <- current_neighberhood[local_best]
      local_best_vec <- swarm[local_best, ]

      if (global) {
        local_best_vec <- swarm[global_best, ]
      }

      v[i, ] <- w * v[i, ] +
        cog * runif(1) * (swarm_best_params[i, ] - swarm[i, ]) +
        soc * runif(1) * (local_best_vec - swarm[i, ])
      swarm[i, ] <- swarm[i, ] + v[i, ]

      swarm[i, ] <- correct_below_lb(swarm[i, ], lb)
      swarm[i, ] <- correct_above_ub(swarm[i, ], ub)

      error <- loss_fct(swarm[i, ])

      if (save_swarm) error_memory[iter + 1, 1:npop] <- error

      if (!is.infinite(error) && !is.na(error) &&
        error < swarm_bests[i]) {
        swarm_bests[i] <- error
        swarm_best_params[i, ] <- swarm[i, ]
      }

      if (!is.infinite(error) && !is.na(error) &&
        error < global_best_error) {
        global_best <- i
        global_best_vec <- swarm[i, ]
        global_best_error <- error
        no_improvement <- 0
      } else {
        no_improvement <- no_improvement + 1
      }
      convergence_check <- convergence_check + 1
      if (save_swarm) {
        best_parameters_memory[iter + 1, ] <- global_best_vec
      }
    }

    iter <- iter + 1

    print(iter)
    print(global_best_vec)
    print(global_best_error)

    if (global_best_error < error_threshold) {
      if (no_improvement > 1000) {
        break
      }
      break
    }
  }

  insilico <- loss_fct(global_best_vec)
  if (save_swarm) {
    return(structure(
      list(
        insilico = insilico,
        best = c(global_best_vec),
        swarm = memory,
        errors = error_memory,
        trace = best_parameters_memory
      ),
      class = "ParticleSwarmResults"
    ))
  } else {
    return(structure(
      list(
        insilico = insilico,
        best = c(global_best_vec)
      ),
      class = "ParticleSwarmResults"
    ))
  }
}

summary.ParticleSwarmResults <- function(object, ...) {
  stopifnot(inherits(object, "ParticleSwarmResults"))
  cat("=== Particle Swarm Optimization Result ===\n")
  cat("Final loss: ", format(object$insilico, digits = 6), "\n")
  cat("Best parameters:\n")
  print(round(object$best, 4))
}
print.ParticleSwarmResults <- function(object, ...) {
  summary(object, ...)
}
as.data.frame.ParticleSwarmResults <- function(x, ...) {
  stopifnot(inherits(x, "ParticleSwarmResults"))
  if (is.null(x$trace)) stop("Trace is not available (save = FALSE)")

  df <- as.data.frame(x$trace)
  colnames(df) <- paste0("Param_", seq_len(ncol(df)))
  df$Generation <- seq_len(nrow(df))
  df <- tidyr::pivot_longer(df, -Generation, names_to = "Parameter", values_to = "Value")
  return(df)
}

plot.ParticleSwarmResults <- function(result, title, ylimits) {
  stopifnot(inherits(result, "ParticleSwarmResults"))
  if (is.null(result$trace)) stop("You have to set save to TRUE in order to call plot")
  stopifnot(is.numeric(ylimits))
  stopifnot(length(ylimits) == 2)
  stopifnot(ylimits[2] > ylimits[1])
  parameter_saved <- result$swarm
  best_vec <- result$trace
  num_gen <- dim(best_vec)[1]

  indices <- seq(1, num_gen, 100)
  parameter_saved <- parameter_saved[indices, , ]
  best_vec <- best_vec[indices, ]

  npop <- dim(parameter_saved)[2]
  npar <- dim(parameter_saved)[3]

  swarm_df <- do.call(rbind, lapply(seq_along(indices), function(i) {
    df <- expand.grid(
      generation = indices[i],
      particle = seq_len(npop),
      param = paste0("Param ", seq_len(npar))
    )
    df$value <- parameter_saved[i, , ] |> as.vector()
    df
  }))

  best_df <- as.data.frame(best_vec)
  colnames(best_df) <- paste0("Param ", seq_len(npar))
  best_df$generation <- indices
  best_df <- tidyr::pivot_longer(best_df, -generation, names_to = "param", values_to = "value")

  ggplot(swarm_df, aes(x = generation, y = value)) +
    geom_point(alpha = 0.5, size = 1) +
    geom_line(data = best_df, aes(x = generation, y = value, color = "Best"), linewidth = 1) +
    facet_wrap(~ param, scales = "free_y") +
    scale_color_manual(values = c("Best" = "darkred")) +
    theme_minimal() +
    scale_y_continuous(limits = ylimits) +
    labs(title = title,
         x = "Generation", y = "Parameter Value", color = "")
}

offset_rastrigin <- function(x, offset = 5) {
  x <- x - offset
  10 * length(x) + sum(x^2 - 10 * cos(2 * pi * x))
}
loss <- offset_rastrigin
offset <- 10

set.seed(1234)
res_optim <- optim(runif(3), offset_rastrigin, method = "L-BFGS-B",
  lower = -10^3, upper = 10^3, control = list(maxit = 10^9)
)
res_optim
optim(runif(3, -100, 100), offset_rastrigin)

set.seed(1234)
res_arbitrary_neighberhood <- pso(
  lb = rep(-10^3, 3), ub = rep(10^3, 3), loss, npop = 40, ngen = 20000,
  error_threshold = -Inf, k = 10, num_gen_new_neighberhood = 5000,
  global = FALSE, save = TRUE, add_info = offset
)
summary(res_arbitrary_neighberhood)
print(res_arbitrary_neighberhood)
p_arbitrary <- plot(res_arbitrary_neighberhood, "Topology: Arbitrary random neighberhood", c(-100, 100))

set.seed(1234)
res_star_topology <- pso(
  lb = rep(-10^3, 3), ub = rep(10^3, 3), loss, npop = 40, ngen = 20000,
  # With more generations (50000) the best solution is found for all parameters. Here only for the first and second one
  error_threshold = -Inf, global = TRUE, save = TRUE, add_info = offset
)
summary(res_star_topology)
print(res_star_topology)
p_global <- plot(res_star_topology, "Topology: Star", c(-100, 100))
plot_grid(p_arbitrary, p_global, nrow = 2)
