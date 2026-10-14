gradient_descent <- function(f, x0,
                             lr = 0.01, tol = 1e-6,
                             max_iter = 1000,
                             h = 1e-5, ...) {
  backtrack <- function(f, x, grad, fx, lr, beta = 0.5) {
    while (TRUE) {
      x_new <- x - lr * grad
      if (f(x_new) < fx) break
      lr <- lr * beta
      if (lr < 1e-12) break
    }
    list(x_new = x_new, lr = lr)
  }
  x <- x0
  for (i in 1:max_iter) {
    fx <- f(x, ...)
    grad <- numeric(length(x))
    for (j in seq_along(x)) {
      xh <- x
      xh[j] <- xh[j] + h
      grad[j] <- (f(xh, ...) - fx) / h
    }

    temp <- backtrack(f, x, grad, fx, lr)
    x_new <- temp$x_new
    lr <- temp$lr
    # x_new <- x - lr * grad

    if (sqrt(sum((x_new - x)^2)) < tol) {
      message("Converged in ", i, " iterations")
      return(x_new)
    }

    x <- x_new
  }

  stop("Did not converge")
}

gradient_descent_momentum <- function(f, x0, lr = 0.1, beta = 0.9, tol = 1e-6, max_iter = 1000, h = 1e-5) {
  backtrack <- function(f, x, v, fx, lr, beta_bt = 0.5) {
    while (TRUE) {
      x_new <- x + v
      if (f(x_new) < fx) break
      lr <- lr * beta_bt
      if (lr < 1e-12) break
      v <- v * beta_bt
    }
    list(x_new = x_new, lr = lr)
  }

  x <- x0
  v <- numeric(length(x))
  
  for (i in 1:max_iter) {
    fx <- f(x)
    grad <- numeric(length(x))
    for (j in seq_along(x)) {
      xh <- x; xh[j] <- xh[j] + h
      grad[j] <- (f(xh) - fx) / h
    }

    v <- beta * v - lr * grad
    temp <- backtrack(f, x, v, fx, lr)
    x_new <- temp$x_new
    lr <- temp$lr

    if (sqrt(sum((x_new - x)^2)) < tol) {
      message("Converged in ", i, " iterations")
      return(x_new)
    }

    x <- x_new
  }

  stop("Did not converge")
}

adam <- function(f, x0, lr = 0.1, beta = 0.9, tol = 1e-6, max_iter = 1000, h = 1e-5) {
  backtrack <- function(f, x, v, fx, lr, beta_bt = 0.5) {
    while (TRUE) {
      x_new <- x + v
      if (f(x_new) < fx) break
      lr <- lr * beta_bt
      if (lr < 1e-12) break
      v <- v * beta_bt
    }
    list(x_new = x_new, lr = lr)
  }

  x <- x0
  v <- numeric(length(x))

  # Store mean of past gradients = m
  # Store uncentred variance of gradients = v
  # Normalise gradient by sqrt(variance) -> adaptive step per dimension
  # m ← β1·m + (1 - β1)·grad
  # v ← β2·v + (1 - β2)·(grad^2)
  # m̂ ← m / (1 - β1^t)
  # v̂ ← v / (1 - β2^t)
  # θ ← θ - lr × m̂ / (sqrt(v̂) + ε)
  
  for (i in 1:max_iter) {
    fx <- f(x)
    grad <- numeric(length(x))
    for (j in seq_along(x)) {
      xh <- x; xh[j] <- xh[j] + h
      grad[j] <- (f(xh) - fx) / h
    }

    v <- beta * v - lr * grad
    temp <- backtrack(f, x, v, fx, lr)
    x_new <- temp$x_new
    lr <- temp$lr

    if (sqrt(sum((x_new - x)^2)) < tol) {
      message("Converged in ", i, " iterations")
      return(x_new)
    }

    x <- x_new
  }

  stop("Did not converge")
}


quad <- function(x) sum((x - 3)^2)
res <- gradient_descent(quad, x0 = c(0, 0), lr = 0.1)
print(res)

res <- gradient_descent(quad, x0 = c(0, 0), lr = 1.5)
print(res)

res <- gradient_descent_momentum(quad, x0 = c(0, 0), lr = 0.1)
print(res)
