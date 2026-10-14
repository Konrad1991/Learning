# --------------------------------------------------------------------------------
# linear model by hand
# --------------------------------------------------------------------------------
df <- data.frame(
  y = 1:18,
  group = factor(rep(c("A", "B", "C"), 6)),
  whatever = factor(c(rep(1, 9), rep(2, 9)))
)
df
#
# Design matrix for the full model
X <- model.matrix(y ~ group + whatever, df)
# Important: in the intercept the reference states group A and whatever1 are stored.
y <- df$y

# Estimate coefficients manually: beta = (X'X)^(-1) X'y
# The betas are the slopes for the difference of the groups to the references
# So the slope for groupB is the actual the slope of B - A
beta <- solve(t(X) %*% X) %*% t(X) %*% y
beta
y_hat <- X %*% beta
residuals <- y - y_hat

sum_squared_residuals <- sum(residuals^2)
var_hat <- sum_squared_residuals / (nrow(X) - ncol(X))
residual_standard_error <- sqrt(var_hat)
residual_standard_error
standard_error_beta_hat <- sqrt(
  diag(var_hat * solve(t(X) %*% X))
)
standard_error_beta_hat
t_values <- beta / standard_error_beta_hat
t_values

p_values <- 2 * pt(
  abs(t_values), df = nrow(X) - ncol(X), lower.tail = FALSE
)
p_values

# Compare with lm()
slm <- summary(lm(y ~ group + whatever, data = df))
slm$coefficients[, 1L] # Coefficients = beta
slm$residuals |> unname() # residuals
slm$sigma # residuals standard error
slm$coefficients[, 2] # standard error for each beta
slm$coefficients[, 3] # t values for each beta
slm$coefficients[, 4] # p values for the t values

# --------------------------------------------------------------------------------
# Simulate t values
# --------------------------------------------------------------------------------
set.seed(1234)
df <- data.frame(
  y = 1:18,
  group = factor(rep(c("A", "B", "C"), 6)),
  whatever = factor(c(rep(1, 9), rep(2, 9)))
)

X <- model.matrix(~ group + whatever, df)
beta_true <- c(5, 2, -1, 3)   # intercept, groupB, groupC, whatever2
sigma_true <- 2

n_sim <- 10000
t_groupB <- numeric(n_sim)

for (i in seq_len(n_sim)) {
  eps <- rnorm(nrow(X), mean = 0, sd = sigma_true)
  y_sim <- X %*% beta_true + eps
  
  beta_hat <- solve(t(X) %*% X) %*% t(X) %*% y_sim
  residuals <- y_sim - X %*% beta_hat
  
  sigma2_hat <- sum(residuals^2) / (nrow(X) - ncol(X))
  se_beta_hat <- sqrt(diag(sigma2_hat * solve(t(X) %*% X)))
  
  # t-value for groupB under H0: beta_groupB = 2
  t_groupB[i] <- (beta_hat[2] - beta_true[2]) / se_beta_hat[2]
}

hist(t_groupB, breaks = 50, probability = TRUE)
curve(dt(x, df = nrow(X) - ncol(X)), add = TRUE, lwd = 2)

# --------------------------------------------------------------------------------
# Visualization
# --------------------------------------------------------------------------------
set.seed(123)
library(ellipse)
n <- 100

# 1. Orthogonal
x1_orth <- rnorm(n)
x2_orth <- rnorm(n)

# 2. moderate correlated
x1_mod <- rnorm(n)
x2_mod <- 0.5 * x1_mod + rnorm(n, 0, 0.5)

# 3. strongly correlated
x1_bad <- rnorm(n)
x2_bad <- x1_bad + rnorm(n, 0, 0.05)

analyze_case <- function(x1, x2) {
  X <- cbind(1, x1, x2)
  y <- 3 + 2*x1 - 1*x2 + rnorm(length(x1), 0, 1)
  XtX <- t(X) %*% X
  XtX_inv <- solve(XtX)
  beta <- XtX_inv %*% t(X) %*% y
  residuals <- y - X %*% beta
  sigma2 <- sum(residuals^2) / (length(y) - ncol(X))
  var_beta <- sigma2 * XtX_inv
  
  plot(x1, x2, xlab = "x1", ylab = "x2", pch = 19)
  Sigma <- var_beta[2:3, 2:3]
  cat("Variances of beta 2 and 3\n")
  print(Sigma)
  eig_sigma <- eigen(Sigma)
  print(eig_sigma$values) # direction of ellipse
  print(eig_sigma$vectors) # length of axis
  ell <- ellipse(Sigma)
  ell[,1] <- ell[,1] + beta[2]
  ell[,2] <- ell[,2] + beta[3]
  plot(ell, type = "l", xlab = "beta1", ylab = "beta2")
  points(beta[2], beta[3], col = "red", pch = 19)
}

par(mfrow = c(3, 2))
analyze_case(x1_orth, x2_orth)
analyze_case(x1_mod, x2_mod)
analyze_case(x1_bad, x2_bad)
