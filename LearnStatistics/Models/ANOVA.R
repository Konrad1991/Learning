set.seed(1234)
A1 <- rnorm(3, 10, 1)
B1 <- rnorm(3, 10, 1)
C1 <- rnorm(3, 10, 1)
A2 <- rnorm(3, 20, 1)
B2 <- rnorm(3, 20, 1)
C2 <- rnorm(3, 20, 1)
df <- data.frame(
  y = c(A1, B1, C1, A2, B2, C2),
  group = factor(rep(c("A", "B", "C"), 6)),
  whatever = factor(c(rep(1, 9), rep(2, 9)))
)
df
calc_f_test <- function(X_reduced, X_full, y, X_error = X_full) {
  calc_rss <- function(X, y) {
    beta <- solve(t(X) %*% X) %*% t(X) %*% y
    y_hat <- X %*% beta
    residuals <- y - y_hat
    sum(residuals^2)
  }

  rss_reduced <- calc_rss(X_reduced, y)
  rss_full <- calc_rss(X_full, y)
  rss_error <- calc_rss(X_error, y)

  p_reduced <- ncol(X_reduced)
  p_full <- ncol(X_full)
  p_error <- ncol(X_error)
  n <- nrow(X_error)

  df1 <- p_full - p_reduced
  df2 <- n - p_error

  sum_sq <- rss_reduced - rss_full
  mean_sq <- sum_sq / df1

  # F = explained variance / entire variance
  f_value <- ((rss_reduced - rss_full) / df1) / (rss_error / df2)
  p_value <- pf(f_value, df1 = df1, df2 = df2, lower.tail = FALSE)

  c(
    df1 = df1, df2 = df2,
    sum_sq = sum_sq, mean_sq = mean_sq,
    f_value = f_value,
    p_value = p_value,
    resid_sum_sq = rss_error, resid_mean_sq = rss_error / df2
  )
}

# -----------------------------------------------------------------
# Type II ANOVA
# -----------------------------------------------------------------
type_2_aov <- function(df, response, ...) {
  terms <- c(...)
  interaction_terms <- terms[grepl(":", terms)]
  main_terms <- terms[!grepl(":", terms)]
  y <- df[[response]]

  # Full model including interactions
  f_entire <- as.formula(
    paste0(response, " ~ ", paste(terms, collapse = " + "))
  )
  X_entire <- model.matrix(f_entire, df)

  # Additive model with main effects only
  f_additive <- as.formula(
    paste0(response, " ~ ", paste(main_terms, collapse = " + "))
  )
  X_additive <- model.matrix(f_additive, df)

  res <- list()

  # Type II tests for main effects:
  # numerator from additive comparison,
  # denominator from full model including interaction
  for (i in seq_along(main_terms)) {
    reduced_terms <- main_terms[-i]

    f_reduced <- as.formula(
      paste0(response, " ~ ", paste(reduced_terms, collapse = " + "))
    )
    X_reduced <- model.matrix(f_reduced, df)

    res[[main_terms[i]]] <- calc_f_test(
      X_reduced = X_reduced,
      X_full = X_additive,
      y = y,
      X_error = X_entire
    )
  }

  # Type II tests for interactions:
  # compare additive model vs full model
  for (term in interaction_terms) {
    res[[term]] <- calc_f_test(
      X_reduced = X_additive,
      X_full = X_entire,
      y = y,
      X_error = X_entire
    )
  }

  names <- names(res)
  result <- Reduce(rbind, res) |> as.data.frame()
  row.names(result) <- names
  result
}
type_2_aov(df, "y", "group", "whatever", "whatever:group")
summary(aov(y ~ group + whatever + whatever:group, data = df))

# -----------------------------------------------------------------
# Type I ANOVA; iterative addition of factors
# -----------------------------------------------------------------
type_1_aov <- function(df, response, ...) {
  terms <- c(...)
  y <- df[[response]]

  # final model for the denominator
  f_final <- as.formula(
    paste0(response, " ~ ", paste(terms, collapse = " + "))
  )
  X_error <- model.matrix(f_final, df)

  X_previous <- model.matrix(
    as.formula(paste0(response, " ~ 1")),
    df
  )

  res <- list()
  current_terms <- character(0)

  for (term in terms) {
    current_terms <- c(current_terms, term)

    f_current <- as.formula(
      paste0(response, " ~ ", paste(current_terms, collapse = " + "))
    )
    X_current <- model.matrix(f_current, df)

    res[[term]] <- calc_f_test(
      X_reduced = X_previous,
      X_full = X_current,
      y = y,
      X_error = X_error
    )

    X_previous <- X_current
  }

  names <- names(res)
  result <- Reduce(rbind, res) |> as.data.frame()
  row.names(result) <- names
  result
}
type_1_aov(df, "y", "group", "whatever", "whatever:group")
anova(lm(y ~ group + whatever + whatever:group, data = df))
