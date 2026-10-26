#IF FULL TABLE AS INPUT, FOR REGRESSION LINES, USE:
expand_frequency_table <- function(joint_freqs, values_x, values_y) {
  idx <- which(joint_freqs > 0, arr.ind = TRUE)   # celdas con al menos un caso
  reps <- joint_freqs[idx]                        # cuántas veces se repite cada par
  list(x = rep(values_x[idx[, 1]], reps),
       y = rep(values_y[idx[, 2]], reps))
}
#JOINT ABSOLUTE FREQUENCY ---------------------------------
joint_absolute_frequency <- function(data_x, data_y) {
  x_modalities <- sort(unique(data_x))
  y_modalities <- sort(unique(data_y))
  counts <- matrix(0, nrow = length(x_modalities), ncol = length(y_modalities))
  for (i in seq_along(x_modalities)) {
    for (j in seq_along(y_modalities)) {
      counts[i, j] <- sum(data_x == x_modalities[i] & data_y == y_modalities[j])
    }
  }
  rownames(counts) <- x_modalities
  colnames(counts) <- y_modalities
  counts
}
# JOINT RELATIVE FREQUENCY ---------------------------------
joint_relative_frequency <- function(data_x, data_y) {
  joint_absolute_frequency(data_x, data_y) / length(data_x)
}
#SCATTER PLOT ---------------------------------
scatter_plot <- function(data_x, data_y) {
  absolute_freqs <- joint_absolute_frequency(data_x, data_y)
  x_modalities <- sort(unique(data_x))
  y_modalities <- sort(unique(data_y))
  points_x <- numeric(0)
  points_y <- numeric(0)
  points_frequency <- numeric(0)
  for (i in seq_along(x_modalities)) {
    for (j in seq_along(y_modalities)) {
      if (absolute_freqs[i, j] > 0) {
        points_x <- c(points_x, x_modalities[i])
        points_y <- c(points_y, y_modalities[j])
        points_frequency <- c(points_frequency, absolute_freqs[i, j])
      }
    }
  }
  x_padding <- 0.15 * diff(range(data_x))
  y_padding <- 0.15 * diff(range(data_y))
  plot(points_x, points_y,
       cex = 2 * sqrt(points_frequency),
       pch = 21,
       bg = "lightblue",
       xlim = range(data_x) + c(-x_padding, x_padding),
       ylim = range(data_y) + c(-y_padding, y_padding),
       xlab = "X",
       ylab = "Y",
       main = "Scatter plot")
}
#MARGINAL ABSOLUTE FREQUENCIES ---------------------------------
marginal_absolute_frequency_x <- function(joint_freqs, values_x) {
  row_totals <- numeric(nrow(joint_freqs))
  for (i in seq_len(nrow(joint_freqs))) {
    row_totals[i] <- sum(joint_freqs[i, ])
  }
  names(row_totals) <- values_x
  row_totals
}
marginal_absolute_frequency_y <- function(joint_freqs, values_y) {
  column_totals <- numeric(ncol(joint_freqs))
  for (j in seq_len(ncol(joint_freqs))) {
    column_totals[j] <- sum(joint_freqs[, j])
  }
  names(column_totals) <- values_y
  column_totals
}
#CONDITIONAL FREQUENCIES ---------------------------------
conditional_absolute_frequency_x_given_y <- function(joint_freqs, values_x, values_y, value_y) {
  column_position <- which(values_y == value_y)
  conditional <- numeric(length(values_x))
  for (i in seq_along(values_x)) {
    conditional[i] <- joint_freqs[i, column_position]
  }
  names(conditional) <- values_x
  conditional
}
conditional_absolute_frequency_y_given_x <- function(joint_freqs, values_x, values_y, value_x) {
  row_position <- which(values_x == value_x)
  conditional <- numeric(length(values_y))
  for (j in seq_along(values_y)) {
    conditional[j] <- joint_freqs[row_position, j]
  }
  names(conditional) <- values_y
  conditional
}
#BIVARIATE MOMENTS ---------------------------------
bivariate_moment_with_respect_to_point <- function(joint_freqs, values_x, values_y, order_x, order_y, point_x, point_y) {
  relative_freqs <- joint_freqs / sum(joint_freqs)
  total <- 0
  for (i in seq_along(values_x)) {
    for (j in seq_along(values_y)) {
      total <- total + (values_x[i] - point_x)^order_x * (values_y[j] - point_y)^order_y * relative_freqs[i, j]
    }
  }
  total
}
#BIVARIATE ORDINARY MOMENT ---------------------------------
bivariate_ordinary_moment <- function(joint_freqs, values_x, values_y, order_x, order_y) {
  bivariate_moment_with_respect_to_point(joint_freqs, values_x, values_y, order_x, order_y, 0, 0)
}
#BIVARIATE CENTRAL MOMENT ---------------------------------
bivariate_central_moment <- function(joint_freqs, values_x, values_y, order_x, order_y) {
  mean_x <- unname(arithmetic_mean(values_x, marginal_absolute_frequency_x(joint_freqs, values_x)))
  mean_y <- unname(arithmetic_mean(values_y, marginal_absolute_frequency_y(joint_freqs, values_y)))
  bivariate_moment_with_respect_to_point(joint_freqs, values_x, values_y, order_x, order_y, mean_x, mean_y)
}
#CENTER OF GRAVITY ---------------------------------
# center_of_gravity = mean(x),mean(y)
center_of_gravity <- function(joint_freqs, values_x, values_y) {
  c(x = bivariate_ordinary_moment(joint_freqs, values_x, values_y, 1, 0),
    y = bivariate_ordinary_moment(joint_freqs, values_x, values_y, 0, 1))
}
#COVARIANCE == BIVARIATE CENTRAL MOMENT(1,1) ---------------------------------
covariance_double_entry_table <- function(joint_freqs, values_x, values_y) {
  bivariate_central_moment(joint_freqs, values_x, values_y, 1, 1)
}
#COVARIANCE == BIVARIATE CENTRAL MOMENT(1,1) Used in regression ---------------------------------
covariance_simple_table <- function(values_x, values_y, frequencies = rep(1, length(values_x))) {
  mean_x <- arithmetic_mean(values_x, frequencies)
  mean_y <- arithmetic_mean(values_y, frequencies)
  total <- 0
  for (i in seq_along(values_x)) {
    total <- total + (values_x[i] - mean_x) * (values_y[i] - mean_y) * frequencies[i]
  }
  total / sum(frequencies)
}
#INDEPENDENCE CHECK
independence_char <- function(joint_freqs, values_x, values_y) {
  joint_relative_freqs <- joint_freqs/sum(joint_freqs)
  marginal_x <- marginal_absolute_frequency_x(joint_freqs, values_x)/length(values_x)
  marginal_y <- marginal_absolute_frequency_y(joint_freqs, values_y)/length(values_y)
  for (i in seq_along(values_x)) {
    for (j in seq_along(values_y)) {
      if (abs(joint_relative_freqs[i, j] - marginal_x[[i]] * marginal_y[[j]]) > 1e-10) {
        return(FALSE)
      }
    }
  }
  TRUE
}

#UNCORRELATED_CHECK
uncorrelated_char <- function(joint_freqs, values_x, values_y) {
  abs(covariance_double_entry_table(joint_freqs, values_x, values_y)) < 1e-10
}
#CORRELATION COEFFICIENT
correlation_coefficient <- function(joint_freqs, values_x, values_y) {
  covariance_double_entry_table(joint_freqs, values_x, values_y) /
    sqrt(bivariate_central_moment(joint_freqs, values_x, values_y, 2, 0) *
           bivariate_central_moment(joint_freqs, values_x, values_y, 0, 2))
}
#FUNCTIONAL DEPENDENCE CHECK
funct_dependence_x_on_y <- function(joint_freqs) {
  for (j in seq_len(ncol(joint_freqs))) {
    if (sum(joint_freqs[, j] != 0) != 1) {
      return(FALSE)
    }
  }
  TRUE
}
#FUNCTIONAL DEPENDENCE CHECK
funct_dependence_y_on_x <- function(joint_freqs) {
  for (i in seq_len(nrow(joint_freqs))) {
    if (sum(joint_freqs[i, ] != 0) != 1) {
      return(FALSE)
    }
  }
  TRUE
}
#SSE (SUM OF SQUARED ERRORS) ---------------------------------
sum_of_squared_errors <- function(data_y, estimated_y) {
  errors <- data_y - estimated_y
  total <- 0
  for (i in seq_along(errors)) {
    total <- total + errors[i]^2
  }
  total
}
#R2 (COEFFICIENT OF DETERMINATION) ---------------------------------
coefficient_of_determination <- function(data_y, estimated_y) {
  1 - mean_squared_error(data_y, estimated_y) / variance(data_y)
}
#MEAN SQUARED ERROR -----------------------------------------------
mean_squared_error <- function(data_y, estimated_y) {
  sum_of_squared_errors(data_y, estimated_y) / length(data_y)
}
regression_slope_y_on_x <- function(data_x, data_y) {
  covariance_simple_table(data_x, data_y) / variance(data_x)
}

regression_intercept_y_on_x <- function(data_x, data_y) {
  unname(mean(data_y)) - regression_slope_y_on_x(data_x, data_y) * unname(mean(data_x))
}

regression_slope_x_on_y <- function(data_x, data_y) {
  covariance_simple_table(data_x, data_y) / variance(data_y)
}

regression_intercept_x_on_y <- function(data_x, data_y) {
  unname(mean(data_x)) - regression_slope_x_on_y(data_x, data_y) * unname(mean(data_y))
}

regression_line_y_on_x <- function(data_x, data_y, x) {
  unname(mean(data_y)) + covariance_simple_table(data_x, data_y) / variance(data_x) * (x - unname(mean(data_x)))
}

regression_line_x_on_y <- function(data_x, data_y, y) {
  unname(mean(data_x)) + covariance_simple_table(data_x, data_y) / variance(data_y) * (y - unname(mean(data_y)))
}
#FULL REGRESSION FUNCTION ---------------------------------
#PLOT_REGRESSION: Plots the regression line for a given model type and returns coefficients, equation, and R².
plot_regression <- function(x, y,
                            type = c("linear", "log", "exp", "ab", "quad", "pow"),
                            C = NULL) {
  type <- match.arg(type)
  
  # --- Validation ---
  if (length(x) != length(y)) stop("x and y must have the same length.")
  ok <- complete.cases(x, y); x <- x[ok]; y <- y[ok]
  if (!is.null(C) && type != "quad") warning("C is only used with 'quad'; ignoring it.")
  if (type %in% c("log", "pow") && any(x <= 0)) stop("This model requires x > 0.")
  if (type %in% c("exp", "ab", "pow") && any(y <= 0)) stop("This model requires y > 0.")
  
  s <- function(v) sprintf("%+.4g", v)  # signed number for the equation
  
  # --- Fit each model ---
  if (type == "linear") {                       # y = a + b·x
    cf <- coef(lm(y ~ x)); a <- cf[[1]]; b <- cf[[2]]
    f  <- function(t) a + b * t
    eq <- sprintf("y = %.4g %s·x", a, s(b))
    co <- c(a = a, b = b)
    
  } else if (type == "log") {                   # y = a + b·ln(x)
    cf <- coef(lm(y ~ log(x))); a <- cf[[1]]; b <- cf[[2]]
    f  <- function(t) a + b * log(t)
    eq <- sprintf("y = %.4g %s·ln(x)", a, s(b))
    co <- c(a = a, b = b)
    
  } else if (type == "exp") {                   # y = a·e^(b·x)
    cf <- coef(lm(log(y) ~ x)); a <- exp(cf[[1]]); b <- cf[[2]]
    f  <- function(t) a * exp(b * t)
    eq <- sprintf("y = %.4g·e^(%.4g·x)", a, b)
    co <- c(a = a, b = b)
    
  } else if (type == "ab") {                    # y = a·b^x
    cf <- coef(lm(log(y) ~ x)); a <- exp(cf[[1]]); b <- exp(cf[[2]])
    f  <- function(t) a * b^t
    eq <- sprintf("y = %.4g·%.4g^x", a, b)
    co <- c(a = a, b = b)
    
  } else if (type == "pow") {                   # y = a·x^b
    cf <- coef(lm(log(y) ~ log(x))); a <- exp(cf[[1]]); b <- cf[[2]]
    f  <- function(t) a * t^b
    eq <- sprintf("y = %.4g·x^%.4g", a, b)
    co <- c(a = a, b = b)
    
  } else {                                      # quad: y = a + b·x + c·x²
    if (is.null(C)) {
      cf <- coef(lm(y ~ x + I(x^2)))
      a <- cf[[1]]; b <- cf[[2]]; c_ <- cf[[3]]
    } else {
      cf <- coef(lm(I(y - C * x^2) ~ x))        # c fixed by the user
      a <- cf[[1]]; b <- cf[[2]]; c_ <- C
    }
    f  <- function(t) a + b * t + c_ * t^2
    eq <- sprintf("y = %.4g %s·x %s·x²", a, s(b), s(c_))
    co <- c(a = a, b = b, c = c_)
  }
  
  # --- R² on the original y scale ---
  r2 <- 1 - sum((y - f(x))^2) / sum((y - mean(y))^2)
  
  # --- Plot ---
  plot(x, y, pch = 19, col = "steelblue",
       main = paste("Regression:", type), xlab = "x", ylab = "y")
  xs <- seq(min(x), max(x), length.out = 300)
  lines(xs, f(xs), col = "red", lwd = 2)
  legend("topleft", bty = "n",
         legend = c(eq, sprintf("R² = %.4f", r2)),
         text.col = c("red", "black"))
  
  invisible(list(type = type, coefficients = co, equation = eq, r2 = r2))
}
# ---- fit_model ---------------------------------------------------------------
# WHAT IT DOES: fits ONE model and returns parameters, prediction function f(x),
#   yest, MSE, RMSE, SSE, R2.
# ARGUMENTS: x, y; model = "linear","exponential","hyperbolic","power","ab","logarithmic";
#   w = frequencies (optional).
# LINKS TO: goodness_of_fit(), compare_models(), predict_model(), fit_plot().
# EXAMPLE (slide 103): x <- c(0,1,2,3,6); y <- c(7,5,4,3.5,3)
#   fit <- fit_model(x, y, "exponential");  fit$a ; fit$b ; fit$MSE ; fit$R2
fit_model <- function(x, y, model = c("linear", "exponential", "hyperbolic", "power", "ab", "logarithmic"),
                      w = NULL) {
  model <- match.arg(model)
  if (is.null(w)) w <- rep(1, length(x))
  if (model %in% c("exponential", "power", "ab") && any(y <= 0)) stop("This model requires y > 0")
  if (model %in% c("power", "logarithmic") && any(x <= 0)) stop("This model requires x > 0")
  if (model == "hyperbolic" && any(y == 0)) stop("The hyperbolic model requires y != 0")
  fit <- switch(model,
    linear      = lm(y ~ x, weights = w),
    exponential = lm(log(y) ~ x, weights = w),
    hyperbolic  = lm(I(1 / y) ~ x, weights = w),
    power       = lm(log(y) ~ log(x), weights = w),
    ab          = lm(log(y) ~ x, weights = w),
    logarithmic = lm(y ~ log(x), weights = w))
  A <- unname(coef(fit)[1]); B <- unname(coef(fit)[2])
  p <- switch(model,
    linear = c(A, B), exponential = c(exp(A), B), hyperbolic = c(A, B),
    power = c(exp(A), B), ab = c(exp(A), exp(B)), logarithmic = c(A, B))
  a <- p[1]; b <- p[2]
  f <- switch(model,
    linear      = function(t) a + b * t,
    exponential = function(t) a * exp(b * t),
    hyperbolic  = function(t) 1 / (a + b * t),
    power       = function(t) a * t^b,
    ab          = function(t) a * b^t,
    logarithmic = function(t) a + b * log(t))
  sg <- function(v) if (v < 0) "-" else "+"
  formula_txt <- switch(model,
    linear      = sprintf("y = %.5g %s %.5g x", a, sg(b), abs(b)),
    exponential = sprintf("y = %.5g * exp(%.5g x)", a, b),
    hyperbolic  = sprintf("y = 1 / (%.5g %s %.5g x)", a, sg(b), abs(b)),
    power       = sprintf("y = %.5g * x^%.5g", a, b),
    ab          = sprintf("y = %.5g * %.5g^x", a, b),
    logarithmic = sprintf("y = %.5g %s %.5g ln(x)", a, sg(b), abs(b)))
  g <- coefficient_of_determination (y, f(x))
  c(list(model = model, a = a, b = b, formula = formula_txt, f = f, yest = f(x)),
    g[c("MSE", "RMSE", "SSE", "R2", "Vex", "residuals")])
}
