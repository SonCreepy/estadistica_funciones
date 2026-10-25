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

joint_relative_frequency <- function(data_x, data_y) {
  joint_absolute_frequency(data_x, data_y) / length(data_x)
}

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

conditional_absolute_frequency_x_given_y <- function(joint_freqs, values_x, values_y, value_y) {
  column_position <- which(values_y == value_y)
  conditional <- numeric(length(values_x))
  for (i in seq_along(values_x)) {
    conditional[i] <- joint_freqs[i, column_position]
  }
  names(conditional) <- values_x
  conditional
}

conditional_relative_frequency_x_given_y <- function(joint_freqs, values_x, values_y, value_y) {
  conditional <- conditional_absolute_frequency_x_given_y(joint_freqs, values_x, values_y, value_y)
  relative_frequency(conditional, sum(conditional))
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

conditional_relative_frequency_y_given_x <- function(joint_freqs, values_x, values_y, value_x) {
  conditional <- conditional_absolute_frequency_y_given_x(joint_freqs, values_x, values_y, value_x)
  relative_frequency(conditional, sum(conditional))
}

bivariate_moment_with_respect_to_point <- function(joint_freqs, values_x, values_y, order_x, order_y, point_x, point_y) {
  relative_freqs <- relative_frequency(joint_freqs, sum(joint_freqs))
  total <- 0
  for (i in seq_along(values_x)) {
    for (j in seq_along(values_y)) {
      total <- total + (values_x[i] - point_x)^order_x * (values_y[j] - point_y)^order_y * relative_freqs[i, j]
    }
  }
  total
}

bivariate_ordinary_moment <- function(joint_freqs, values_x, values_y, order_x, order_y) {
  bivariate_moment_with_respect_to_point(joint_freqs, values_x, values_y, order_x, order_y, 0, 0)
}

bivariate_central_moment <- function(joint_freqs, values_x, values_y, order_x, order_y) {
  mean_x <- unname(weighted_mean(values_x, marginal_absolute_frequency_x(joint_freqs, values_x)))
  mean_y <- unname(weighted_mean(values_y, marginal_absolute_frequency_y(joint_freqs, values_y)))
  bivariate_moment_with_respect_to_point(joint_freqs, values_x, values_y, order_x, order_y, mean_x, mean_y)
}
# center_of_gravity = mean(x),mean(y)
center_of_gravity <- function(joint_freqs, values_x, values_y) {
  c(x = bivariate_ordinary_moment(joint_freqs, values_x, values_y, 1, 0),
    y = bivariate_ordinary_moment(joint_freqs, values_x, values_y, 0, 1))
}

covariance_double_entry_table <- function(joint_freqs, values_x, values_y) {
  bivariate_central_moment(joint_freqs, values_x, values_y, 1, 1)
}

covariance_simple_table <- function(values_x, values_y, frequencies = rep(1, length(values_x))) {
  mean_x <- weighted_mean(values_x, frequencies)
  mean_y <- weighted_mean(values_y, frequencies)
  total <- 0
  for (i in seq_along(values_x)) {
    total <- total + (values_x[i] - mean_x) * (values_y[i] - mean_y) * frequencies[i]
  }
  total / sum(frequencies)
}

covariance_alternative_form_double_entry_table <- function(joint_freqs, values_x, values_y) {
  bivariate_ordinary_moment(joint_freqs, values_x, values_y, 1, 1) -
    bivariate_ordinary_moment(joint_freqs, values_x, values_y, 1, 0) * bivariate_ordinary_moment(joint_freqs, values_x, values_y, 0, 1)
}

covariance_alternative_form_simple_table <- function(values_x, values_y, frequencies = rep(1, length(values_x))) {
  mean_x <- weighted_mean(values_x, frequencies)
  mean_y <- weighted_mean(values_y, frequencies)
  total <- 0
  for (i in seq_along(values_x)) {
    total <- total + values_x[i] * values_y[i] * frequencies[i]
  }
  total / sum(frequencies) - mean_x * mean_y
}

independence_char <- function(joint_freqs, values_x, values_y) {
  joint_relative_freqs <- relative_frequency(joint_freqs, sum(joint_freqs))
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

uncorrelated_char <- function(joint_freqs, values_x, values_y) {
  abs(covariance_double_entry_table(joint_freqs, values_x, values_y)) < 1e-10
}

correlation_coefficient <- function(joint_freqs, values_x, values_y) {
  covariance_double_entry_table(joint_freqs, values_x, values_y) /
    sqrt(bivariate_central_moment(joint_freqs, values_x, values_y, 2, 0) *
           bivariate_central_moment(joint_freqs, values_x, values_y, 0, 2))
}

funct_dependence_x_on_y <- function(joint_freqs) {
  for (j in seq_len(ncol(joint_freqs))) {
    if (sum(joint_freqs[, j] != 0) != 1) {
      return(FALSE)
    }
  }
  TRUE
}

funct_dependence_y_on_x <- function(joint_freqs) {
  for (i in seq_len(nrow(joint_freqs))) {
    if (sum(joint_freqs[i, ] != 0) != 1) {
      return(FALSE)
    }
  }
  TRUE
}

sum_of_squared_errors <- function(data_y, estimated_y) {
  errors <- residual_vector(data_y, estimated_y)
  total <- 0
  for (i in seq_along(errors)) {
    total <- total + errors[i]^2
  }
  total
}

regression_mean_squared_error <- function(data_y, estimated_y) {
  sum_of_squared_errors(data_y, estimated_y) / length(data_y)
}

residual_vector <- function(data_y, estimated_y) {
  data_y - estimated_y
}

variance_explained <- function(data_y, estimated_y) {
  coefficient_of_determination(data_y, estimated_y) * variance(data_y)
}

coefficient_of_determination <- function(data_y, estimated_y) {
  1 - regression_mean_squared_error(data_y, estimated_y) / variance(data_y)
}

coefficient_of_determination_alternative_form <- function(data_y, estimated_y) {
  variance_explained(data_y, estimated_y) / variance(data_y)
}

linear_fitting <- function(data_x, a, b) {
  a + b * data_x
}

quadratic_fitting <- function(data_x, a, b, c) {
  a * data_x^2 + b * data_x + c
}

hyperbolic_fitting <- function(data_x, a, b) {
  1 / (a + b * data_x)
}

exponential_fitting <- function(data_x, a, b) {
  a * exp(b * data_x)
}

general_regression_curve_y_on_x <- function(joint_freqs, values_x, values_y) {
  conditional_means <- numeric(length(values_x))
  for (i in seq_along(values_x)) {
    conditional_means[i] <- weighted_mean(values_y, conditional_absolute_frequency_y_given_x(joint_freqs, values_x, values_y, values_x[i]))
  }
  names(conditional_means) <- values_x
  conditional_means
}

least_squares_obj_funct <- function(data_y, estimated_y) {
  total <- 0
  for (i in seq_along(data_y)) {
    total <- total + (data_y[i] - estimated_y[i])^2
  }
  total
}

linear_least_squares_obj_funct <- function(data_x, data_y, a, b) {
  least_squares_obj_funct(data_y, linear_fitting(data_x, a, b))
}

linear_least_squares_obj_funct_x_on_y <- function(data_x, data_y, a_prime, b_prime) {
  least_squares_obj_funct(data_x, linear_fitting(data_y, a_prime, b_prime))
}

normal_equations_regression_line_y_on_x <- function(data_x, data_y) {
  sum_x <- 0
  sum_y <- 0
  sum_xy <- 0
  sum_x_squared <- 0
  for (i in seq_along(data_x)) {
    sum_x <- sum_x + data_x[i]
    sum_y <- sum_y + data_y[i]
    sum_xy <- sum_xy + data_x[i] * data_y[i]
    sum_x_squared <- sum_x_squared + data_x[i]^2
  }
  coefficients_matrix <- matrix(c(length(data_x), sum_x,
                                  sum_x, sum_x_squared), nrow = 2, byrow = TRUE)
  independent_terms <- c(sum_y, sum_xy)
  solution <- solve(coefficients_matrix, independent_terms)
  c(a = solution[[1]], b = solution[[2]])
}

normal_equations_regression_line_x_on_y <- function(data_x, data_y) {
  sum_x <- 0
  sum_y <- 0
  sum_xy <- 0
  sum_y_squared <- 0
  for (i in seq_along(data_y)) {
    sum_x <- sum_x + data_x[i]
    sum_y <- sum_y + data_y[i]
    sum_xy <- sum_xy + data_x[i] * data_y[i]
    sum_y_squared <- sum_y_squared + data_y[i]^2
  }
  coefficients_matrix <- matrix(c(length(data_y), sum_y,
                                  sum_y, sum_y_squared), nrow = 2, byrow = TRUE)
  independent_terms <- c(sum_x, sum_xy)
  solution <- solve(coefficients_matrix, independent_terms)
  c(a_prime = solution[[1]], b_prime = solution[[2]])
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

linear_correlation_coefficient <- function(data_x, data_y) {
  covariance_simple_table(data_x, data_y) / (standard_deviation(data_x) * standard_deviation(data_y))
}

multiple_linear_fitting <- function(predictors_data, coefficients) {
  estimated_y <- numeric(nrow(predictors_data))
  for (i in seq_len(nrow(predictors_data))) {
    estimated_y[i] <- coefficients[1]
    for (j in seq_len(ncol(predictors_data))) {
      estimated_y[i] <- estimated_y[i] + coefficients[j + 1] * predictors_data[i, j]
    }
  }
  estimated_y
}

multiple_linear_least_squares_objective_function <- function(predictors_data, data_y, coefficients) {
  least_squares_obj_funct(data_y, multiple_linear_fitting(predictors_data, coefficients))
}

normal_equations_multiple_regression <- function(predictors_data, data_y) {
  matrix_m <- cbind(1, predictors_data)
  coefficients <- solve(t(matrix_m) %*% matrix_m, t(matrix_m) %*% data_y)
  names_coefficients <- paste0("a", seq_along(coefficients) - 1)
  setNames(as.numeric(coefficients), names_coefficients)
}

least_squares_solution_multiple_regression <- function(predictors_data, data_y) {
  matrix_m <- cbind(1, predictors_data)
  coefficients <- solve(t(matrix_m) %*% matrix_m) %*% t(matrix_m) %*% data_y
  names_coefficients <- paste0("a", seq_along(coefficients) - 1)
  setNames(as.numeric(coefficients), names_coefficients)
}

exponential_fitting_by_linearization <- function(data_x, data_y) {
  log_data_y <- log(data_y)
  intercept_a_capital <- regression_intercept_y_on_x(data_x, log_data_y)
  c(A = intercept_a_capital,
    a = exp(intercept_a_capital),
    b = regression_slope_y_on_x(data_x, log_data_y))
}

hyperbolic_fitting_by_linearization <- function(data_x, data_y) {
  inverse_data_y <- 1 / data_y
  c(a = regression_intercept_y_on_x(data_x, inverse_data_y),
    b = regression_slope_y_on_x(data_x, inverse_data_y))
}


## Below just in case simpler alternative functions, and some useful rules

x_meanF <- function(row, table, N) { 
  return(sum(row*rowSums(table))/N)
}


y_meanF <- function(col, table, N) {
  return(sum(col*colSums(table))/N)
}


mom_ord2 <- function(data_x, data_y, joint_freqs, r, s, a,b) {
  N <- sum(joint_freqs)
  sum <- 0
  for(i in seq_along(data_x)) {
    for(j in seq_along(data_y)) {
      sum <- sum + (data_x[i]-a)^r * (data_y[j]-b)^s * joint_freqs[i, j]/N
    }
    
  }
  
  return (sum)
}

#For ordinary moments, a,b=0,0, for central moments, a,b=x_mean,y_mean


#central moment 20 = s2n(x)
#central moment 02 = s2n(y)
#central moment 11 = cov(x,y)- If cov(x,y) = 0, uncorrelated
#ordinary moment 10 = mean_x
#ordinary moment 01 = mean_y

#Regression lines: y = y_mean + (Cov(x,y)/s2n(x) * (x-x_mean)
#Regression lines: x = x_mean + (Cov(x,y)/s2n(Y) * (y-y_mean)

#r^2 = b*b'
#b = r * sn(Y)/sn(X)
#b`= r*sn(X)/sn(Y)

#0<r<1 direct linear correlation
#-1<=r<0 inverse linear correlation
#r=1 or r=-1 perfect linear correlation


#mY <- regression_slope_y_on_x(data_x,data_y)
#iY <- regression_intercept_y_on_x(data_x,data_y)
#cat("y =",mY,"x +",iY)

#mX <- regression_slope_x_on_y(data_x,data_y)
#iX <- regression_intercept_x_on_y(data_x,data_y)
#cat("x =",mX,"y +",iX)

#plot(x,y,pch=21,bg='lightblue')
#abline(iY,mY,col='red')
#rmX <- 1/mX
#riX <- -iX/mX
#abline(riX,rmX,col='blue')

#x_corte <- (riX - iY) / (mY - rmX)
#y_corte <- iY + mY * x_corte

#c <- center_of_gravity(t,x,y)
#c
#cat(x_corte,y_corte)
