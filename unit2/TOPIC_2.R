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
  mean_x <- unname(weighted_mean(values_x, marginal_absolute_frequency_x(joint_freqs, values_x)))
  mean_y <- unname(weighted_mean(values_y, marginal_absolute_frequency_y(joint_freqs, values_y)))
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
