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