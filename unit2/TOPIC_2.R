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