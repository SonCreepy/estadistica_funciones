  absolute_frequency <- function(data) {
  modalities <- sort(unique(data))
  counts <- numeric(length(modalities))
  for (i in seq_along(modalities)) {
    counts[i] <- sum(data == modalities[i])
  }
  names(counts) <- modalities
  counts
}
