##ABSOLUTE FREQUENCY-------------------------------------------------------
  absolute_frequency <- function(data) {
  modalities <- sort(unique(data))
  counts <- numeric(length(modalities))
  for (i in seq_along(modalities)) {
    counts[i] <- sum(data == modalities[i])
  }
  names(counts) <- modalities
  counts
}
##CUMULATIVE ABSOLUTE FREQUENCY-------------------------------------------
cumulative_absolute_frequency <- function(absolute_freqs) {
  cumulative <- numeric(length(absolute_freqs))
  total <- 0
  for (i in seq_along(absolute_freqs)) {
    total <- total + absolute_freqs[i]
    cumulative[i] <- total
  }
  cumulative
}
##CLASS MARK-------------------------------------------------------------------
class_mark <- function(lower_limit, upper_limit) {
  (lower_limit + upper_limit) / 2
}
##HISTOGRAM HEIGHT-------------------------------------------------------------
histogram_height <- function(frequencies, interval_widths) {
  frequencies / interval_widths
}
