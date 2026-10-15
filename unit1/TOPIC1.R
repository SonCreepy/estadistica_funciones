absolute_frequency <- function(data) {
  modalities <- sort(unique(data))
  counts <- numeric(length(modalities))
  for (i in seq_along(modalities)) {
    counts[i] <- sum(data == modalities[i])
  }
  names(counts) <- modalities
  counts
}

relative_frequency <- function(absolute_freq, totalN) {
  absolute_freq / totalN
}

cumulative_absolute_frequency <- function(absolute_freqs) {
  cumulative <- numeric(length(absolute_freqs))
  total <- 0
  for (i in seq_along(absolute_freqs)) {
    total <- total + absolute_freqs[i]
    cumulative[i] <- total
  }
  cumulative
}

cumulative_relative_frequency <- function(cumulative_absolute_freq, totalN) {
  cumulative_absolute_freq / totalN
}

arithmetic_mean <- function(data) {
  n_i <- absolute_frequency(data)
  xi <- as.numeric(names(n_i))
  total <- 0
  for (i in seq_along(xi)) {
    total <- total + xi[i] * n_i[[i]]
  }
  total / length(data)
}

weighted_mean <- function(values, weights) {
  numerator <- 0
  for (i in seq_along(values)) {
    numerator <- numerator + values[i] * weights[i]
  }
  numerator / sum(weights)
}

quadratic_mean <- function(data) {
  absolute_freqs <- absolute_frequency(data)
  values <- as.numeric(names(absolute_freqs))
  total <- 0
  for (i in seq_along(values)) {
    total <- total + values[i]^2 * absolute_freqs[[i]]
  }
  sqrt(total / length(data))
}

quadratic_mean_grouped <- function(values, frequencies) {
  total <- 0
  for (i in seq_along(values)) {
    total <- total + values[i]^2 * frequencies[i]
  }
  sqrt(total / sum(frequencies))
}

mode_value <- function(data) {
  absolute_freqs <- absolute_frequency(data)
  modalities <- as.numeric(names(absolute_freqs))
  if (length(modalities) > 1 && length(unique(absolute_freqs)) == 1) {
    return(numeric(0))
  }
  modalities[absolute_freqs == max(absolute_freqs)]
}

median_value <- function(data) {
  quantile(data, 0.5)
}

quantile <- function(data, proportion) {
  sorted_data <- sort(data)
  position <- round(proportion * length(data), 10)
  integer_part <- floor(position)
  decimal_part <- position - integer_part
  if (decimal_part == 0) {
    (sorted_data[integer_part] + sorted_data[integer_part + 1]) / 2
  } else {
    sorted_data[integer_part + 1]
  }
}

percentile <- function(data, percentile_number) {
  quantile(data, percentile_number / 100)
}

deviation <- function(value, parameter) {
  abs(value - parameter)
}

mean_deviation_concerning_average <- function(data, parameter) {
  absolute_freqs <- absolute_frequency(data)
  values <- as.numeric(names(absolute_freqs))
  total <- 0
  for (i in seq_along(values)) {
    total <- total + deviation(values[i], parameter) * absolute_freqs[[i]]
  }
  total / length(data)
}

mean_deviation <- function(data) {
  mean_deviation_concerning_average(data, unname(arithmetic_mean(data)))
}

mean_squared_error <- function(data, parameter) {
  absolute_freqs <- absolute_frequency(data)
  values <- as.numeric(names(absolute_freqs))
  total <- 0
  for (i in seq_along(values)) {
    total <- total + absolute_freqs[[i]] * (values[i] - parameter)^2
  }
  total / length(data)
}

variance <- function(data) {
  absolute_freqs <- absolute_frequency(data)
  values <- as.numeric(names(absolute_freqs))
  mean_value <- unname(arithmetic_mean(data))
  total <- 0
  for (i in seq_along(values)) {
    total <- total + (values[i] - mean_value)^2 * absolute_freqs[[i]]
  }
  total / length(data)
}

standard_deviation <- function(data) {
  sqrt(variance(data))
}

sample_variance <- function(data) {
  absolute_freqs <- absolute_frequency(data)
  values <- as.numeric(names(absolute_freqs))
  sample_mean <- unname(arithmetic_mean(data))
  total <- 0
  for (i in seq_along(values)) {
    total <- total + (values[i] - sample_mean)^2 * absolute_freqs[[i]]
  }
  total / length(data)
}

sample_quasi_variance <- function(data) {
  absolute_freqs <- absolute_frequency(data)
  values <- as.numeric(names(absolute_freqs))
  sample_mean <- unname(arithmetic_mean(data))
  total <- 0
  for (i in seq_along(values)) {
    total <- total + (values[i] - sample_mean)^2 * absolute_freqs[[i]]
  }
  total / (length(data) - 1)
}

sample_quasi_variance_from_sample_variance <- function(sample_variance_value, sample_size) {
  sample_size / (sample_size - 1) * sample_variance_value
}

pearson_coefficient_of_variation <- function(standard_deviation_value, mean_value) {
  standard_deviation_value / abs(mean_value)
}

moment_with_respect_to_point <- function(data, order, point) {
  absolute_freqs <- absolute_frequency(data)
  values <- as.numeric(names(absolute_freqs))
  total <- 0
  for (i in seq_along(values)) {
    total <- total + absolute_freqs[[i]] * (values[i] - point)^order
  }
  total / length(data)
}

ordinary_moment <- function(data, order) {
  absolute_freqs <- absolute_frequency(data)
  values <- as.numeric(names(absolute_freqs))
  total <- 0
  for (i in seq_along(values)) {
    total <- total + absolute_freqs[[i]] * values[i]^order
  }
  total / length(data)
}

central_moment <- function(data, order) {
  absolute_freqs <- absolute_frequency(data)
  values <- as.numeric(names(absolute_freqs))
  mean_value <- unname(arithmetic_mean(data))
  total <- 0
  for (i in seq_along(values)) {
    total <- total + absolute_freqs[[i]] * (values[i] - mean_value)^order
  }
  total / length(data)
}

pearson_skewness_coefficient <- function(data) {
  (unname(arithmetic_mean(data)) - mode_value(data)) / standard_deviation(data)
}

fisher_skewness_coefficient <- function(data) {
  central_moment(data, 3) / standard_deviation(data)^3
}

fisher_kurtosis_coefficient <- function(data) {
  central_moment(data, 4) / standard_deviation(data)^4 - 3
}
