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
##RELATIVE FREQUENCY-----------------------------------------------------
relative_frequency <- function(absolute_freq, totalN) {
  absolute_freq / totalN
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
##CUMULATIVE RELATIVE FREQUENCY-----------------------------------------------
cumulative_relative_frequency <- function(cumulative_absolute_freq, totalN) {
  cumulative_absolute_freq / totalN
}
##ARITHMETIC MEAN------------------------------------------------------------
mean(data)

##WEIGHTED MEAN-----------------------------------------------------------
weighted_mean <- function(values, weights) {
  numerator <- 0
  for (i in seq_along(values)) {
    numerator <- numerator + values[i] * weights[i]
  }
  numerator / sum(weights)
}
##QUADRATIC MEAN-------------------------------------------------------

 quadratic_mean <- function(data){  
    return (sqrt(sum((data^2)) / data.length))
 } 

##QUADRATIC MEAN GROUPED-----------------------------------------------------
quadratic_mean_grouped <- function(values, frequencies) {
  total <- 0
  for (i in seq_along(values)) {
    total <- total + values[i]^2 * frequencies[i]
  }
  sqrt(total / sum(frequencies))# sum(freqs) == N
}
##MODE VALUE----------------------------------------------------------------
mode_value <- function(data) {
  table <- table(data)
  as.numeric(names(table)[table == max(table)])
}
##MEDIAN VALUE--------------------------------------------------------------
median_value <- function(data) {
  quantile(data, 0.5)
}
##QUANTILE & PERCENTILE ----------------------------------------------------
data <- c(2, 5, 3, 4, 7, 0, 11, 2, 3, 8)

quartiles <- quantile(data, probs = c(0.25, 0.50, 0.75))
print("quartiles:")
print(quartiles)
percentile <- function(data, percentile_number) {
  quantile(data, percentile_number / 100)
}
##DEVIATION-------------------------------------------------------------------
deviation <- function(value, parameter) {
  abs(value - parameter)
}
##MEAN DEVIATION CONCERNING AVERAGE --------------------------------------------
mean_deviation_concerning_average <- function(data, parameter) {
  absolute_freqs <- absolute_frequency(data)
  values <- as.numeric(names(absolute_freqs))
  total <- 0
  for (i in seq_along(values)) {
    total <- total + deviation(values[i], parameter) * absolute_freqs[[i]]
  }
  total / length(data)
}
##MEAN DEVIATION---------------------------------------------------------------
mean_deviation <- function(data) {
  mean_deviation_concerning_average(data, unname(arithmetic_mean(data)))
}
##MEAN SQUARED ERROR-----------------------------------------------------------
mean_squared_error <- function(data, ref_point) {
  mse_mean <- sum((data - ref_point)^2) / data.length
}
##VARIANCE-------------------------------------------------------------------
variance <- function(data) {
  n <- length(data)
  variance <- var(data)*((n-1)/n)
}
##STANDARD DEVIATION---------------------------------------------------------
standard_deviation <- function(data) {
  sqrt(variance(data))
}
##SAMPLE QUASIVARIANCE--------------------------------------------------------
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
##QUASI VARIANCE FROM SAMPLE VARIANCE------------------------------------------
sample_quasi_variance_from_sample_variance <- function(sample_variance_value, sample_size) {
  sample_size / (sample_size - 1) * sample_variance_value
}
##PEARSON COEFFICIENT OF VARIATION---------------------------------------------
pearson_coefficient_of_variation <- function(standard_deviation_value, mean_value) {
  standard_deviation_value / abs(mean_value)
}
##MOMENT RESPECT TO POINT-------------------------------------------------------
moment_with_respect_to_point <- function(data, order, point) {
  absolute_freqs <- absolute_frequency(data)
  values <- as.numeric(names(absolute_freqs))
  total <- 0
  for (i in seq_along(values)) {
    total <- total + absolute_freqs[[i]] * (values[i] - point)^order
  }
  total / length(data)
}
##ORDINARY MOMENT---------------------------------------------------------------
ordinary_moment <- function(data, order) {
  absolute_freqs <- absolute_frequency(data)
  values <- as.numeric(names(absolute_freqs))
  total <- 0
  for (i in seq_along(values)) {
    total <- total + absolute_freqs[[i]] * values[i]^order
  }
  total / length(data)
}
##CENTRAL MOMENT------------------------------------------------------------------
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
##PEARSON SKEWNESS COEFFICIENT-----------------------------------------------
pearson_skewness_coefficient <- function(data) {
  (unname(arithmetic_mean(data)) - mode_value(data)) / standard_deviation(data)
}
##FISHER SKEWNESS COEFFICIENT--------------------------------------------------
fisher_skewness_coefficient <- function(data) {
  central_moment(data, 3) / standard_deviation(data)^3
}
##FISHER KURTOSIS COEFFICIENT-----------------------------------------------
fisher_kurtosis_coefficient <- function(data) {
  central_moment(data, 4) / standard_deviation(data)^4 - 3
}