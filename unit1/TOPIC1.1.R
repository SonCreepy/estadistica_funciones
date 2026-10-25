##ABSOLUTE FREQUENCY-------------------------------------------------------
#Frequency table 
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
relative_frequency <- function(absolute_freq, total_n) {
  absolute_freq / total_n
}
##CUMULATIVE ABSOLUTE FREQUENCY-------------------------------------------
#Cumsum table
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
cumulative_relative_frequency <- function(cumulative_absolute_freq, total_n) {
  cumulative_absolute_freq / total_n
}
##INTERVAL WIDTH---------------------------------------------------------------
interval_width <- function(lower_limit, upper_limit) {
  upper_limit - lower_limit
}
##CLASS MARK-------------------------------------------------------------------
class_mark <- function(lower_limit, upper_limit) {
  (lower_limit + upper_limit) / 2
}
##HISTOGRAM HEIGHT-------------------------------------------------------------
histogram_height <- function(frequencies, interval_widths) {
  frequencies / interval_widths
}
##ARITHMETIC MEAN------------------------------------------------------------
arithmetic_mean <- function(values, frequencies) {
  total <- 0
  for (i in seq_along(values)) {
    total <- total + values[i] * frequencies[i]
  }
  total / sum(frequencies)
}
##AFFINE TRANSFORMATION MEAN--------------------------------------------------
affine_transformation_mean <- function(mean_x, slope, intercept) {
  slope * mean_x + intercept
}
##WEIGHTED MEAN-----------------------------------------------------------
weighted_mean <- function(values, weights) {
  numerator <- 0
  for (i in seq_along(values)) {
    numerator <- numerator + values[i] * weights[i]
  }
  numerator / sum(weights)
}
##PIE CHART AND BAR PLOT----------------------------------
#barplot(data,main="TITLE",ylab="COLUMNS",col)
#pie(data, main, col=c(1,2,3))

##QUADRATIC MEAN-------------------------------------------------------
quadratic_mean <- function(data ,frequencies) {
  sqrt(sum(data^2 * frequencies) / sum(frequencies))
}
##QUADRATIC MEAN GROUPED-----------------------------------------------------
quadratic_mean_grouped <- function(values, frequencies) {
  total <- 0
  for (i in seq_along(values)) {
    total <- total + values[i]^2 * frequencies[i]
  }
  sqrt(total / sum(frequencies))
}
##MODE VALUE----------------------------------------------------------------
mode_value <- function(values, frequencies) {
  if (length(unique(frequencies)) == 1) {
    return(numeric(0))
  }
  values[frequencies == max(frequencies)]
}
##MODAL INTERVAL--------------------------------------------------------------
modal_interval <- function(lower_limits, upper_limits, absolute_frequencies) {
  heights <- numeric(length(absolute_frequencies))
  for (i in seq_along(absolute_frequencies)) {
    heights[i] <- histogram_height(absolute_frequencies[i], interval_width(lower_limits[i], upper_limits[i]))
  }
  modal_positions <- which(heights == max(heights))
  cbind(lower = lower_limits[modal_positions], upper = upper_limits[modal_positions])
}
##QUANTILE DISCRETE-----------------------------------------------------------
quantile_discrete <- function(data, proportion) {
  sorted_data <- sort(data)
  position <- proportion * length(sorted_data)
  integer_part <- floor(position + 1e-9)
  decimal_part <- position - integer_part
  if (abs(decimal_part) < 1e-9) {
    return((sorted_data[integer_part] + sorted_data[integer_part + 1]) / 2)
  }
  sorted_data[integer_part + 1]
}
##QUANTILE CONTINUOUS---------------------------------------------------------
quantile_continuous <- function(proportion, lower_limits, upper_limits, absolute_frequencies) {
  total_n <- sum(absolute_frequencies)
  cumulative <- cumulative_absolute_frequency(absolute_frequencies)
  target <- proportion * total_n
  for (i in seq_along(absolute_frequencies)) {
    previous_cumulative <- 0
    if (i > 1) {
      previous_cumulative <- cumulative[i - 1]
    }
    if (previous_cumulative <= target && target < cumulative[i]) {
      width <- interval_width(lower_limits[i], upper_limits[i])
      return(lower_limits[i] + (target - previous_cumulative) / absolute_frequencies[i] * width)
    }
  }
}
##MEDIAN VALUE--------------------------------------------------------------
median_value <- function(values, frequencies) {
  order_positions <- order(values)
  values <- values[order_positions]
  frequencies <- frequencies[order_positions]
  cumulative <- cumulative_absolute_frequency(frequencies)
  total_n <- sum(frequencies)
  lower <- values[which(cumulative >= floor((total_n + 1) / 2))[1]]
  upper <- values[which(cumulative >= ceiling((total_n + 1) / 2))[1]]
  (lower + upper) / 2
}
##MEDIAN VALUE--------------------------------------------------------------
median_valueU <- function(data) {
  quantile_discrete(data, 0.5)
}
##QUARTILE, DECILE & PERCENTILE ----------------------------------------------
quartile <- function(data, quartile_number) {
  quantile_discrete(data, quartile_number / 4)
}
decile <- function(data, decile_number) {
  quantile_discrete(data, decile_number / 10)
}
percentile <- function(data, percentile_number) {
  quantile_discrete(data, percentile_number / 100)
}
##RANGES----------------------------------------------------------------------
range_value <- function(data) {
  max(data) - min(data)
}
interquartile_range <- function(data) {
  quartile(data, 3) - quartile(data, 1)
}
intercentile_range <- function(data) {
  percentile(data, 99) - percentile(data, 1)
}
##MEAN DEVIATION CONCERNING AVERAGE --------------------------------------------
mean_deviation_concerning_average <- function(data, parameter) {
  absolute_freqs <- absolute_frequency(data)
  values <- as.numeric(names(absolute_freqs))
  total <- 0
  for (i in seq_along(values)) {
    total <- total + abs(values[i]- parameter) * absolute_freqs[[i]]
  }
  total / length(data)
}
mean_deviation_concerning_average_grouped <- function(values, frequencies, parameter) {
  total <- 0
  for (i in seq_along(values)) {
    total <- total + abs(values[i] - parameter) * frequencies[i]
  }
  total / sum(frequencies)
}
##MEAN DEVIATION---------------------------------------------------------------
mean_deviation <- function(data) {
  mean_deviation_concerning_average(data, mean(data))
}
##MEAN SQUARED ERROR-----------------------------------------------------------
mean_squared_error <- function(data, ref_point) {
  sum((data - ref_point)^2) / length(data)
}
##VARIANCE-------------------------------------------------------------------
variance <- function(data) {
  mean_squared_error(data, mean(data))
}
variance_alternative_form <- function(data) {
  sum(data^2) / length(data) - mean(data)^2
}
##STANDARD DEVIATION---------------------------------------------------------
standard_deviation <- function(data) {
  sqrt(variance(data))
}
##SAMPLE QUASI-VARIANCE-------------------------------------------------------
sample_quasi_variance <- function(data) {
  sample_size <- length(data)
  sample_size / (sample_size - 1) * variance(data)
}
##STANDARDISED VARIABLE-------------------------------------------------------
standardised_variable <- function(values, mean_value, standard_deviation_value) {
  (values - mean_value) / standard_deviation_value
}
##PEARSON COEFFICIENT OF VARIATION---------------------------------------------
pearson_coeff_variation <- function(data) {
  standard_deviation(data) / abs(mean(data))
}
pearson_coeff_variation_percentage <- function(data) {
  pearson_coeff_variation(data) * 100
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
  moment_with_respect_to_point(data, order, 0)
}
##CENTRAL MOMENT----------------------------------------------------------------
central_moment <- function(data, order) {
  moment_with_respect_to_point(data, order, mean(data))
}
central_moment_3_from_ordinary_moments <- function(data) {
  mean_value <- mean(data)
  ordinary_moment(data, 3) - 3 * ordinary_moment(data, 2) * mean_value + 2 * mean_value^3
}
central_moment_4_from_ordinary_moments <- function(data) {
  mean_value <- mean(data)
  ordinary_moment(data, 4) - 4 * ordinary_moment(data, 3) * mean_value + 6 * ordinary_moment(data, 2) * mean_value^2 - 3 * mean_value^4
}
##PEARSON SKEWNESS COEFFICIENT-----------------------------------------------
pearson_skewness_coefficient <- function(data) {
  absolute_freqs <- absolute_frequency(data)
  (mean(data) - mode_value(as.numeric(names(absolute_freqs)), absolute_freqs)) / standard_deviation(data)
}
# If > 0, positive skew, if = 0 symmetric, if < 0 negative skew

#Symmetry means that Mean=Median=Mode

##FISHER SKEWNESS COEFFICIENT--------------------------------------------------
fisher_skewness_coefficient <- function(data) {
  central_moment(data, 3) / standard_deviation(data)^3
}
# If > 0, positive skew, if = 0 symmetric, if < 0 negative skew
##FISHER KURTOSIS COEFFICIENT-----------------------------------------------
fisher_kurtosis_coefficient <- function(data) {
  central_moment(data, 4) / standard_deviation(data)^4 - 3
}
# If < 0, Platykurtic (less peaked than the normal), if = 0, Mesokurtic(same peakedness), if > 0, Leptokurtic(more peaked than the normal)
