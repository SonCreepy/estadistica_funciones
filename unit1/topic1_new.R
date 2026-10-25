#REPETITIONS -> If you get unique values and its frequency, this to transform into repeated values array
rep(data,freq)

##ABSOLUTE FREQUENCY-------------------------------------------------------
absolute_frequency <- function(data) {
  table(factor(data, levels = unique(data[!is.na(data)])))
}
frequency_array <- function(data) {
  frequencies <- absolute_frequency(data)
  as.numeric(frequencies)
}
##RELATIVE FREQUENCY-------------------------------------------------------
relative_frequency <- function(absolute_freq, total_n) {
  absolute_freq / total_n
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
##ARITHMETIC MEAN---------------------------------------------------------
arithmetic_mean <- function(values, frequencies) { # If want to use weighted mean, pass frequencies as weights
  total <- 0
  for (i in seq_along(values)) {
    total <- total + values[i] * frequencies[i]
  }
  total / sum(frequencies)
}
##PIE CHART AND BAR PLOT----------------------------------
#barplot(data,main="TITLE",ylab="COLUMNS",col)
#pie(data, main, col=c(1,2,3))

##QUADRATIC MEAN GROUPED-----------------------------------------------------
quadratic_mean <- function(data ,frequencies) {
  sqrt(sum(data^2 * frequencies) / sum(frequencies))
}
##MODE VALUE----------------------------------------------------------------
mode_value <- function(values) {
  freq_table <- table(values)
  mode_value <- as.numeric(names(freq_table)[which.max(freq_table)])
  return(mode_value)
}
#QUANTILE AND DECILE -------------------------------------------------------
quantile_value <- function(values, quantile) {
  quantile(values, probs = quantile)
}
decile <- function(data, decile_number) {
  quantile_discrete(data, decile_number / 10)
}
percentile <- function(data, percentile_number) {
  quantile_discrete(data, percentile_number / 100)
}
# Use IQR native function 
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
  quantile_value(data, 0.5)
}
#MEAN DEVIATION CONCERNING AVERAGE --------------------------------------------
mean_deviation_concerning_average <- function(values, frequencies, parameter) {
  total <- 0
  for (i in seq_along(values)) {
    total <- total + abs(values[i] - parameter) * frequencies[i]
  }
  total / sum(frequencies)
}
##MEAN SQUARED ERROR-----------------------------------------------------------
mean_squared_error <- function(data, ref_point) {
  sum((data - ref_point)^2) / length(data)
}
##VARIANCE-------------------------------------------------------------------
variance <- function(data) {
  mean_squared_error(data, mean(data))
}
##STANDARD DEVIATION---------------------------------------------------------
standard_deviation <- function(data) {
  sqrt(variance(data))
}##PEARSON COEFFICIENT OF VARIATION---------------------------------------------
##STANDARDISED VARIABLE-------------------------------------------------------
standardised_variable <- function(values, mean_value, standard_deviation_value) {
  (values - mean_value) / standard_deviation_value
}
pearson_coeff_variation <- function(data) {
  standard_deviation(data) / abs(mean(data))
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
##PEARSON SKEWNESS COEFFICIENT-----------------------------------------------
pearson_skewness_coefficient <- function(data) {
  (mean(data) - mode_value(data)) / standard_deviation(data)
}