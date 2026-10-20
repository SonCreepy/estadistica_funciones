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
##CUMULATIVE RELATIVE FREQUENCY-----------------------------------------------
cumulative_relative_frequency <- function(cumulative_absolute_freq, total_n) {
  cumulative_absolute_freq / total_n
}
##ARITHMETIC MEAN------------------------------------------------------------
mean(data)
#MEAN WITH FREQUENCIES
arithmetic_mean <- function(data, freqs) {
  r <- 0
  for(i in seq_along(data)){
    r <- r + data[i] * freqs[i];
  }
  r
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
data <- c(1500, 750, 1000, 500, 250)

names(data) <- c("Chemistry", "Mathematics", "Physiscs", "Biology", "Geology")

barplot(data,
        main = "Students enrolled by Section",
        ylab = "Number of Students",
        col  =  "steelblue")

pie(data,
    main = "Students Enrolled by Section",
    col = c("red", "blue", "green", "purple", "grey"))


##QUADRATIC MEAN-------------------------------------------------------
quadratic_mean <- function(data) {
  (sqrt(sum((data^2)) / length(data)))
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
mean_deviation_average <- function(data, parameter) {
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
  mean_deviation_average(data, unname(mean(data)))
}
##MEAN SQUARED ERROR-----------------------------------------------------------
mean_squared_error <- function(data, ref_point) {
  sum((data - ref_point)^2) / length(data)
}
##VARIANCE-------------------------------------------------------------------
variance <- function(data) { # var() from R is quasi variance
  n <- length(data)
  (var(data) * ((n - 1) / n))
}
##STANDARD DEVIATION---------------------------------------------------------
standard_deviation <- function(data) {
  sqrt(variance(data))
}
##PEARSON COEFFICIENT OF VARIATION---------------------------------------------
pearson_coeff_variation <- function(data) {
  standard_deviation((data)) / abs(mean(data))
}
##MOMENT RESPECT TO POINT-------------------------------------------------------
# If point == 0, calculating ORDINARY MOMENT
# If point == mean(data) calculating CENTRAL MOMENT
moment_with_respect_to_point <- function(data, order, point) {
  absolute_freqs <- absolute_frequency(data)
  values <- as.numeric(names(absolute_freqs))
  total <- 0
  for (i in seq_along(values)) {
    total <- total + absolute_freqs[[i]] * (values[i] - point)^order
  }
  total / length(data)
}
##PEARSON SKEWNESS COEFFICIENT-----------------------------------------------
pearson_skewness_coefficient <- function(data) {
  (mean(data) - mode_value(data)) / standard_deviation(data)
}
##FISHER SKEWNESS COEFFICIENT--------------------------------------------------
fisher_skewness_coefficient <- function(data) {
  moment_with_respect_to_point(data,3 , mean(data)) / standard_deviation(data)^3
}
##FISHER KURTOSIS COEFFICIENT-----------------------------------------------
fisher_kurtosis_coefficient <- function(data) {
  moment_with_respect_to_point(data,4, mean(data)) /
  standard_deviation(data)^4 - 3
}