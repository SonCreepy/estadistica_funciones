##ABSOLUTE FREQUENCY-------------------------------------------------------
  absolute_frequency <- function(data) {
  table(data)
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
