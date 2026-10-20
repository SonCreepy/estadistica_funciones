#T * E * C * A

#Moving average formula
#Works just for 3,4,12. If any other order, just use the stats:filter function
moving_avg <- function(x, order) { #Works just for 3,4,12. If any other order, just use the stats:filter function
  if (order == 3) {
    w <- c(1, 1, 1) / 3
  } else {
    # even orders (4 and 12): centered MA with half weights at the ends
    w <- c(0.5, rep(1, order - 1), 0.5) / order
  }

  stats::filter(x, w)
}

#Time Series function
#Start must be a c(year,1). plot(uk) to see the spiky graph
time_series <- function(xx, start, frequency) { 
  uk <- ts( xx , start , frequency)
  uk
}

# Trend component , to calculate do the moving average of uk in the order of frequency
# plot (uk)
# lines (trend, col=2)


# ##############################################
# Multiplicative Decomposition
# ##############################################

# Seasonal (E) * Random Component (A) = X/TC :
EA <- function(uk, trend) {
  Seasonal_Random <- uk/trend
}

#Seasonal variation index to remove random component
# Unnormalized Seasonal Component
Unnormalized_Seasonal_Component <- function(EA, ncol) { #ncol is columns of EA
  seasonalM <- colMeans ( matrix ( EA , ncol, byrow = T ), na.rm= T ) 
}

# Normalize the Seasonal Component
Normalized_Seasonal_Component <- function(seasonalM) { #ncol is columns of EA
  seasonalN <- seasonalM/mean(seasonalM)
}

# Random Component
Random_Component <- function(EA, seasonalN) {
  randomC <- EA/seasonalN
}


# Deseasonalized Series
Deseasonalize <- function(uk, seasonalN) { #To graph it, plot(uk), lines (DeseasonalizedN, col=4)
  DeseasonalizedN <- uk/seasonalN
}

# ###############################################
# Calculate a linear regression of the deseasonalized 
# ###############################################
# This puts a vector 1:n in xx , where n is the length of uk
trend_line <- function(desest, start, frequency) { #start like c(1970,1) and frequency 4. desest is deseasonalized series
  # Vector 1:n, where n is the length of the series
  xx <- seq_along(desest)

  # Linear regression
  model <- lm(desest ~ xx)

  # Predicted values of the line, as a time series
  regL <- ts(predict.lm(model), start = start, frequency = frequency)

  # Plot the deseasonalized series with the line
  plot(desest)
  lines(regL, col = 2)

 # invisible(list(model = model, trend = regL)) To not print the list on console
 # print(coef(model)[1], digits=x) for the intercept (vertical axis value when xx=0) ; or [2] for slope of the model
}


# AUTOCORRELATION 
autoCorrel <- function (data , n ) {
# Remove the last n data points
xx1 <- head (data , -n )
# Remove the first n data points
xx2 <- tail (data , -n )
# correlation
sqrt ( summary (lm( xx1 ~ xx2 ))$r.squared )
}

