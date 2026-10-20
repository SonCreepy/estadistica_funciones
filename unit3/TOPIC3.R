#T * E * C * A

#Moving average formula
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
time_series <- function(xx, start, frequency) { #Start must be a c(year,1). plot(uk) to see the spiky graph
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
  seasonalM <- colMeans ( matrix ( est_aleM , ncol, byrow = T ), na.rm= T ) 
}

# Normalize the Seasonal Component
Normalized_Seasonal_Component <- function(seasonalM) { #ncol is columns of EA
  seasonalN <- seasonalM/sum(seasonalM)
}

# Random Component
aleM <- est_aleM / estM
aleM

# Deseasonalized Series
desestM <- uk / estM
desestM
# Graph of the Deseasonalized Series
plot ( uk )
lines ( desestM , col =4)

# ###############################################
# Calculate a linear regression of the deseasonalized # ###############################################
# This puts a vector 1:n in xx , where n is the
length of uk
xx <- seq _ along ( uk )
# Linear regression
model <- lm( desestM ~ xx )
# Calculate the predicted values of the line
regL <- ts( predict .lm( model ) , start =c(1969 , 1) ,
frequency =4)
# Plot the deseasonalized series with the line
plot ( desestM )
lines ( regL , col =2)

data <- c(1.2 , 1.4 , 2.0 , 1.23 , 1.37 , 2.07 ,
1.25 , 1.47 , 2.09)

# AUTOCORRELATION 
frequency n
autoCorrel <- function (data , n ) {
# Remove the last n data points
xx1 <- head (data , -n )
# Remove the first n data points
xx2 <- tail (data , -n )
# correlation
sqrt ( summary (lm( xx1 ~ xx2 ))$r . squared )
}
autoCorrel ( xx , 3)
