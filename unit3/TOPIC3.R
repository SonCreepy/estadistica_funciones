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

library ( tidyverse )
xx <- c (178.2 , 153.2 , 185.9 , 163.6 ,
196.3 , 156.9 , 197.9 , 166.4 ,
197.3 , 159.7 , 202.6 , 175.6 ,
209.5 , 169.5 , 202.4 , 179.8 ,
200.0 , 168.6 , 216.1 , 178.5
)
# Reading the data
# Indicate the frequency of the seasonal component
uk <- ts( xx , start = c(1980 , 1) , frequency = 4)
uk
plot ( uk )

# Trend component , using a moving average of order 4
tend <- stats :: filter ( uk , c(.5 , 1 , 1 , 1 , .5) / 4)
tend
# Plot the series with the trend in red
plot ( uk )
lines ( tend , col = 2)
# ##############################################
# Multiplicative Decomposition
# ##############################################
# Seasonal (E) * Random Component (A) = X/TC :
est_aleM <- uk / tend
est_aleM

# Unnormalized Seasonal Component
est1M <- colMeans ( matrix ( est_aleM , ncol =4 , byrow = T ) ,
na.rm= T )
est1M

# Normalize the Seasonal Component
estM <- est1M / mean ( est1M )
estM
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
