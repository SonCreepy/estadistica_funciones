
##PIE CHART AND BAR PLOT----------------------------------
data <- c(1500, 750, 1000, 500, 250)

names(data) <- c("Chemistry", "Mathematics", "Physiscs", "Biology", "Geology")

barplot(data,
        main = "Students enrolled by Section",
        ylab = "Number of Students",
        col ="steelblue")

pie(data,
    main = "Students Enrolled by Section",
    col = c("red", "blue", "green", "purple", "grey"))



##ROOTED MEAN SQUARE-------------------------------------
data <- c(2,3,1,0,4,3)

rms <- sqrt(sum(data^2)/length(data))
print(rms)

##MODE------------------------------------------------

data <- c(7,11,11,8,12,9,6,6)
mode <- function(x) {
  table <- table(x)
  as.numeric(names(table)[table == max(table)])
}
mode(data)

##MEDIAN--------------------------------------------------

data <-c(11,11,16,17,25)
median(data)
data<-c(1,4,8,8,10,16,16,19)
median(data)

#QUANTILES---------WRONG----------------------------------

data <- c(2, 5, 3, 4, 7, 0, 11, 2, 3, 8)


quartiles <- quantile(data, probs = c(0.25, 0.50, 0.75))
print("quartiles:")
print(quartiles)

# (P37 y P68)
percentiles <- quantile(data, probs = c(0.37, 0.68))
print("Percentiles 37 y 68:")
print(percentiles)

##MEAN DEVIATION AND MEAN SQUARED ERROR----------------------------

# 1. define data and length
x <- c(5, 2, 3, 3, 3, 5, 7)
n <- length(x)

# 2. mean and median
mean <- mean(x)
median <- median(x)

# 3. (Mean Deviation)
md_mean <- sum(abs(x - mean)) / n
md_median <- sum(abs(x - median)) / n

# 4. (Mean Squared Error)
mse_mean <- sum((x - mean)^2) / n
mse_median <- sum((x - median)^2) / n

# 5. PRINT
print(paste("MD respecto a la media:", round(md_mean, 4)))
print(paste("MD respecto a la mediana:", round(md_median, 4)))
print(paste("MSE respecto a la media:", round(mse_mean, 4)))
print(paste("MSE respecto a la mediana:", round(mse_median, 4)))

##EXERCISE TO TRY--------------------------------------------
# 1. Define the variables based on the given table
predicted <- c(0, 0, 0, 1, 1, 1, 3, 3, 3)
actual <- c(0, 1, 3, 0, 1, 3, 0, 1, 3)
frequency <- c(6, 3, 2, 3, 5, 1, 1, 3, 7)

# 2. Calculate total sample size (N)
total_n <- sum(frequency)

# 3. Compute the estimation error variable

# 4. Calculate the arithmetic mean of the error
error_mean <- sum(error * frequency) / total_n

# 5. Compute the Quadratic Mean (Root Mean Square)
quadratic_mean <- sqrt(sum((error^2) * frequency) / total_n)

# 6. Compute the Mean Deviation (Mean Absolute Deviation from the mean)
mean_deviation <- sum(abs(error - error_mean) * frequency) / total_n

# 7. Compute the Mean Squared Deviation (Population Variance)
mean_squared_deviation <- sum(((error - error_mean)^2) * frequency) / total_n

# 8. Print the formatted output
cat("Quadratic Mean:", round(quadratic_mean, 4), "\n")
cat("Mean Deviation:", round(mean_deviation, 4), "\n")
cat("Mean Squared Deviation:", round(mean_squared_deviation, 4), "\n")

##---------STANDARD DEVIATION && VARIANCE--------------------------
# 1. Define the numerical lists
list_a <- c(12, 10, 9, 9, 10)
list_b <- c(5, 10, 16, 15, 4)

# 2. Create a custom function for the population standard deviation
  sd <- function(x) {
  n <- length(x)
  variance <- sum((x - mean(x))^2) / n
  sqrt(variance)
}

# 3. Calculate the standard deviation for both lists
sd_a <- sd(list_a)
sd_b <- sd(list_b)

# 4. Print the results
cat("Standard Deviation of List A:", round(sd_a, 4), "\n")
cat("Standard Deviation of List B:", round(sd_b, 4), "\n")