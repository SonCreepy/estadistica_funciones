#Copyright Marco Toscano 2026©. Do not distribute

# DISCLAIMER: If using relative frequencies, N=1 !!

Center_Gravity <- function(row, col, table, N) {
  return(c(x_meanF(row, table, N), y_meanF(col, table, N)))
}



x_meanF <- function(row, table, N) {
  return(sum(row*rowSums(table))/N)
}




y_meanF <- function(col, table, N) {
  return(sum(col*colSums(table))/N)
}




mom_ord2 <- function(row, col, table, r, s, a,b) {
  N <- sum(table)
  sum <- 0
  for(i in seq_along(row)) {
    for(j in seq_along(col)) {
      sum <- sum + (row[i]-a)^r * (col[j]-b)^s * table[i, j]/N
    }
    
  }
  
  return (sum)
}

#row -> vector of X,
#col -> vector of Y,
#table -> matrix of frequencies
#r -> exponent of x factor
#s -> exponent of y factor
#a -> reference point of x factor
#b -> reference point of y factor

#For ordinary moments, a,b=0,0, for central moments, a,b=x_mean,y_mean


Covariance <- function(row, col, table, N) {
  m11 <- mom_ord2(row, col, table, 1, 1, 0, 0)
  
  return(m11 - x_meanF(row, table, N)*y_meanF(col, table, N))
}


SecondOrd_momentX <- function(row, col, table, N) {
  return(mom_ord2(row,col,table,2,0,0,0) - (x_meanF(row,table,N)^2))
}  


SecondOrd_momentY <- function(row, col, table, N) {
  return(mom_ord2(row,col,table,0,2,0,0) - (y_meanF(col,table,N)^2))
}