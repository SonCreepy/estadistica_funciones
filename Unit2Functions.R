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

