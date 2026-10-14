areIndependent <- function(data_x,data_y,freqs,tol = 1e-9){
  i <-1
  indep <- TRUE
  while(indep && i <= length(data_x)){
    j<-1 
    while(indep && j <= length(data_x)){
      if(abs(freqs[i,j] - (data_x[i]* data_y[j]))>tol)
        indep <- FALSE;
      j<- j+1
    }
    i<-i+1
  }
  return (indep)
}