areIndependent <- function(data_x,data_y,freqs){
  i <-0
  indep <- TRUE
  while(indep && i < length(data_x)){
    while(indep && i < length(data_x)){
      if(freqs[i][j] != data_x[i]* data_y[j])
        indep = FALSE;
    }
  }
  return (indep)
}