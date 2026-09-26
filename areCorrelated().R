areCorrelated <- function(data_x,data_y,freqs,N){
  return(areIndependent(data_x,data_y,freqs) || Covariance(data_x,data_y,table,N))
}