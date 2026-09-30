x<-c(0,1,2,3,6)
y<- c(7,5,4,3.5,3)
N<- length(x)
sum_x <- sum(x)
x2 <- (x^2)
sum_y <- sum(y)
y2 <- (y^2)


Y<- log(y)
sum_lny <- sum(Y)
xY<-x*Y

mat <- matrix(
  c(N,sum_x,        sum(Y),
    sum_x,sum(x2),     sum(xY)),
  nrow = 2,byrow = TRUE
)
A <- mat[,1:2]
B<- mat[,3]


a<-solve(A,B)[1]
b<-solve(A,B)[2]

sse <- sum((y-y_(x))^2)
mse<-sse/N
sn_y <- sum(y2)/N -(sum_y/N)^2

cat("SSE= ", sse)
cat("MSE=", mse )
R_2<- 1-(mse/sn_y)
cat("R^2 = ",R_2)

y_<-function(x){
  return(exp(a)*exp(b*x))
}