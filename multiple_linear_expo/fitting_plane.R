x <- c(0,1,0,-1,0,1,1,2)
y<- c(0,1,1,1,3,2,2,0)
z<-c(2,3,4,5,7,4,5,0)

sum_x <- sum(x)
sum_y <- sum(y)
sum_z <- sum(z)
N<- length(x)

sum_x2 <- sum(x^2)
sum_y2 <- sum(y^2)

sum_x_y <- sum(x*y)
sumz_y <- sum(z*y)
sumz_x <- sum(z*x)

# Obtain the Sum of Squared Errors
mat <- matrix(
  c(N,sum_x,sum_y,        sum_z,
    sum_x,sum_x2,sum_x_y, sumz_x,
    sum_y,sum_x_y,sum_y2, sumz_y),
    nrow = 3,byrow = TRUE
)
A <- mat[,1:3]
B<- mat[,4]


a<-solve(A,B)[1]
b<-solve(A,B)[2]
c<-solve(A,B)[3]

z. <- function(x,y){
  return(a + b*x + c*y)
}

sse <- sum((z - z.(x,y))^2)
mse<-sse/N
sn_y <- sum_y2/N -(sum_y/N)^2

cat("SSE= ", sse)
cat("MSE=", mse )
R_2<- 1-(mse/sn_y)
cat("R^2 = ",R_2)