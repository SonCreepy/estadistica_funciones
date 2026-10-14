x<- c(5,5,5,5,10,10,10,10,15,15,15,15,20,20,20,20)
y<- c(1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4)
z<- c(28,30,48,74,29,50,57,42,20,24,31,47,9,18,22,31)

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

cat("z =", a, "+", b, "* x +", c, "* y\n")
#--------------------------------------------
x<- c(5,5,5,5,10,10,10,10,15,15,15,15,20,20,20,20)
y<- c(1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4)
z<- c(28,30,48,74,29,50,57,42,20,24,31,47,9,18,22,31)

M<-cbind(rep(1,length(x)),x,y)
Y<- matrix(z,ncol = 1)
A<- solve(t(M)%*%M)%*% t(M)%*% Y

cat(sprintf("Model: z = %.2f + (%.2f)*x + (%.2f)*y\n", A[1], A[2], A[3]))
