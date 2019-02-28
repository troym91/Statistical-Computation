rm(list=ls())

solve(A)
# Eigenvalue
A <- matrix(c(1,0.4,0.2,0.4,1,0.4,0.2,0.4,1), byrow = T, nrow =3)
x0 <- as.vector(c(1,0,0)) # initial value of vector
diff <- 1
eps <- 0.0001
count <- 0 
while(diff > eps){
  x1 <- A %*% x0
  lambda1 <- sqrt(sum(x1*x1)/sum(x0*x0))
  x1 <- x1/sqrt(sum(x1*x1))
  diff <- sum((x1-x0)*(x1-x0))
  x0 <- x1
  count <- count + 1
}
round(x0,4)
round(lambda1,4)
count

# 2nd e-value & e-vector
A <- A - lambda1 * x0 %*% t(x0)

x0 <- as.vector(c(1,0,0)) # initial value of vector
diff <- 1
eps <- 0.0001
count <- 0 
while(diff > eps){
  x1 <- A %*% x0
  lambda2 <- sqrt(sum(x1*x1)/sum(x0*x0))
  x1 <- x1/sqrt(sum(x1*x1))
  diff <- sum((x1-x0)*(x1-x0))
  x0 <- x1
  count <- count + 1
}
round(x0,4)
round(lambda2,4)

# third e-value,vector.
A <- A - lambda2 * x0 %*% t(x0)

x0 <- as.vector(c(1,0,0)) # initial value of vector
diff <- 1
eps <- 0.0001
count <- 0 
while(diff > eps){
  x1 <- A %*% x0
  lambda3 <- sqrt(sum(x1*x1)/sum(x0*x0))
  x1 <- x1/sqrt(sum(x1*x1))
  diff <- sum((x1-x0)*(x1-x0))
  x0 <- x1
  count <- count + 1
}
round(x0,4)
round(lambda3,4)

e.values <- c(lambda1,lambda2,lambda3)
A <- matrix(c(1,0.4,0.2,0.4,1,0.4,0.2,0.4,1), byrow = T, nrow =3)
A
e.A <- eigen(A)
round(e.A$values,4)
round(e.values,4)


##원점 기준으로 k*(pi/3)만큼 회전.
set.seed(123)
x <- cbind(rnorm(100,2,0.5),rnorm(100,3,0.5))
x 
rotate <- function(x, theta){
  A <- matrix(c(cos(theta), -sin(theta), sin(theta), cos(theta)),
              byrow = T, nrow = 2 )
  return(x %*% t(A))
}
plot(x, xlim=c(-5,5), ylim=c(-5,5), xlab="", ylab="", col=rainbow(100) )
for (k in 1:5){
  x.prime <- rotate(x, k*pi/3)
  par(new = T)
  plot(x.prime, xlim=c(-5,5), ylim=c(-5,5), xlab="", ylab="", col = rainbow(100))
}
par(mfrow=c(1,1))


##원점과 (-1,2)를 연결하는 직선을 축으로 한 반전
reflect <- function(x, c){
  K = ((c[1]^2 + c[2]^2)^(-1))*matrix(c(c[1]^2-c[2]^2, 2*c[1]*c[2], 2*c[1]*c[2], c[2]^2-c[1]^2),
                                      byrow = T, nrow = 2)
  return(x %*% t(K))
}
plot(x, xlim=c(-5,5), ylim=c(-5,5), xlab="", ylab="", col=rainbow(100))
par(new = T)
plot(reflect(x,c(-1,2)), xlim=c(-5,5), ylim=c(-5,5), xlab="", ylab="", col=rainbow(100),
     main="Reflection")
abline(0,-2)

solve
# projection
project <- function(x,v){
  H <- v %*% solve(t(v) %*% v) %*% t(v)
  return (x %*% H)
}
plot(x, xlim=c(-5,5), ylim=c(-5,5), xlab="", ylab="", col=rainbow(100))
par(new = T)
v <- c(-1,2)
x.prime <- project(x,v)
plot(x.prime, xlim=c(-5,5), ylim=c(-5,5), xlab="", ylab="", col=rainbow(100), main ="Projection")
abline(0,v[2]/v[1])
par(mfrow=c(1,2))

A
svd(A)
det(A)

B = matrix(c(2,-3,1,4), byrow = T, nrow = 2)
det(B)
image(A)
image(B)
reflect(x,as.vector(c(-1,2)))
c(-1,2)[2]

par(mfrow=c(1,2))
theta <- seq(0,2*pi,pi/180)
r <- 1 - cos(theta)
x <- r * cos(theta); y <- r * sin(theta)
plot(x, y, xlim=c(-2.5, 2.5), ylim=c(-2.5, 2.5), xlab = "", ylab = "", type = "l", main = "Original")

X <- cbind(x,y)
X
X_minus90 = rotate(X, -pi/2)
X_plus30 = rotate(X, pi/6)
X_plus150 = rotate(X, 5/6*pi)
plot(X_minus90, xlim=c(-2.5, 2.5), ylim=c(-2.5, 2.5), xlab = "", ylab = "", type = "l", col = 'red', main = "Rotated 3 Versions")
par(new = T)
plot(X_plus30, xlim=c(-2.5, 2.5), ylim=c(-2.5, 2.5), xlab = "", ylab = "", type = "l", col = 'blue')
par(new = T)
plot(X_plus150, xlim=c(-2.5, 2.5), ylim=c(-2.5, 2.5), xlab = "", ylab = "", type = "l", col = 'green')

