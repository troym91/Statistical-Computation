rm(list=ls())
fibbonacci <- function(n, x1, x2){
  temp.1 <- x1
  temp.2 <- x2
  for (k in 3:n){
    temp.3 <- temp.1 + temp.2
    temp.1 <- temp.2
    temp.2 <- temp.3
  }
  return(temp.3)
}
fibbonacci(10,1,1)
fibbonacci(6,1,1)
fibbonacci(20,1,1)

n <- 1000
Fi <- rep(0,n)
Fi[1] <- 1
Fi[2] <- 1
for (k in 3:n) Fi[k] <- Fi[k-1] + Fi[k-2]

table(4018 %/% 10^floor(log(4018,10)))
table(Fi %/% 10^floor(log(Fi,10))) ##Benford's Law

Fi[78]
Fi[79] %% 2
Fi[80] %% 2
Fi[81] %% 2

addmargins(table(Fi[1:78] %% 2))
addmargins(table(Fi[1:81] %% 2)) ## Why???


## 선형합동수열
# x(n) = 21*x(n-1) + 31 (mod 100) (pseudo-random number generator)
x <- rep(0,101)
x[1] <- 0
for (i in 2:101){
  x[i] <- (21*x[i-1] + 31) %% 100
}
x ## 주기가 100인 수열임을 확인할 수 있다.

install.packages("randtoolbox")
library(randtoolbox)
congruRand(10)
?congruRand


#ex1. root of x=cosx x(1) = 0.5
n <- 50
x <- rep(0,n)
x[1] <- 0.5
diff <- 0.5
eps <- .0001/2
k <- 1
while(diff > eps){
  k <- k+1
  x[k] <- cos(x[k-1])
  diff <- abs(x[k] - x[k-1])
}
k
x[k]
cos(x[k])

##ex2. root of -x^3 + 2*x^2 - 2*x +2 = 0 (between 0 and 2.5) start at x=0.5
n <- 50
x <- rep(0,n)
x[1] <- 5
ftn <- function(x){-0.5*x^3 + x^2 + 1}
diff <- x[1] - ftn(x[1])
eps <- 1e-4/2
k <- 1
while(abs(diff) > eps){
  k <- k+1
  x[k] <- ftn(x[k-1])
  diff <- x[k] - x[k-1]
}
k
round(x[k],4)

##ex2. root of x=sqrt(0.5*x^3 + x + 1). start at x= 0.5.
n <- 50
x <- rep(0,n)
x[1] <- 0.5
ftn2 <- function(x){sqrt(0.5*x^3 + x - 1)}
diff <- x[1] - ftn(x[1])
eps <- 1e-4/2
k <- 1
while(abs(diff) > eps){
  k <- k+1
  x[k] <- ftn(x[k-1])
  diff <- x[k] - x[k-1]
}
k
round(x[k],4)
x1 <- seq(0.8,2.5,0.01)
plot(x,ftn2(x), xlim = c(0.5,1.2), ylim = c(0.5, 1.2), xlab ="previous term", ylab = "new term")
abline(a=0,b=1)
par(new= T)
plot(x1, ftn2(x1), type='l', col = "blue")
ftn2(x1)
?curve


n <- 10
x <- runif(n)
round(x,2)
for (i in 1:(n-1)){
  for (j in (i+1):n){
    if(x[i] > x[j]){
      temp <- x[i]
      x[i] <- x[j]
      x[j] <- temp
    }
  }
}
round(x,2)

n <- 10
x <- runif(n)
round(x,2)
continue <- 1



###연습문제
n <- 1e4
Fib = rep(0,n)
Fib[1] <- 1
Fib[2] <- 1
for (i in 3:n){
  Fib[i] <- Fib[i-1] + Fib[i-2]
}
Fib

ratio.Fib = rep(0,(n-1))
for (i in 1:(n-1)){
  ratio.Fib[i] = Fib[i+1]/Fib[i]
}
ratio.Fib

conv = list()
eps <- 1e-7
k <- 1
diff <- 1
while (abs(diff) > eps){
  diff <- ratio.Fib[k+1] - ratio.Fib[k]
  k <- k + 1
}
k
conv.ratio <- ratio.Fib[k]
conv.ratio

fun <- function(x){-0.5*x^3 + x^2 + 1}
x <- seq(-1,4,0.01)
fun(x)
plot(x,fun(x), lty = 2, xlim = c(-1,3), ylim= c(-5,5))
abline(0,1)

x <- round(runif(10),3)
x
order(x)
rank1 <- function(x){
  N = length(x)
  rank_list = rep(0,N)
  for (i in 1:N){
    rank_list[i] <- sum(x-x[i] <= 0)
  }
  return(rank_list)
}
rank1(x) == rank(x)

n1 = 50
p50 = rep(0, n1)
p50[1] = 1; p50[2] = 1/3
for (i in 3:n1){
  p50[i] = (10/3)*p50[i-1] - p50[i-2]
}
p50
par(mfrow=c(1,2))
one_to_fifty = seq(1,50,1)
plot(one_to_fifty, p50, pch= 19)

n2 = 100
p100 = rep(0, n2)
p100[1] = 1; p100[2] = 1/3
for (i in 3:n2){
  p100[i] = (10/3)*p100[i-1] - p100[i-2]
}
p100
one_to_hundred = seq(1,100,1)
plot(one_to_hundred, p100, pch =2 )
p100 > 1
p100[p100>1]
p50[p50>1]

