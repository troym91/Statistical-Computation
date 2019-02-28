rm(list=ls())
.Machine$integer.max
.Machine$double.max
.Machine$double.max.exp
.Machine$double.min
.Machine$double.xmax
2^1023
2^1024
-2**1024
.Machine$double.xmin
2^-1022

2^1024 * 2^-1024
2^1078 * 2^-1078
2^1024 / 2^1024
2^1024 - 2^1024

2^-52
.Machine$double.eps ##machine epsilon

0.6-0.5 ## 사용자가 보기 편하게 인쇄.
0.6-0.5 == 0.1 ## R내에서 2진법 연산이 실행되면 0.1이 아니다.

print(0.6-0.5, 16)
print(0.1, 16)

0.1+0.5 == 0.6 ## 모든 2진법 연산이 어그러지는 것은 아님.

print(tan(pi/4),16)
print(tan(pi/4 + pi),16)
print(tan(pi/4 + 2*pi),16)
print(tan(pi/4+ 3*pi),16)

## 정리: 컴퓨터 수학과 진짜 수학은 다르다. 컴퓨터 수치를 다룰 때는 조심해야 한다.

#연습문제1 - pass.

factorial(100)
factorial(200) # 어떻게 200C100을 계산할 수 있을까?
# 200C100 의 계산
c <- 1
for (k in 1:100){
  c <- c*((k+100)/k)
}
c
choose(200,100)

# Consider exp(x) where x is an elmt of [-1,1].
# Taylor expansion of up to N-th term.
exponential <- function(x){
  sum <- 1; temp <- 1; k <- 1
  while(abs(temp) > .Machine$double.eps){
    temp <- temp*(x/k)
    sum <- sum + temp
    k <- k+1
  }
  return(sum)
}
print(exponential(1),16)
print(exp(1),16)

#exponential 함수의 보완
exp.1 <- function(x){
  sum <- 1
  temp <- 1
  for (k in 1:100){
    if (abs(temp) < .Machine$double.eps) return(sum)
    temp <- temp * (x/k)
    sum <- sum + temp
  }
  return("Computation is incomplete within the limit")
}

print(exp.1(1), 16)
print(exp(1), 16)
print(exp.1(100), 16)
print(exp.1(1)^100, 16)

#연습문제2.
sine <- function(x){
  sum <- x
  temp <- x
  for (k in 2:100){
    if (abs(temp) < .Machine$double.eps) return(sum)
    temp <- temp * (-(x^2)/((2*k-1)*(2*k-2)))
    sum <- sum + temp
  }
  return("Computation is incomplete within the limit")
}
sine(pi/4)
sin(pi/4)

##처리시간(processing time)
set.seed(1)
A <- matrix(runif(1000*100), nrow = 1000, ncol=100)
## 목표: A의 행별 합계를 구하라.
tic <- proc.time()
for (k in 1:100){
  r <- rep(0,1000)
  for (i in 1:1000)
    for (j in 1:100) r[i] = r[i] + A[i,j]
}
r
toc <- proc.time()
toc - tic

x <- cbind(x1 = 3, x2 = c(4:1, 2:5))
dimnames(x)[[1]] <- letters[1:8]
apply(x, 2, mean, trim = .2)


?apply
tic <- proc.time()
r <- apply(A,1,sum) ##A에 대해 row별 sum을 해라. 만약 1 대신 2면 column별 sum.
r
toc <- proc.time()
toc - tic

tic <- proc.time()
r <- rep(0,100)
for (j in 1:100)
  for (i in 1:1000) r[j] <- r[j] + A[i,j]
r
toc <- proc.time()
toc - tic


tic <- proc.time()
r <- apply(A,2,sum) ##A에 대해 row별 sum을 해라. 만약 1 대신 2면 column별 sum.
r
toc <- proc.time()
toc - tic

install.packages("Rmpfr")
library(Rmpfr)
?Rmpfr
