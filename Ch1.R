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

0.6-0.5 ## ����ڰ� ���� ���ϰ� �μ�.
0.6-0.5 == 0.1 ## R������ 2���� ������ ����Ǹ� 0.1�� �ƴϴ�.

print(0.6-0.5, 16)
print(0.1, 16)

0.1+0.5 == 0.6 ## ��� 2���� ������ ��׷����� ���� �ƴ�.

print(tan(pi/4),16)
print(tan(pi/4 + pi),16)
print(tan(pi/4 + 2*pi),16)
print(tan(pi/4+ 3*pi),16)

## ����: ��ǻ�� ���а� ��¥ ������ �ٸ���. ��ǻ�� ��ġ�� �ٷ� ���� �����ؾ� �Ѵ�.

#��������1 - pass.

factorial(100)
factorial(200) # ��� 200C100�� ����� �� ������?
# 200C100 �� ���
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

#exponential �Լ��� ����
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

#��������2.
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

##ó���ð�(processing time)
set.seed(1)
A <- matrix(runif(1000*100), nrow = 1000, ncol=100)
## ��ǥ: A�� �ະ �հ踦 ���϶�.
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
r <- apply(A,1,sum) ##A�� ���� row�� sum�� �ض�. ���� 1 ��� 2�� column�� sum.
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
r <- apply(A,2,sum) ##A�� ���� row�� sum�� �ض�. ���� 1 ��� 2�� column�� sum.
r
toc <- proc.time()
toc - tic

install.packages("Rmpfr")
library(Rmpfr)
?Rmpfr