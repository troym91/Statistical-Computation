rm(list=ls())
m <- 7.8453211e7
m
x <- seq(0, 1, 1/m)
sample(x,100,replace = FALSE)

n <- 4e4
x <- runif(n)
hist(x, freq = F, nclass = 10, main = "Uniform", ylim = c(0, 1.2))
abline(h=1, col='red', lty='dotted', lwd=2)

n <- 10
N <- 1000
M <- rep(0,N)
for (i in 1:N){
  M[i] <- max(runif(n))
}
hist(M,20, xlim = c(0,1), main="Max of 10 uniform's")
summary(M)

N <- 1000
theta <- runif(N)*2*pi
pos.x <- sin(theta)
hist(pos.x, xlim=c(-1,1), main="x-Position", freq=F)
summary(pos.x)

p <- 0.6
n <- 1e4
tf <- (runif(n) < p)*1
tf
sum(tf)/n
hist(tf, 10, freq=F)

# Bern(0.5)를 따르는 X1, X2, ..., X100
p <- 0.5
n <- 100
bern100 <- (runif(n) < p) * n
sum(bern100)/n # p-hat(MLE)

k <- 3
p <- 0.5
max.trial <- 10
n.repeat <- 1000
count <- 0 

rbinom(1,1,0.5)

for (r in 1:n.repeat){
  balance <- k
  trial <- 0
  while((trial < max.trial) & (balance > 0)){
    trial <- trial + 1
    balance <- balance + 2*rbinom(1,1,p) - 1
  }
  if(balance>0) count <- count + 1
}
trial
count/n.repeat

a <- rbinom(1000,1,0.5)
Mat.a <- matrix(a,byrow = T, nrow = 100)
Bin <- apply(Mat.a,1,sum)
Bin
hist(Bin,11, xlim=c(0,10), freq = F)
par(new = T)
plot(dbinom(x,100,0.5))

##예 4: B(n,p)를 따르는 이항확률변수가 홀수값을 취할 확률?
n <- 100
p <- 0.25
prob1000 <- sum(rbinom(1000,n,p) %% 2 == 1)/1000
prob1000

## 음이항분포 - 성공확률 0.5, 세번째 성공까지의 실패 횟수.
k <- 3
p <- 0.5
N <- 1000
random.T <- rep(0,N)
for (i in 1:N){
  success <- 0
  fail <- 0
  while (success < k){
    ifelse(rbinom(1,1,p) == 1, success <- success + 1, fail <- fail + 1)
  }
  random.T[i] <- fail
}
hist(random.T,freq=F, xlim=c(0,15))
table(random.T)

## Exponetial
u <- runif(1000)
lambda <- 2
Y <- -1/lambda * log(1-u)
hist(Y, 13, freq=F, main="Histogram of Y ~ Exp(2)")
round(summary(Y),2)
curve(dexp(x,1),add=T, col = "blue")
rexp()

mean(Y > 1)
exp(-2)

## Poisson(2)
lambda <- 2
N <- 1000
z <- rep(0, N)
for (i in 1:N){
  time <- 0
  count <- 0
  while(time < 1){
    time <- time + rexp(1,lambda)
    count <- count + 1
  }
  z[i] <- count - 1
}
sum(z == 0)/N
exp(-2)

print(mean(rpois(1000,2) > 5), 4)
print(mean(rpois(1000,2) > 7))
?rpois

theta <- runif(1000)*2*pi
cos(theta); sin(theta)

mu <- 172
sigma <- 12
x <- rnorm(10000, mu, sigma)
obs <- floor(x)
table(obs %% 2)

# 예6
N <- 10000
u <- rnorm(N)
v <- rnorm(N)
rho <- 0.5
x <- u
y <- rho * u + sqrt(1-rho^2) * v
sum((x > 0) & (y > 0))/N * 100
mean((y > x) & (x > 0))/mean(x > 0)

N <- 100
lambda
rexp(N, lambda)
sum(rexp(N, lambda))
rgamma(1, N, lambda)


sum(rexp(10,2))
hist(rgamma(1000,10,2), 20, freq = F, main = "Histogram of 1000 Gamma(10,2) Samples")
curve(dgamma(x,10,2), xlim=c(0,14), col = 'blue', add= T, lty = 2)
mean(rgamma(1000,10,2))
sum(rgamma(1000,10,2)>10)/1000 * 100
qgamma(0.95,10,2)

T.A <- rgamma(10000,10,2)
T.B <- rgamma(10000,10,2)
F.ratio <- T.A/T.B
hist(F.ratio,30)


y <- apply(matrix(rexp(1000*100, 1/100),1000,100),1,max)
hist(y, 20, freq = F, xlim = c(0,1500), ylim=c(0,0.005))
f <- function(z){
  0.01*exp(-0.01*(z-100*log(100))-exp(-0.01*(z-100*log(100))))
}
par(new=T)
plot(f,0,1500,ylim=c(0,0.004), lty='dotted', col='blue', xlab='', ylab='')

quantile(y,probs = 0.95)
n <- 1000
q95 <- rep(0,n)
for (i in 1:n){
  y <- apply(matrix(rexp(1000*100, 1/100), 1000, 100), 1, max)
  q95[i] <- quantile(y, probs = 0.95)
}
hist(q95, xlim=c(700,820), freq=F, main = "Histogram of Upper 5% Quantile")
q95.hat <- mean(q95); q95.hat
q95.sd <- sd(q95)/sqrt(1000); round(q95.sd,3)
