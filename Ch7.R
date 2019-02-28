rm(list=ls())

n <- 1000
x1 <- rnorm(n)
x2 <- rgamma(n,10,2)
par(mfrow=c(1,2))
boxplot(x1, ylab="1000 normal numbers")
boxplot(x2, ylab="1000 gamma numbers")

a <- rnorm(100)
fivenum(a)
summary(a)

boxp <- function(x, ylab){
  five <- fivenum(x)
  quart <- five[2:4]
  fence.1 <- quart[1] - 3*(quart[2] - quart[1])
  fence.2 <- quart[3] + 3*(quart[3] - quart[2])
  fence.1.adj <- min(x[x > fence.1])
  fence.2.adj <- max(x[x < fence.2])
  y.range <- max(x) - min(x)
  y.lim <- c(max(x) + 0.1*y.range, min(x)-0.1*y.range)
  outlier <- x[x < fence.1 | x > fence.2]
  
  x.1 <- c(0,0)
  par(xaxt = 'n')
  plot(y.lim ~ x.1, type="n", xlim=c(-2,2), xlab="", ylab=ylab)
  lines(c(-1,1), c(quart[3],quart[3])) ##q3
  lines(c(-1,1), c(quart[2],quart[2]), lwd=3) ##q2
  lines(c(-1,1), c(quart[1],quart[1])) ##q1
  lines(c(-1,-1), c(quart[1],quart[3]))
  lines(c(1,1), c(quart[1],quart[3]))
  lines(c(0,0), c(quart[3],fence.2.adj))
  lines(c(0,0), c(quart[1],fence.1.adj))
  for (i in 1:length(outlier)) points(0,outlier[i],cex=1)
}

n <- 1000
set.seed(124)
x1 <- rnorm(n,100,15)
x2 <- rgamma(n,10,2)

par(mfrow=c(1,2))
boxp(x1,"1000 normal numbers")
boxp(x2, "1000 gamma numbers")
