f <- function(t){ log2(3*t)-2}
interval <- c(1,10)


solve_me <- function(t){f(t) - 2}
uniroot(solve_me,interval)$root

x <- seq(1,10,0.1)
plot(x, f(x), type = "l")
abline(h=0)
abline(h=2)



g <- function(x){ 3*x-15-exp(-x+6) }
uniroot(g,c(0,10))$root

x <-seq(0,30,1)
plot(x,g(x), type="l")
abline(h=0, col = "lightgray")







rm(list=ls())
library(data4led)
bulb <- led_bulb(1,seed = 123)

t <- bulb$hours
y <- bulb$percent_intensity

f0 <- function(x,a0=100 + 0*x ){ a0 }
f1 <- function(x,a0=100,a1=7e-4){ a0 + a1*x }
f2 <- function(x,a0=100,a1=1.1e-3,a2=-1.5e-7){ a0 + a1*x + a2*x^2 }
f3 <- function(x,a1=-1.9,a2=0.00114){ (100-a1) + a1*exp(-a2*x) }
f4 <- function(x,a0=100,a1=-1.81e-4,a2=0.83){a0+a1*x+a2*log(0.005*x+1)}
f5 <- function(x,a0=100,a1=6.23e-3,a2=5.06e-5){ (a0 + a1*x)*exp(-a2*x) }

x <- seq(-10,80001,2)
y0 <- f0(x)
y1 <- f1(x)
y2 <- f2(x)
y3 <- f3(x)
y4 <- f4(x)
y5 <- f5(x)


par(mfrow=c(1,2),mar=c(2,2,3,0.25),oma=rep(0.5,4))
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16,main='f0')
lines(x,y0,col=2)
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16, xlim = c(-10,80000),ylim = c(-10,120))
lines(x,y0,col=2)

par(mfrow=c(1,2),mar=c(2,2,3,0.25),oma=rep(0.5,4))
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16,main='f1')
lines(x,y1,col=2)
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16, xlim = c(-10,80000),ylim = c(-10,120))
lines(x,y1,col=2)

par(mfrow=c(1,2),mar=c(2,2,3,0.25),oma=rep(0.5,4))
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16,main='f2')
lines(x,y2,col=2)
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16, xlim = c(-10,80000),ylim = c(-10,120))
lines(x,y2,col=2)

par(mfrow=c(1,2),mar=c(2,2,3,0.25),oma=rep(0.5,4))
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16,main='f3')
lines(x,y3,col=2)
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16, xlim = c(-10,80000),ylim = c(-10,120))
lines(x,y3,col=2)

par(mfrow=c(1,2),mar=c(2,2,3,0.25),oma=rep(0.5,4))
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16,main='f4')
lines(x,y4,col=2)
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16, xlim = c(-10,80000),ylim = c(-10,120))
lines(x,y4,col=2)

par(mfrow=c(1,2),mar=c(2,2,3,0.25),oma=rep(0.5,4))
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16,main='f5')
lines(x,y5,col=2)
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16, xlim = c(-10,80000),ylim = c(-10,120))
lines(x,y5,col=2)


f4(12000)


solve_me <- function(t){f3(t) - 90}
uniroot(solve_me, c(-10000,0))$root
