library(data4led)
bulb <- led_bulb(1,seed = 123)
t <- bulb$hours
y <- bulb$percent_intensity

f4 <- function(x,a0=100,a1=-1.81e-4,a2=0.83){a0+a1*x+a2*log(0.005*x+1)}

x <- seq(-10,80001,2)
y4 <- f4(x)

par(mfrow=c(1,2),mar=c(2,2,3,0.25),oma=rep(0.5,4))
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16,main='f4')
lines(x,y4,col=2)
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16, xlim = c(-10,80000),ylim = c(-10,120))
lines(x,y4,col=2)

solve_me <- function(x){f4(x)-80}
interval <- c(10000,1000000)
uniroot(solve_me, interval)$root
