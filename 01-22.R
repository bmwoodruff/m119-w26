exp(2)
exp(1)


x <- seq(-3,3,0.01)
plot(x, exp(x), type="l")


x <- seq(-4,20,0.1)
y <- 10-sqrt(x+4)
par(mar=c(2.5,2.5,0.5,0.5))
plot(x,y,type='l')



library(data4led)
dist <- led_time(2100)
head(dist)
hist(dist$percent_intensity,probability = TRUE)



f1 <- function(L,h=0,a=1){
  #Make sure h > 0 and a > 0.
  1/sqrt(2*pi*a)*exp(-(L-h)^2/(2*a))
}

a <- 1
L <- seq(80,120,0.1)
h <- 101.5
y <- f1(L,h,a)

par(mfrow=c(1,2),mar=c(2.5,2.5,1,0.25))
plot(L,y,type='l',xlim=c(80,120))
mtext('plot f1 with h= and a=', side = 3, line = 0)

h <- 104.5
y <- f1(L,h,a)

plot(L,y,type='l',xlim=c(80,120))
mtext('change h=, keep a=', side = 3, line = 0)
