library(data4led)
bulb <- led_bulb(1,seed=123)
ti <- bulb$hours
yi <- bulb$percent_intensity

g <- sum((yi-100)*ti) 
b <- sum(ti^2)
my_a1 <- g/b

-b

plot(ti,yi)
f <- function(x, a1=my_a1 ){100+a1*x}
x <- seq(0,5000)
lines(x, f(x), type='l')



## Let's create a function to solve this system of equations. 
solvesystem <- function(c11, c12,b1,c21,c22,b2){ 
  c((b1*c22 - c12*b2)/(c11*c22 - c21*c12),
    (c11*b2 - b1*c21)/(c11*c22 - c21*c12))
}
## Before moving forward, let's verify that the function works.
## The solution to $2x+3y=4$, $5x+6y=7$ is $(-1,2)$.  
## Does this function yield the same result?
solvesystem(2,3,4,5,6,7)

c11 <- sum(ti^2)
c12 <- sum(ti^3)
b1 <- sum((yi-100)*ti)
c21 <- sum(ti^3)
c22 <- sum(ti^4)
b2 <- sum((yi-100)*ti^2)

sol <- solvesystem(c11,c12,b1,c21,c22,b2)

plot(ti,yi)
f <- function(x, a1=sol[1], a2=sol[2]  ){100+a1*x+a2*x^2}
x <- seq(0,5000)
lines(x, f(x), type='l')



zi <- exp(-0.00005*ti)
w <- sum(ti*zi*(yi-100*zi)) 
b <- sum((ti*zi)^2)
my_a1 <- w/b
my_a1

plot(ti,yi)
f <- function(x, a1=my_a1 ){100*exp(-0.00005*x)+a1*x*exp(-0.00005*x)}
x <- seq(0,5000)
lines(x, f(x), type='l')

