data <- read.csv(url("https://byuistats.github.io/M119/logLikelihood_practice.csv"))

x <- data$x
y <- data$y2

c11 <- 50
c12 <- sum(x)
c21 <- c12
c22 <- sum(x^2)
b1 <- sum(y)
b2 <- sum(x*y)

## Create a function to solve a system of equations. 
solvesystem <- function(c11, c12,b1,c21,c22,b2){ 
  c((b1*c22 - c12*b2)/(c11*c22 - c21*c12),
    (c11*b2 - b1*c21)/(c11*c22 - c21*c12))
}

sol1 <- solvesystem(c11, c12, b1, c12, c22, b2)
best_b <- sol1[1] 
best_m <- sol1[2] 

best_b
best_m



c11 <- sum(x^2)
c12 <- sum(x)
c21 <- c12
c22 <- 50
b1 <- sum(y*x)
b2 <- sum(y)

## Create a function to solve a system of equations. 
solvesystem <- function(c11, c12,b1,c21,c22,b2){ 
  c((b1*c22 - c12*b2)/(c11*c22 - c21*c12),
    (c11*b2 - b1*c21)/(c11*c22 - c21*c12))
}

sol <- solvesystem(c11, c12, b1, c12, c22, b2)
best_m <- sol[1] 
best_b <- sol[2] 

best_b
best_m

lmm <- -c11
lbb <- -c22
lmb <- -c12
D <- lmm*lbb-lmb^2
D>0
lmm<0

fxx <- -c11 
fxy <- -c12 
fyy <- -c22
D <- fxx*fyy - fxy^2
D>0
fxx<0

h <- function(x, b = best_b, m = best_m){b + m*x}

x_in <- seq(min(x),max(x),0.01)
par(mfrow=c(1,1),mar=c(2.5,2.5,0.25,0.25))
plot(x,y,type='p',pch=16)
lines(x_in,h(x_in),col=3)
abline(h=0,lty=3,col='gray')
abline(v=0,lty=3,col='gray')


rm(list=ls())
data <- read.csv(url("https://byuistats.github.io/M119/logLikelihood_practice.csv"))

x <- data$x
y <- data$y2
plot(x,y)



c11 <- sum(x^2)
b1 <- sum(y*x)

best_m <- b1/c11
best_m

h <- function(x, m = best_m){ m*x}
x_in <- seq(min(x),max(x),0.01)
par(mfrow=c(1,1),mar=c(2.5,2.5,0.25,0.25))
plot(x,y,type='p',pch=16)
lines(x_in,h(x_in),col=3)
abline(h=0,lty=3,col='gray')
abline(v=0,lty=3,col='gray')







rm(list=ls())
data <- read.csv(url("https://byuistats.github.io/M119/logLikelihood_practice2.csv"))

x <- data$x
y <- data$y1
plot(x,y)


c11 <- sum(exp(-x)^2)
b1 <- sum(y*exp(-x))

best_b <- b1/c11
best_b

h <- function(x, b = best_b){ b*exp(-x)}
x_in <- seq(min(x),max(x),0.01)
par(mfrow=c(1,1),mar=c(2.5,2.5,0.25,0.25))
plot(x,y,type='p',pch=16)
lines(x_in,h(x_in),col=3)
abline(h=0,lty=3,col='gray')
abline(v=0,lty=3,col='gray')


