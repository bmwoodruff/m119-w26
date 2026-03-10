rm(list=ls())
data <- read.csv(url("https://byuistats.github.io/M119/data2_ls.csv"))
x <- data$x
y <- data$y

c11 <- sum(1+0*x)
c12 <- sum(x)
b1 <- sum(y)
c21 <- sum(x)
c22 <- sum(x^2)
b2 <- sum(y*x)

solvesystem <- function(c11, c12,b1,c21,c22,b2){ 
  c((b1*c22 - c12*b2)/(c11*c22 - c21*c12),
    (c11*b2 - b1*c21)/(c11*c22 - c21*c12))
}

sol <- solvesystem(c11,c12,b1,c21,c22,b2)
sol
my_b <- sol[1]
my_m <- sol[2]

f <- function(x, b = my_b, m = my_m){b + m*x}
t <- seq(-6,6,0.1)
plot(x,y)
lines(t,f(t),type = "l") 

f(4) # 28.72758
uniroot(function(x){f(x)-5},c(-2,2))$root # 0.3402777