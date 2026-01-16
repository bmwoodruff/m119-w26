f <- function(x){x^3}

x <- seq(-10,10,0.1)
plot(x,f(x), type = "l", col = "black", 
     #These choices were made after I hated my first graph.
     xlim=c(-4,4), 
     ylim = c(-10,10)
     )
lines(x,f(x+2), type = "l", col = "red")
lines(x,f(x-2), type = "l", col = "blue")
lines(x,f(x)+2, type = "l", col = "green")
lines(x,f(x)-2, type = "l", col = "orange")



rm(list=ls())

f1 <- function(x){
  sqrt(3-x)
}

f1(3)
f1(0)
f1(-100)
f1(10)

x <- seq(-10,3,0.1)
y <- f1(x)

par(mar=c(2.5,2.5,0.25,0.25))
plot(x,y,type='l')
  ?par



f_quad <- function(x,a=1,b=0,c=0){
  a*x^2 + b*x + c
}

f_quad(-2)
f_quad(-1)
f_quad(0)
f_quad(1)
f_quad(2)
f_quad(2,a=4,b=7,c=-13)


x <- seq(-2,2,0.1)

par(mar=c(2.5,2.5,0.25,0.25))
plot(x,f_quad(x),type='l')


f_quad1 <- function(x,a=1,b=0,c=0){
  a*x^2 + b*x + c
}

f_quad2 <- function(x,a,b,c){
  a*x^2 + b*x + c
}

f_quad1(1/2)
f_quad2(1/2,1,0,0)
