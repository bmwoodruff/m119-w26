f <- function(x){x^2}
x1 <- 3
x2 <- 3.01
slope <- (f(x2)-f(x1))/(x2-x1)
slope


f <- function(x){x^2}
x1 <- 3
x2 <- c(4,3.1,3.01,3.001, 3.0001, 3.00001,3)
slope <- (f(x2)-f(x1))/(x2-x1)
data.frame(x1 = x1, x2 = x2, slope = slope)



#Define the functions.
f1 <- function(x){x^4 -10*x^2 +3*x}
f2 <- function(x){exp(2*x)-1}
f3 <- function(x){sign(x-1)*(abs(x-1))^(1/3)}
f4 <- function(x){3*log(x-2)}

#Graph the functions.
x <- seq(-5,5,1e-3)

###figure 1###
y1 <- f1(x)
par(mfrow=c(2,2),mar=c(2,2,0.25,0.25))
plot(x,y1,type='l',xlim=c(-4,4))
points(3,f1(3),pch=16,col=2)
plot(x,y1,type='l',xlim=c(3-1,3+1),ylim=c(-50,50))
points(3,f1(3),pch=16,col=2)
plot(x,y1,type='l',xlim=c(2.75,3.25),ylim=c(-12.5,12.5))
points(3,f1(3),pch=16,col=2)
plot(x,y1,type='l',xlim=c(2.9,3.1),ylim=c(-5,5))
points(3,f1(3),pch=16,col=2)


###figure 2###
y2 <- f2(x)
par(mfrow=c(2,2),mar=c(2,2,0.25,0.25))
plot(x,y2,type='l',xlim=c(-5,5))
points(0,f2(0),pch=16,col=2)
plot(x,y2,type='l',xlim=c(-1,1),ylim=c(-5,5))
points(0,f2(0),pch=16,col=2)
plot(x,y2,type='l',xlim=c(-0.5,0.5),ylim=c(-2.5,2.5))
points(0,f2(0),pch=16,col=2)
plot(x,y2,type='l',xlim=c(-0.01,0.01),ylim=c(-.05,.05))
points(0,f2(0),pch=16,col=2)

###figure 3###
x3 <- seq(0,5,1e-3)
y3 <- f3(x3)
par(mfrow=c(2,2),mar=c(2,2,0.25,0.25))
plot(x3,y3,type='l',xlim=c(0,5), ylim=c(-2.5,2.5) )
points(2,f3(2),pch=16,col=2)
plot(x3,y3,type='l',xlim=c(1,3),ylim=c(0,2))
points(2,f3(2),pch=16,col=2)
plot(x3,y3,type='l',xlim=c(1.5,2.5),ylim=c(0.5,1.5))
points(2,f3(2),pch=16,col=2)
plot(x3,y3,type='l',xlim=c(1.99,2.01),ylim=c(0.99,1.01))
points(2,f3(2),pch=16,col=2)

###figure 4###
x4 <- seq(2,10,1e-3)
y4 <- f4(x4)
par(mfrow=c(2,2),mar=c(2,2,0.25,0.25))
plot(x4,y4,type='l',xlim=c(0,10), ylim = c(-5,5))
points(2.75,f4(2.75),pch=16,col=2)
plot(x4,y4,type='l',xlim=c(1.75,3.75),ylim=c(f4(2.75)-1,f4(2.75)+1))
points(2.75,f4(2.75),pch=16,col=2)
plot(x4,y4,type='l',xlim=c(2.5,3),ylim=c(f4(2.75)-0.25,f4(2.75)+0.25))
points(2.75,f4(2.75),pch=16,col=2)
plot(x4,y4,type='l',xlim=c(2.7,2.8),ylim=c(f4(2.75)-0.05,f4(2.75)+0.05))
points(2.75,f4(2.75),pch=16,col=2)



#Define the functions.
f1 <- function(x){x^4 -10*x^2 +3*x}
f2 <- function(x){exp(2*x)-1}
f3 <- function(x){sign(x-1)*(abs(x-1))^(1/3)}
f4 <- function(x){3*log(x-2)}

#Create a function called zooming_in that will generate a plot and 3 zoomed in versions.
zooming_in <- function(f, xvalue, xwidth=10, ywidth=10, zoom = c(0.1, 0.01, 0.001)){
  yvalue <- f(xvalue)
  
  #Create sequence of values for plots
  x <- seq(xvalue - xwidth/2, xvalue + xwidth/2, xwidth/100)
  y <- f(x)
  
  #Generate original plot, and then zoom in 3 times by factors given by zoom
  par(mfrow=c(2,2),mar=c(2,2,0.25,0.25))
  plot(x,y,type='l',xlim=xvalue + c(-1,1)*xwidth/2, ylim = yvalue + c(-1,1)*ywidth/2)
  points(xvalue,yvalue,pch=16,col=2)
  plot(x,y,type='l',xlim=xvalue + c(-1,1)*xwidth/2*zoom[1],ylim=yvalue + c(-1,1)*ywidth/2*zoom[1])
  points(xvalue,yvalue,pch=16,col=2)
  plot(x,y,type='l',xlim=xvalue + c(-1,1)*xwidth/2*zoom[2],ylim=yvalue + c(-1,1)*ywidth/2*zoom[2])
  points(xvalue,yvalue,pch=16,col=2)
  plot(x,y,type='l',xlim=xvalue + c(-1,1)*xwidth/2*zoom[3],ylim=yvalue + c(-1,1)*ywidth/2*zoom[3])
  points(xvalue,yvalue,pch=16,col=2)
}

#Use zooming_in() to plot each of the 4 original functions.
zooming_in(f1,3,10,400)
zooming_in(f2,0)
zooming_in(f3,2)
zooming_in(f4,2.75)


f1 <- function(x){x^4 -10*x^2 +3*x}
x_center <- 3
shifts <-c(-1, -0.5, -0.1, -0.01, -0.001, 0, 0.001, 0.01, 0.1, 0.5, 1)
x <- x_center + shifts
y <- f1(x)
slope <- (f1(x)-f1(x_center))/(x-x_center)
data.frame(x=x,y=y,slope=slope)

f2 <- function(x){exp(2*x)-1}
x_center <- 0
shifts <-c(-1, -0.5, -0.1, -0.01, -0.001, 0, 0.001, 0.01, 0.1, 0.5, 1)
x <- x_center + shifts
y <- f2(x)
slope <- (f2(x)-f2(x_center))/(x-x_center)
data.frame(x=x,y=y,slope=slope)



x_center <- 0
shifts <-c(-1, -0.5, -0.1, -0.01, -0.001, 0, 0.001, 0.01, 0.1, 0.5, 1)
x <- x_center + shifts
y <- f3(x)
slope <- (f3(x)-f3(x_center))/(x-x_center)
data.frame(x=x,y=y,slope=slope)

x_center <- 2.75
shifts <-c(-1, -0.5, -0.1, -0.01, -0.001, 0, 0.001, 0.01, 0.1, 0.5, 1)
x <- x_center + shifts
y <- f4(x)
slope <- (f4(x)-f4(x_center))/(x-x_center)
data.frame(x=x,y=y,slope=slope)


