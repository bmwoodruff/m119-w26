(-2)^2
(-1)^2
# Define the vector of numbers
numbers <- c(-2, -1, 0, 1, 2, 3, 4, 5)

# Compute the square of each number
squares <- numbers^2

# View the results
squares


x <- seq(-2,5,0.5)
x
x^2

y <- x^2

x <- seq(-2,5,0.1)
y <- x^2
plot(x,y,type="l")


f <- function(x){x^2}

x <- seq(-2,5,0.1)
plot(x,f(x),type="l")

f <- function(x){ifelse(x < 1, 2*x, 3)}
f(17)

x <- seq(-2,5,0.001)
plot(x,f(x), type="l")


x <- seq(-2,0.999,0.001)
plot(x,f(x), 
     type = "l")
x <- seq(1,5,0.1)
lines(x,f(x), type = "l")


?mtcars


par(mar=c(4,4,0.25,0.25))
x <- mtcars$wt
y <- mtcars$mpg
plot(x,y)


par(mar=c(4,4,2,0.25))
x <- mtcars$wt
y <- mtcars$mpg
plot(x,y,
     pch=16,
     xlab='weight (1000 lbs)',
     ylab='Miles per US gallon',
     main='Our 1st Scatter Plot')

#We can also put everything on a single line, as done below, but the above is easier to read.
plot(x,y,pch=16,xlab='weight (1000 lbs)',ylab='Miles per US gallon',main='Our 1st Scatter Plot')
