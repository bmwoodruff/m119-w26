p3v1 <- function(x,lambda=2){
  # each element of x must be a whole number
  prod((lambda^x/factorial(x))*exp(-lambda))
}

lambda <- seq(7.66666,7.66667,0.0000001)
x <- c(7,13,3) #how many storms
probs <- sapply(lambda, p3v1, x = x)
plot(lambda,probs,type='l')



###Define the distribution###
p <- function(x,lambda=1){
  # x must be a whole number
  (lambda^x/factorial(x))*exp(-lambda)
}

###Define the likelihood function###
LP <- function(lambda,x){
  # The element of x must be a whole numbers.
  prod(p(x,lambda))
}

###Possible Parameter Values###
lambda <- seq(0,10,0.001)

###Data###
# Florida Hurricane Data (2000-2022)
data <- c(4,4,8,8,6,8,2,8,8,4,8,6,4,3,2,4,6,7,4,7,13,3,3)

#Here we calculate the output from the likelihood function given the observed data.
y <- sapply(lambda,FUN=LP,x=data)

#We plot the likelihood function.
par(mar=c(2.5,2.5,3,0.25))
plot(lambda,y,type='l',main='Poisson Likelihood')



###Define the distribution###
f2 <- function(x,lambda=1){
  # x and lambda must be positive
  lambda*exp(-lambda*x)
}

###Define the Likelihood function###
LE <- function(lambda,x){
  # The elements of x must be positive.
  prod(f2(x,lambda))
}

###Possible Parameter Values###
lambda <- seq(0,10,0.001)

###Data###
# Some Simulated Data (This is data from an Exponential random variable.)
data <- c(0.45729967, 0.47156107, 1.21461705, 0.20539769, 1.78975399, 0.09095850, 0.64675475, 1.60109333, 1.57752679, 0.01238945)

#Here we calculate the output from the likelihood function given the observed data.
y <- sapply(lambda,FUN=LE,x=data)

#We plot the likelihood function.
par(mar=c(2.5,2.5,3,0.25))
plot(lambda,y,type='l',main='Exponential Likelihood')
