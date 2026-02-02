p <- function(x,lambda=2){
  # x must be a whole number
  (lambda^x/factorial(x))*exp(-lambda)
}
p(1, lambda=1)
p(0, lambda=1)
p(2, lambda=1) + p(3, lambda=1) + p(4, lambda=1) + p(5, lambda=1) + p(6, lambda=1) + p(7, lambda=1) + p(8, lambda=1) + p(9, lambda=1) + p(10, lambda=1) + p(11, lambda=1) + p(12, lambda=1)  + p(13, lambda=1) 

i <- seq(2,100)
sum(p(i,lambda=1))

1-sum(p(0:1,1))

#Group Meeting
i <- seq(0,7)
sum(p(i,lambda=3))

i <- seq(0,7)
sum(p(i,lambda=6))

i <- seq(8,100)
sum(p(i,lambda=6))
i <- seq(0,7)
1-sum(p(i,lambda=6))


#Additional Calculations and Questions
#Compute the probability of 7 Florida tropical storms in a year assuming lambda = 8
p(7,lambda = 8)
p(7,lambda = 4)
p(7,lambda = 6)
p(7,lambda = 7)
p(7,lambda = 6.9)
p(7,lambda = 7.1)

lambda = seq(6,8,0.01)
plot(lambda,p(7,lambda), type = "l")



p3v1 <- function(x,lambda=2){
  # each element of x must be a whole number
  prod((lambda^x/factorial(x))*exp(-lambda))
}

p3v2 <- function(x1,x2,x3,lambda=2){
  # x1, x2, and x3 must be whole numbers
  (lambda^(x1+x2+x3)/(factorial(x1)*factorial(x2)*factorial(x3)))*exp(-3*lambda)
}


#The probability of 4 Florida tropical storms this year, 4 Florida tropical storms next year, and 8 Florida tropical storms the year after (using $\lambda = 2$ as assumed).
p3v1(c(4,4,8))
#The same probability as above, using the other version
p3v2(4,4,8)
#The same probabilty as above by just multiplying probabilities of independent events together. 
p(4)*p(4)*p(8)

#The probability of 2 Florida tropical storms this year, 5 Florida tropical storms next year, and 3 Florida tropical storms the year after.
p3v1(c(2,5,3))


p3v1(c(4,4,8), lambda = 10)
p3v1(c(4,4,8), lambda = 1)
p3v1(c(4,4,8), lambda = 5)
p3v1(c(4,4,8), lambda = 5.1)
p3v1(c(4,4,8), lambda = 5.2)
p3v1(c(4,4,8), lambda = 5.3)
p3v1(c(4,4,8), lambda = 5.4)
p3v1(c(4,4,8), lambda = 6)

lambda <- seq(5.1,5.4,0.01)
plot(lambda, p3v2(4,4,8,lambda), type = "l")
