b11 <- 2
b12 <- 3
c1 <- 4
b21 <- 5
b22 <- 6
c2 <- 7

x <- (c1*b22-b12*c2)/(b11*b22-b12*b21)
y <- (b11*c2-c1*b21)/(b11*b22-b12*b21)
x
y

solve_system <- function(b11,b12,c1,b21,b22,c2){
  c( 
    (c1*b22-b12*c2)/(b11*b22-b12*b21),
    (b11*c2-c1*b21)/(b11*b22-b12*b21)
  )
}

solve_system(2,3,4,5,6,7)
solve_system(b11,b12,c1,b21,b22,c2)

pi
log(2)
i <- seq(1,3)
i

b11 <- pi
b12 <- log(2)
c1 <- 7
b21 <- sum(i^2)
b22 <- sum(i-1)
c2 <- sum(i-i^2)

solution <- solve_system(b11,b12,c1,b21,b22,c2)
a1 <- solution[1]
a1
a2 <- solution[2]
a2

n<-seq(1,44)

b11 <- sum(n)
b12 <- 44*3
c1 <- 44*7
b21 <- 44*5 
b22 <- sum(n)
c2 <- sum(n^2)

solution <- solve_system(b11,b12,c1,b21,b22,c2)
solution
a1 <- solution[1]
a1
a2 <- solution[2]
a2

