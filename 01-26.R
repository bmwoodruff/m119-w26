f <- function(x){3^x-17}
interval <- c(2,3)
my_root <- uniroot(f, interval)$root
my_root

#The code below produces a plot that illustrates what uniroot found. 
x<-seq(interval[1],interval[2],0.1)
plot(x,f(x),type="l")
abline(h=0,col = "gray")
abline(v=my_root,col = "gray")
points(my_root,0, col = "red")



f <- function(x){log(4*x-2)-5}
interval <- c(1,40)
my_root <- uniroot(f, interval)$root
my_root
uniroot(f, interval)
f(1)
f(10)

f <- function(x){log10(5*x-30)-1}
interval <- c(6.1,1000000)
my_root <- uniroot(f, interval)$root
my_root



f <- function(x){(3*x-5)-(exp(-x))}
interval <- c(-1000000,1000000)
my_root <- uniroot(f, interval)$root
my_root

interval <- c(1,2)
my_root <- uniroot(f, interval)$root
my_root

