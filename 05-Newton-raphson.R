options(digits=6)
library(deriv)

f <- function(x)
  x+exp(-x^2)

newton <- function(f, x0, tol, n){
  h <- 1e-4
  i <- 0; x1 = x0
  p <- numeric(n)
  while (i <= n) {
    df.dx <- (f(x0 + h) - f(x0)) / h
    x1 <- (x0 - (f(x0) / df.dx))
    i <- i + 1
    if (abs(x1 - x0) < tol) 
      break
    x0 <- x1
    cat ("Iterasi ke-", i, "Root : ", x1, "\n")
  }
} 

newton(f, 0.1, 1e-4, 100)











