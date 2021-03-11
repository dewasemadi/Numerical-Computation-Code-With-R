options(digits=6)

f <- function(x)
  x+exp(-x^2)

secant <- function(f, p0, p1, epsilon, n) {
  for ( i in 1:n ) {
    p2 <- p1-(f(p1)*(p1-p0)/(f(p1)-f(p0)))
    if (abs(p2-p1) < epsilon)
      break
    cat("Iterasi ke-", i, "Root: ", p2, "\n")
    p0 <- p1
    p1 <- p2
  }
}

secant(f, -1, 2,  1e-2, 7)
