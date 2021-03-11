g <- function(y)
  (y*cos(y) + 1)/(y^2 + 1)

error_abs <- function(xt, xa)
  return (abs(xt - xa))

error_relatif_abs <- function(xt, xa)
  return ((abs((xt - xa)/xt))*100)

xt = c(1.0886142737899036, 1.0098985101531512, 1.0009989985010017, 1.0000999899985001, 1.0000099998999983) 
xa = c(round(g(0.1), digits=6), round(g(0.01), digits=6), 
       round(g(0.001), digits=6), round(g(0.0001), digits=6), 
       round(g(0.00001), digits=6)) ; xt

error_abs(xt,xa)
error_relatif_abs(xt,xa)
