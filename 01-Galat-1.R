library(Deriv)
#berapa kali suatu fungsi akan diturunkan?
order_func <- function(fun, order){
  if(order == 1)
    Deriv(fun)
  else
    order_func(Deriv(fun), order-1)
}

options(digits = 6)

error_abs <- function(xt, xa)
  return (abs(xt - xa))

error_relatif_abs <- function(xt, xa)
  return (abs((xt - xa)/xt))*100

#fungsi awal
f <- function(x) 
  (4*x^3 + 3*x^2)/sqrt(4*x+10)

df = order_func(f, 1)
ddf = order_func(f, 2) 

#nilai sebenarnya (xt) dari turunan pertama dan kedua
xt= c(3.8868036256589003, 6.3722735558900947)

#nilai pendekatan (xa) dari turunan pertama dan kedua
xa<-c(round(df(0.9), digits=6), round(ddf(0.9), digits=6)); xa

error_abs(xt,xa)
error_relatif_abs(xt,xa)

