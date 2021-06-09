library(Deriv)
options(digits = 6)
#berapa kali suatu fungsi akan diturunkan?
order_func <- function(fun, order){
  if(order == 1)
    Deriv(fun)
  else
    order_func(Deriv(fun), order-1)
}

f = function(x)
     (2*x^2+3*x-1)

# true value
df = order_func(f, 1); df
res = df(1); res

selisihMaju = function(f, x0, h){
    return ((f(x0+h)-f(x0))/h)
}

selisihMundur = function(f, x0, h){
    return ((f(x0)-f(x0-h))/h)
}

selisihPusat = function(f, x0, h){
    return ((f(x0+h)-f(x0-h))/(2*h))
}

h = 0.1
print("Selisih Maju")
for(i in 0: 3){
    x = selisihMaju(f, 1, h)
    y = selisihMundur(f, 1, h)
    z = selisihPusat(f, 1, h)
    galat1 = x - res
    galat2 = y - res
    galat3 = z - res
    
    cat("h = ", h, "\n");
    cat("Maju  : ", x, "Galat: ", round(galat1, digits = 6), "\n")
    cat("Mundur: ", y, "Galat: ", round(galat2, digits = 6), "\n")
    cat("Pusat : ", z, "Galat: ", round(galat3, digits = 6), "\n")
    h = h/10
}

