f = function(x,y) 
  return ((2*x-3*y)/2)

f1 = function(x, y)
  return (-2*x*y^2)

# ordo 4
rangeKuta = function(f, x0, y0, h, p) {
  while (p != x0) {
    k1 = h*f(x0,y0)
    k2 = h*f(x0 + (1/2)*h, y0 + (1/2)*k1)
    k3 = h*f(x0 + (1/2)*h, y0 + (1/2)*k2)
    k4 = h*f(x0 + h, y0 + k3)
    k_final = y0 + (1/6)*(k1 + 2*k2 + 2*k3 + k4)
  
    x1 = x0+h
    x0 = x0+h
    y0 =  k_final
    
    cat("x:",x1, "y:",  k_final,"\n")
  }
  return (k_final)
}

# rangeKuta(f, 0, 1, 1, 3)

# true_value = 1.571637893
# galat_absolute = abs(true_value - rangeKuta(f, 0, 1, 1, 3)); 
# cat("galat_absolute: ", galat_absolute, "\n")

rangeKuta(f1, 0, 1, 0.25, 1)