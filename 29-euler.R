f = function(x, y)
    return ((3*sqrt(x)) + y^2)

options(digits = 6)

# Euler
Euler = function(f, x0, y0, h, p) {
  i = 0 
  buffer = x0 
  langkah = p+h
  while(langkah != buffer) {
    cat("Iterasi ke:", i, "-> x=", buffer, "\t", "y=", y0, "\n")
  
    y1 = y0 + h*f(buffer,y0)
        
    y0 = y1 
    buffer = buffer + h 
    i = i + 1
  }
}

# Heun
Heun = function(f, x0, y0, h, p) {
  i = 0
  langkah = p+h
  while (x0 != langkah) {
    cat("Iterasi ke:", i, "-> x=", x0, "\t", "y=", y0, "\n")
    x1 = x0 + h
    predictor = y0 + h*f(x0,y0)
    corrector = y0+ (h/2)*(f(x0,y0)+f(x1,predictor))
  

    x0 = x0+h
    y0 = corrector
    i = i + 1
    }
}

f1 = function(x, y)
  return (-2*x*y^2)

Euler(f1, 0, 1, 0.25, 1)

Heun(f1, 0, 1, 0.25, 1)