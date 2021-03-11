options(digits=6)

f <- function(x)
  (x*exp(-x)+2)/(x^2 + 2)

bisection <- function(f, a, b, tol, max) {
  iter = 0
  while(abs(b-a)>tol){
    c = (a+b)/2
    #biar berhenti
    if(iter == max)
      break
    if(f(c)==0)
      root = c
    
    if(f(a)*f(c)<0)
      b = c
    else
      a = c
    iter = iter+1
    cat("Iterasi ke-", iter, "Nilai a: ", a, "Nilai b: ", b, "Nilai c:", c, "Nilai b-c: ", b-c, "Nilai f(c)", f(c), "\n")
  }
  root = (a+b)/2
  return (root)
}

regulafalsi <- function(f, a, b, tol, max){
  iter = 0 
  cold = b
  
  while(iter <= max){
    c = a - (f(a)*(b-a))/(f(b)-f(a))
    iter = iter+1
    if(abs(c - cold)<tol*abs(c)){
      cat("Root ", c)
      break
    }
    else{
      cat("Iterasi ke-", iter, "Nilai a: ", a, "Nilai b: ", b, "Nilai c:", c, "Nilai b-c: ", b-c, "Nilai f(c)", f(c), "\n")
      cold = c
      if(f(a)*f(c)>0)
        a = c
      else
        b = c
    }
  }
}
bisection(f, -1, 5, 0.0001, 15)
regulafalsi(f, -1, 5, 0.0001, 15)
