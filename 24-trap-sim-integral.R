f <- function(x,y) 
  return(x^2*y^3-x*y)

TrapesiumKemudianSimpson = function(f, xa, xb, ya, yb, xh, yh) {
  xi = xa
  res2 = 0
  N = (xb-xa)/xh
  for (x in 0:N){
    if(x > 0 && x <= N-1){
      yi = ya 
      res1 = 0
      N= abs(ya-yb)/yh
      for (y in 0:N){
        if(y > 0 && y %% 2 == 1)
          res1 = res1 + 4*f(xi,yi)
        else if(y > 0 && y %% 2 == 0 && y != N)
          res1 = res1 + 2*f(xi,yi)
        else
          res1 = res1 + f(xi,yi)
        yi = yi + yh
      }
      fxydy = yh/3 * res1
      res2 = res2 + 2*fxydy
    }
    else{
      yi = ya 
      res1 = 0
      N= abs(ya-yb)/yh
      for (y in 0:N){
        if(y > 0 && y %% 2 == 1)
          res1 = res1 + 4*f(xi,yi)
        else if(y > 0 && y %% 2 == 0 && y != N)
          res1 = res1 + 2*f(xi,yi)
        else
          res1 = res1 + f(xi,yi)
        yi = yi + yh
      }
      fxydy = yh/3 * res1
      res2 = res2 + fxydy
    }
    xi = xi + xh
  }
  fyxdyx = (xh/2) * (res2)
  return (fyxdyx)
}

TrapesiumKemudianSimpson(f,1,3,0,2,0.5,0.5)