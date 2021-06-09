f <- function(x,y) 
  return(x^2*y^3-x*y)

SimpsonKemudianTrapesium <- function(f,xa,xb,ya,yb,xh,yh) {
  xi = xa 
  res2 = 0
  Nx = abs(xa-xb)/xh
  for (x in 0:Nx){
    if(x > 0 && x %% 2 == 1){
      yi = ya
      res1 = 0
      Ny = abs(yb-ya)/yh
      for (y in 0:Ny){
        if(y > 0 && y <= Ny-1){
          res1 = res1 + 2*f(xi,yi)
        }
        else{
          res1 = res1 + f(xi,yi)
        }
        yi = yi + yh
      }
      fxydx = (yh/2) * (res1)
      res2 = res2 + 4*fxydx
    }
    else if(x > 0 && x %% 2 == 0 && x != Nx){
      yi = ya
      res1 = 0
      Ny = abs(yb-ya)/yh
      for (y in 0:Ny){
        if(y > 0 && y <= Ny-1){
          res1 = res1 + 2*f(xi,yi)
        }
        else{
          res1 = res1 + f(xi,yi)
        }
        yi = yi + yh
      }
      fxydx = (yh/2) * (res1)
      res2 = res2 + 2*fxydx
    }
    else{
      yi = ya
      res1 = 0
      Ny = abs(yb-ya)/yh
      for (y in 0:Ny){
        if(y > 0 && y <= Ny-1){
          res1 = res1 + 2*f(xi,yi)
        }
        else{
          res1 = res1 + f(xi,yi)
        }
        yi = yi + yh
      }
      fxydx = (yh/2) * (res1)
      res2 = res2 + fxydx
    }
    xi = xi + xh
  }
  fxydxy = (yh/3) * (res2)
  return (fxydxy)
}

SimpsonKemudianTrapesium(f,1,3,0,2,0.5,0.5)