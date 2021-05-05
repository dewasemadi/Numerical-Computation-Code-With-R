library(pracma)
f = function(x)
    x^5-3*x^3+x+4

x = c(1,2,3,4)
y = c(f(1), f(2), f(3), f(4))

newtonInterp(x,y,3.5);

