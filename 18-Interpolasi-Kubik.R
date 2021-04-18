library(pracma)
x = c(0, 0.3, 0.6, 0.9)
y = sin(x) + 2

xExp = 0.3
na = cubicspline(x, y, xExp); na
ns = 2.2955202066613394; ns;
galat_absolute = abs(ns-na); galat_absolute
