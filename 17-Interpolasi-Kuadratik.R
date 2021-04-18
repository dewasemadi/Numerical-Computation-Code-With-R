library(PolynomF)

x = c(15, 15.1, 15.3)
y = log(x, base = 5)
xExp = 15.2

require(PolynomF)
polyf = poly_calc(x, y)
na = polyf(xExp); na
ns = 1.69083591; ns

galat_absolute = abs(ns-na); galat_absolute