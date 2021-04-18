x = c(15, 15.3)
y = log(x, base = 5)
xExp = 15.2

# liniear approximation
apf = approxfun(x, y)
na = apf(xExp); na #na -> nilai aproksimasi
ns = 1.69083591; ns #ns -> nilai sebenarnya

galat_absolute = abs(ns-na); galat_absolute