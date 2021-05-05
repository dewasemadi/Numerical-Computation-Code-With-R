x = c(0, 2, 6, 8, 10)
y = c(0, 1.5, 1.5, 1, 2)
#mencari interpolasi piecewise polinomial
polyfit = poly.calc(x, y)
polyfit #hasilnya berupa fungsi

plot(x,y)
curve(polyfit, add=T) #polynomial curve fit
plot.new()
curve(polyfit, ylim= c(-1,5)) #polynomial
