x = c(-5,-3,-2,0,1,4,5,7,10,13)
y = c(0,1,2,4,5,6,7,9,12,15)

yfit2 = lm(y~poly(x,2,raw=TRUE))
yfit2

plot(x,y,pch = 19, col="blue")
abline(yfit2) 