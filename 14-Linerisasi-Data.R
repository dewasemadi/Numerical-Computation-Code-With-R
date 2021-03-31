x = c(-2,2,4,4,5,7,8,10,12,13)
y = c(3,3,10,12,18,38,54,58,62,66)

nlsfit = nls(y ~ d/(x+c), start=list(c=1,d=1))
summary(nlsfit)

plot(x,y,pch = 19, col="blue")
abline(nlsfit)