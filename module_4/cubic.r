set.seed(1)
N=30
x= 6*runif(N) -3
x=sort(x)
y= 7 - x - x^2 - 3/2*x^3 + 3*rnorm(N)
quartz()

ylabel <- expression(paste("y, ",hat(y)))
op <- par(mar = c(5,5,4,2) + 0.1)          # default is c(5,4,4,2) + 0.1
plot(x,y,col='blue',cex=2,lwd=2,xlab="x",ylab=ylabel,main="Cubic Fit to Data")
x1=x
x2=x^2
x3=x^3
cubic_fit=lm(y~x1+x2+x3)
beta0=cubic_fit$coefficients[1]
beta1=cubic_fit$coefficients[2]
beta2=cubic_fit$coefficients[3]
beta3=cubic_fit$coefficients[4]
yhat = beta0 + beta1*x + beta2*x^2 + beta3*x^3
lines(x,yhat,col='red',lwd=3)
cubic_fit
