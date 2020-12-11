setwd('~/mjw/r/data_mining/introduction')
library(mvtnorm)
mu <- c( 0,0)
P<-matrix(c(cos(pi/6),sin(pi/6),-sin(pi/6),cos(pi/6)),ncol=2);
Sigma <- P %*% matrix(diag(c(9,2)),ncol=2) %*% t(P)
N<-10000
x<-rmvnorm(n=N,mean=mu,sigma=Sigma)
cat("The dimensions of x are:",dim(x),"\n")
x1=x[,1];
x2=x[,2];
plot.new
e <- eigen(Sigma)
v <- e$vectors
A <- v %*% sqrt(diag(e$values)) %*% t(v)
theta <- seq(0,2*pi,length=500)
a1 <- cos(theta)
a2 <- sin(theta)
p <-  3 * A %*% t(cbind(a1,a2))
plot(x1,x2,pch='.',cex=4,col="orange",asp=1,xlim=c(-10,10),ylim=c(-10,10),
     xlab="",ylab="",xaxs="i",yaxs="i")
lines(p[1,],p[2,],pch='.',lwd=2,col="blue",asp=1)
xlab <- expression(paste('x'[1]))
ylab <- expression(paste('x'[2]))
mtext(xlab,1,3,cex=1.5)
mtext(ylab,2,2,cex=1.5)
#------------------------------------------------
# in 2-d, the 1-sigma, 2-sigma, and 3-sigma
# containments are  0.393, 0.865, 0.989
#------------------------------------------------
q <- solve(A) %*% t(cbind(x1,x2))
f = function(q,sigma) 1 - sum(q[1,]*q[1,] + q[2,]*q[2,] > sigma^2 )/N
title(main= sprintf('Gaussian data with 3-sigma ellipse, N = %2d,
       Containment = %2.4f',N,f(q,3)))
