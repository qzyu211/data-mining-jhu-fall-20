# library(latex2exp)
library(tikzDevice)
### 1
### a
exponential_distribution <- function(x, theta) {
  ifelse(x >= 0,
         theta * exp(-theta * x),
         0)
}

xs <- seq(-1, 5, length.out = 1e4)
ys <- exponential_distribution(x = xs, theta = 1)

thetas <- seq(0.5, 1.5, length.out = 3)
y_vec <- sapply(X = thetas, FUN = function(x)
  exponential_distribution(x = xs, theta = x))

plot(xs, y_vec[,3], type = 'l',
     main = TeX('$p(\\textit{x}|\\theta)\\;vs.\\;x,\\;\\theta =0.5,1,1.5\\;x=\\[-1,\\;5\\]$'),
     xlab = TeX('$x$'), ylab = TeX('$p(x|\\theta)$'))
lines(xs, y_vec[,2], col = 'red'); lines(xs, y_vec[,1], col = 'blue')
points(0, 0, pch=1)
points(0, 0.5, pch=19)
points(0, 1, pch=19)
points(0, 1.5, pch=19)
legend("topright",
       legend = c(TeX('$\\theta =1.5$'), TeX('$\\theta =1$'), TeX('$\\theta =0.5$')),
       col = c("black", "red", "blue"), lty = rep(1,3))
### b
thetas <- seq(0, 5, length.out = 1e4)
exp_dist_vec <- Vectorize(exponential_distribution, vectorize.args = "theta")

y_vec <- sapply(X = seq(1,3,length.out = 3), FUN = function(x)
  exp_dist_vec(x = x, theta = thetas))

plot(thetas, y_vec[,1], type = 'l',
     main = TeX('$p(x|\\theta)\\;vs.\\;\\theta,\\;\\theta = (0,\\;5\\],\\;x=1, 2, 3$'),
     xlab = TeX('$\\theta$'), ylab = TeX('$p(x|\\theta)$'))
lines(thetas, y_vec[,2], col = 'red')
lines(thetas, y_vec[,3], col = 'blue')
points(0, 0, pch=1)
legend("topright", legend = c(TeX('$x=1$'), TeX('$x=2$'), TeX('$x=3$')),
       col = c('black', 'red', 'blue'), lty = rep(1,3))

### 2
### a
uniform_distribution <- function(x, theta) {
  ifelse((0<=x) & (x<=theta),
         1 / theta,
         0)
}

# xs <- seq(-1, 6, length.out = 1e4)
# ys <- uniform_distribution(x = xs, theta = 5)
# plot(xs, ys, type = 'l')
# points(0, 0, pch=1); points(0, 0.2, pch=16)
# points(5, 0, pch=1); points(5, 0.2, pch=16)

thetas <- seq(2, 10, length.out = 1e4)
ys1 <- uniform_distribution(x = 3, theta = thetas)
ys2 <- uniform_distribution(x = 5, theta = thetas)
ys3 <- uniform_distribution(x = 7, theta = thetas)

y_vec <- sapply(X = seq(1,3,length.out = 3), FUN = function(x)
  uniform_distribution(x = x, theta = thetas))

plot(thetas, ys1, type = 'l',
     main = TeX('$p(x|\\theta)\\;vs.\\;\\theta,\\;\\theta = \\[2,\\;10\\],\\;x=3,5,7$'),
     xlab = TeX('$\\theta$'), ylab = TeX('$p(x|\\theta)$'))
lines(thetas, ys2, col = 'red')
lines(thetas, ys3, col = 'blue')
points(3, 0, pch=1); points(3, max(ys1), pch=19)
points(5, 0, pch=1); points(5, max(ys2), pch=19)
points(7, 0, pch=1); points(7, max(ys3), pch=19)
legend("topright", legend = c(TeX('$x=3$'), TeX('$x=5$'), TeX('$x=7$')),
       col = c('black', 'red', 'blue'), lty = rep(1,3))

