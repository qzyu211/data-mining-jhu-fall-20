# Load data
polynomial_data <- read.csv('polynomial_data.txt', header = FALSE, sep = "")
colnames(polynomial_data) <- c("X", "Y")

# Create initial variables
n <- nrow(polynomial_data)
X <- polynomial_data[,1]
Y <- polynomial_data[,2]

# Calculate the nth degree for X
nth_degree <- function(x, n) {
  return(x^n)
}

x2 <- nth_degree(x = X, n = 2)
x3 <- nth_degree(x = X, n = 3)
x4 <- nth_degree(x = X, n = 4)
x5 <- nth_degree(x = X, n = 5)

# Create the matrices for the nth degree polynomials
ones <- rep(1, n)
X3 <- cbind(ones, X, x2, x3)
X4 <- cbind(ones, X, x2, x3, x4)
X5 <- cbind(ones, X, x2, x3, x4, x5)

# Calculate the beta hat
beta_hat <- function(X, y) {
  beta_hat <- solve(t(X) %*% X) %*% t(X) %*% y
  return(beta_hat)
}

beta_hat_3 <- beta_hat(X = X3, y = Y)
beta_hat_4 <- beta_hat(X = X4, y = Y)
beta_hat_5 <- beta_hat(X = X5, y = Y)

# Calculate the nth degree y-hat
y3 <- X3 %*% beta_hat_3
y4 <- X4 %*% beta_hat_4
y5 <- X5 %*% beta_hat_5

# Calculate the adjusted R-squared
adj_r_squared <- function(X, betahat, y) {
  y_bar <- mean(y)
  y_hat <- X %*% betahat

  SS_tot <- sum((y - y_bar)^2)
  SS_res <- sum((y - y_hat)^2)
  
  n <- length(y)
  p <- ncol(X) - 1
  df_e <- n - p - 1
  df_t <- n - 1
  
  adj_r_square <- 1 - ((SS_res / df_e) / (SS_tot / df_t))
  return(adj_r_square)
}

adj_r_squared_3 <- adj_r_squared(X = X3, betahat = beta_hat_3, y = Y)
adj_r_squared_4 <- adj_r_squared(X = X4, betahat = beta_hat_4, y = Y)
adj_r_squared_5 <- adj_r_squared(X = X5, betahat = beta_hat_5, y = Y)

### Chosen model: n=4
y_hat_4 <- X4 %*% beta_hat_4
e_4 <- Y- y_hat_4
mean(e_4)
sd(e_4)

sorted_residuals <- sort(e_4)
e_quantile <- qnorm((1:n)/n, mean = mean(e_4), sd = sd(e_4))
plot(e_quantile, sorted_residuals, main = 'Q-Q Plot',
     xlab = latex2exp::TeX('$Normal(\\bar{e}, \\sigma_{e}^2)$'),
     ylab = latex2exp::TeX('Sorted Residuals'))
df <- data.frame(x = e_quantile, y = sorted_residuals)
fit <- lm(formula = y~x, data = df[1:(n-1),])
abline(fit)

