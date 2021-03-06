# Load libraries
library(mvtnorm); library(mclust); library(e1071)

# Initialize variables
mu_1 <- matrix(c(2,0), nrow=2)
mu_2 <- matrix(c(-2,0), nrow=2)
sigma_1 <- matrix(c(1, 0, 0, 1), nrow=2)
P <- (1 / 2) * matrix(c(sqrt(3), 1, -1, sqrt(3)), ncol=2)
D <- matrix(c(9, 0, 0, 4), nrow=2)
sigma_2 <- P %*% D %*% t(P)

set.seed(1) # Generate sample data
n <- 1e3
sample_1 <- rmvnorm(n = n, mean = mu_1, sigma = sigma_1)
sample_2 <- rmvnorm(n = n, mean = mu_2, sigma = sigma_2)
sample_data <- rbind(sample_1, sample_2)

# k-means
par(mfrow = c(1,2))
k_mean_clustering <- kmeans(x = sample_data, centers = 2)
plot(x = sample_data[,1], y = sample_data[,2],
     col = c(rep('red', 1e3), rep('blue', 1e3)),
     main = 'Sample Data Colored by Class with Centers',
     xlab = 'x1', ylab = 'x2')
points(k_mean_clustering$centers, col = 'green', pch = 19)

plot(x = sample_data[,1], y = sample_data[,2], 
     col=k_mean_clustering$cluster,
     main = 'Sample Data After k-Means Classification',
     xlab = 'x1', ylab = 'x2')
points(k_mean_clustering$centers, col = 'green', pch = 19)

# Error rate 0.1975
sum(k_mean_clustering$cluster != c(rep(1, 1e3), rep(2, 1e3))) / 2e3

# fuzzy k-means
fuzzy_k_mean_clustering <- cmeans(x = sample_data, centers = 2)
plot(x = sample_data[,1], y = sample_data[,2],
     col = c(rep('red', 1e3), rep('blue', 1e3)),
     main = 'Sample Data Colored by Class with Centers',
     xlab = 'x1', ylab = 'x2')
points(fuzzy_k_mean_clustering$centers, col = 'green', pch = 19)

plot(x = sample_data[,1], y = sample_data[,2],
     col=fuzzy_k_mean_clustering$cluster,
     main = 'Sample Data After Fuzzy k-Means Classification',
     xlab = 'x1', ylab = 'x2')
points(fuzzy_k_mean_clustering$centers, col = 'green', pch = 19)

# Error rate 0.193
sum(fuzzy_k_mean_clustering$cluster != c(rep(1, 1e3), rep(2, 1e3))) / 2e3

# EM
em_clustering = Mclust(data = sample_data, G = 2)
plot(x = sample_data[,1], y = sample_data[,2],
     col = c(rep('red', 1e3), rep('blue', 1e3)),
     main = 'Sample Data Colored by Class with Centers',
     xlab = 'x1', ylab = 'x2')
points(t(em_clustering$parameters$mean), col = 'green', pch = 19)
plot(x = sample_data[,1], y = sample_data[,2],
     col=em_clustering$classification,
     main = 'Sample Data After EM Classification',
     xlab = 'x1', ylab = 'x2')
points(t(em_clustering$parameters$mean), col = 'green', pch = 19)

# Error rate 0.1005
sum(em_clustering$classification != c(rep(1, 1e3), rep(2, 1e3))) / 2e3

# Center comparison
round(k_mean_clustering$centers, 4)
round(fuzzy_k_mean_clustering$centers, 4)
round(t(em_clustering$parameters$mean), 4)
