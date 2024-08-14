# Load libraries
library(dplyr)
library(ggplot2)
library(plotly)
library(reshape2)
library(readr)
library(np)
library(FNN) # for kNN
library(Metrics)
library(gridExtra)
library(ggforce)
library(splines)

# Load data
setwd("C:/Users/Katie Overengland/Desktop/STATS 527/Hw 4")
data <- load("heart.RData")

# Generate data
X1 <- runif(40, min = 0, max = 1)
X2 <- runif(40, min = 0, max = 1)
eps <- rnorm(40, mean = 0, sd = 1)
gstar <- 2 - 12 * X1^2 + 15 * (X2 - 0.5)^2
Y <- gstar + eps

# gaussian kernel function
gaussian_kernel <- function(xa, xb) {
  return(exp(-0.5 * (norm(xa - xb, "2"))^2))
}

# Gram matrix function
gram <- function(X, kernel) {
  n <- dim(X)[1]
  K <- matrix(nrow = n, ncol = n)
  
  for (i in 1:n) {
    for (j in 1:n) {
      K[i, j] <- kernel(X[i, ], X[j, ])
    }
  }
  
  return(K)
}

# Coefficients vector function
alpha_hat <- function(X, y, lambda, kernel) {
  K <- gram(X, kernel)
  n <- dim(X)[1]
  I <- diag(n)
  A <- K + lambda * I
  return(solve(A, y))
}

# ghat function
ghat <- function(input, ahat, X, kernel) {
  n <- dim(X)[1]
  m <- dim(input)[1]
  g <- rep(0, m)
  for (j in 1:m) {
    for (i in 1:n) {
      g[j] <- g[j] + ahat[i] * kernel(input[j, ], X[i, ])
    }
  }
  return(g)
}

# Compute alpha hats
lambda1 <- 1e-10
lambda2 <- 1e-2
X <- cbind(X1, X2)
ahat1 <- alpha_hat(X, Y, lambda1, gaussian_kernel)
ahat2 <- alpha_hat(X, Y, lambda2, gaussian_kernel)

# Evaluate solution
l <- 21
grid01 <- seq(0, 1, length.out = l)
input <- expand.grid(grid01, grid01)
ghat1 <- ghat(input, ahat1, X, gaussian_kernel)
ghat2 <- ghat(input, ahat2, X, gaussian_kernel)

# Reshape ghat1 and ghat2 into matrices for plotting
ghat1 <- t(matrix(ghat1, nrow = l, ncol = l))
ghat2 <- t(matrix(ghat2, nrow = l, ncol = l))

# Plot solution 1
plot_ly() %>%
  add_surface(x = ~grid01, y = ~grid01, z = ~ghat1) %>%
  layout(title = "Kernel Ridge Regression\n lambda = 1e-10",
         scene = list(
           xaxis = list(title = "X2"),
           yaxis = list(title = "X1"),
           zaxis = list(title = "Y")),
          margin = list(l = 50, r = 50, b = 50, t = 50)) %>%
  add_markers(x = X1, y = X2, z = Y)

# Plot solution 2
plot_ly() %>%
  add_surface(x = ~grid01, y = ~grid01, z = ~ghat2) %>%
  layout(title = "Kernel Ridge Regression\n lambda = 1e-2",
         scene = list(
           xaxis = list(title = "X2"),
           yaxis = list(title = "X1"),
           zaxis = list(title = "Y")),
         margin = list(l = 50, r = 50, b = 50, t = 50)) %>%
  add_markers(x = X1, y = X2, z = Y)

# Generalized cross-validation
GCV <- function(lambda, X, Y, kernel) {
  K <- gram(X, kernel)
  n <- dim(X)[1]
  I <- diag(n)
  S <- K %*% solve(K + lambda * I)
  tr <- sum(diag(S))
  bottom <- 1 - tr / n

  ahat <- alpha_hat(X, Y, lambda, kernel)
  score <- 0
  for (i in 1:n){
    top <- Y[i] - ghat(t(matrix(X[i, ])), ahat, X, kernel)
    score <- score + (top / bottom)^2
  }
  score <- score / n
  return(score)
}

# Find optimal lambda for gaussian kernel
l <- 4
p <- seq(0, 12, length.out = 12 * l )
lambdas <- 10^(-p)
scores <- rep(0, 12 * l)
for (i in 1:(12 * l)) {
  scores[i] <- GCV(lambdas[i], X, Y, gaussian_kernel)
}

idx <- which.min(scores)
lambda_opt <- lambdas[idx]

ggplot(NULL) +
  geom_point(aes(x = log(lambdas, base = 10), y = scores)) +
  geom_point(aes(x = log(lambda_opt, base = 10), y = scores[idx]),
             color = "red")

##
# Re-plot solution for optimal lambda
#

# Compute alpha hat
ahat3 <- alpha_hat(X, Y, lambda_opt, gaussian_kernel)

# Evaluate solution
l <- 21
grid01 <- seq(0, 1, length.out = l)
input <- expand.grid(grid01, grid01)
ghat3 <- ghat(input, ahat3, X, gaussian_kernel)

# Reshape ghat3 into a matrix for plotting
ghat3 <- t(matrix(ghat3, nrow = l, ncol = l))

# Plot solution 3
plot_ly() %>%
  add_surface(x = ~grid01, y = ~grid01, z = ~ghat3) %>%
  layout(title = "Kernel Ridge Regression\n lambda_opt = 0.0091",
         scene = list(
           xaxis = list(title = "X2"),
           yaxis = list(title = "X1"),
           zaxis = list(title = "Y")),
         margin = list(l = 50, r = 50, b = 50, t = 50)) %>%
  add_markers(x = X1, y = X2, z = Y)

##
# Repeat the step above but replace the gaussian
# kernel with a linear kernel

# Linear kernel function
linear_kernel <- function(xa, xb) {
  #return((t(t(xa)) %*% xb)[1])
  return(xa[1] * xb[1] + xa[2] * xb[2])
}

# Find optimal lambda
l <- 4
p <- seq(-2, 10, length.out = 12 * l )
lambdas <- 10^(-p)
scores <- rep(0, 12 * l)
for (i in 1:(12 * l)) {
  scores[i] <- GCV(lambdas[i], X, Y, linear_kernel)
}

idx <- which.min(scores)
lambda_opt <- lambdas[idx]

ggplot(NULL) +
  geom_point(aes(x = log(lambdas, base = 10), y = scores)) +
  geom_point(aes(x = log(lambda_opt, base = 10), y = scores[idx]),
             color = "red")

# Compute alpha hat
ahat4 <- alpha_hat(X, Y, lambda_opt, linear_kernel)

# Evaluate solution
l <- 10
grid01 <- seq(0, 1, length.out = l)
input <- expand.grid(grid01, grid01)
ghat4 <- ghat(input, ahat4, X, linear_kernel)

# Reshape ghat4 into a matrix for plotting
ghat4 <- t(matrix(as.numeric(ghat4), nrow = l, ncol = l))

# Plot solution 4
plot_ly() %>%
  add_surface(x = ~grid01, y = ~grid01, z = ~ghat4) %>%
  layout(title = "Kernel Ridge Regression\n Linear Kernel",
         scene = list(
           xaxis = list(title = "X2"),
           yaxis = list(title = "X1"),
           zaxis = list(title = "Y")),
         margin = list(l = 50, r = 50, b = 50, t = 50)) %>%
  add_markers(x = X1, y = X2, z = Y)



# test gaussian kernel
l <- 5
grid01 <- seq(0, 1, length.out = l)
input <- expand.grid(grid01, grid01)
test <- rep(0, l^2)
for (j in 1:l^2) {
  test[j] <- gaussian_kernel(input[j, ], X[1, ])
}

# Reshape test into a matrix for plotting
test <- t(matrix(test, nrow = l, ncol = l))

# Plot test
plot_ly() %>%
  add_surface(x = ~grid01, y = ~grid01, z = ~test) %>%
  layout(title = "Test Gaussian Kernel",
         scene = list(
           xaxis = list(title = "X2"),
           yaxis = list(title = "X1"),
           zaxis = list(title = "Y")),
         margin = list(l = 50, r = 50, b = 50, t = 50)) %>%
  add_markers(x = X1[1], y = X2[1], z = 0)



# test linear kernel
l <- 5
grid01 <- seq(0, 1, length.out = l)
input <- expand.grid(grid01, grid01)
test <- rep(0, l^2)
for (j in 1:l^2) {
  test[j] <- linear_kernel(input[j, ], X[1, ])
}

# Reshape test into a matrix for plotting
test <- t(matrix(as.numeric(test), nrow = l, ncol = l))

# Plot test
plot_ly() %>%
  add_surface(x = ~grid01, y = ~grid01, z = ~test) %>%
  layout(title = "Test Linear Kernel",
         scene = list(
           xaxis = list(title = "X2"),
           yaxis = list(title = "X1"),
           zaxis = list(title = "Y")),
         margin = list(l = 50, r = 50, b = 50, t = 50)) %>%
  add_markers(x = X1[1], y = X2[1], z = 0)

