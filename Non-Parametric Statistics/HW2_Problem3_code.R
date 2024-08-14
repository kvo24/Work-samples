# Load library
library(dplyr)
library(ggplot2)
library(plotly)
library(reshape2)
library(readr)
library(np)

## Non-parametric model:

# Generate data
Z <- runif(100, min = 0, max = 3)
eps <- rnorm(100, mean = 0, sd = 0.0025)
g <- sin(Z)
g <- g^2

# Perform local constant aka NW regression for
# Y = sin^2(Z) + eps

Y <- g + eps # First, calculate Y

bws = c(0.3, 0.7, 3)
Yhats = matrix(0, nrow = 100, ncol = length(bws))
k = 1
for (bw in bws) {
  ghat1 <- npreg(xdat = Z, ydat = Y, bws = bw, regtype = "lc")
  Yhats[ , k] <- ghat1$mean
  k <- 1 + k
}

ggplot(NULL) + 
  geom_point(aes(x = Z, y = Y)) + 
  geom_point(aes(x = Z, y = Yhats[ , 3]), color = "red",
             size = 1.2, shape = 5) + 
  geom_point(aes(x = Z, y = Yhats[ , 2]), color = "orange",
             size = 1.2, shape = 5) + 
  geom_point(aes(x = Z, y = Yhats[ , 1]), color = "purple",
             size = 1.2, shape = 5) + 
  labs(title = bquote("Local constant regression for" ~ Y == sin(z)^2 + e))



# Perform local linear regression for
# Y = sin^2(Z) + eps
bws = c(0.3, 1, 3)
Yhats = matrix(0, nrow = 100, ncol = length(bws))
k = 1
for (bw in bws) {
  ghat1 <- npreg(xdat = Z, ydat = Y, bws = bw, regtype = "ll")
  Yhats[ , k] <- ghat1$mean
  k <- 1 + k
}

ggplot(NULL) + 
  geom_point(aes(x = Z, y = Y)) + 
  geom_point(aes(x = Z, y = Yhats[ , 3]), color = "red",
             size = 1.2, shape = 5) + 
  geom_point(aes(x = Z, y = Yhats[ , 2]), color = "orange",
             size = 1.2, shape = 5) + 
  geom_point(aes(x = Z, y = Yhats[ , 1]), color = "purple",
             size = 1.2, shape = 5) +
  labs(title = bquote("Local linear regression for" ~ Y == sin(z)^2 + e))


## Second option for Y
# Re-generate data
Z <- runif(100, min = 0, max = 3)
eps <- rnorm(100, mean = 0, sd = 0.0025)

# Perform local constant aka NW regression for
# Y = Z + eps

Y <- Z + eps # First, calculate Y

bws = c(0.3, 0.7, 3)
Yhats = matrix(0, nrow = 100, ncol = length(bws))
k = 1
for (bw in bws) {
  ghat1 <- npreg(xdat = Z, ydat = Y, bws = bw, regtype = "lc")
  Yhats[ , k] <- ghat1$mean
  k <- 1 + k
}

ggplot(NULL) + 
  geom_point(aes(x = Z, y = Y)) + 
  geom_point(aes(x = Z, y = Yhats[ , 3]), color = "red",
             size = 1.2, shape = 5) + 
  geom_point(aes(x = Z, y = Yhats[ , 2]), color = "orange",
             size = 1.2, shape = 5) + 
  geom_point(aes(x = Z, y = Yhats[ , 1]), color = "purple",
             size = 1.2, shape = 5) + 
  labs(title = bquote("Local constant regression for" ~ Y == Z + e)) +
  theme(plot.title = element_text(hjust = 0.5))



# Perform local linear regression for
# Y = Z + eps
bws = c(0.3, 1, 3)
Yhats = matrix(0, nrow = 100, ncol = length(bws))
k = 1
for (bw in bws) {
  ghat1 <- npreg(xdat = Z, ydat = Y, bws = bw, regtype = "ll")
  Yhats[ , k] <- ghat1$mean
  k <- 1 + k
}

ggplot(NULL) + 
  geom_point(aes(x = Z, y = Y)) + 
  geom_point(aes(x = Z, y = Yhats[ , 3]), color = "red",
             size = 1.2, shape = 5) + 
  geom_point(aes(x = Z, y = Yhats[ , 2]), color = "orange",
             size = 1.2, shape = 5) + 
  geom_point(aes(x = Z, y = Yhats[ , 1]), color = "purple",
             size = 1.2, shape = 5) +
  labs(title = bquote("Local linear regression for" ~ Y == Z + e)) + 
  theme(plot.title = element_text(hjust = 0.5))




## PART 2
## Partially linear model:

# Run the procedure once to get a p-value
X <- runif(100, min = 0, max = 3)
eps <- rnorm(100, mean = 0, sd = 0.0025)
Z <- runif(100, min = 0, max = 3)
g <- sin(Z)
g <- g^2
Y <- g + eps # where beta_1 = 0

# Perform local linear regression to estimate
# the conditional expectation E(Y|Z)
bw1 <- npregbw(xdat = Z, ydat = Y, regtype = "ll")
ghat1 <- npreg(bw1, regtype = "ll")
EYZ <- ghat1$mean
res1 <- Y - EYZ

# Perform local linear regression to estimate
# the conditional expectation E(X|Z)
bw2 <- npregbw(xdat = Z, ydat = X, regtype = "ll")
ghat2 <- npreg(bw2, regtype = "ll")
EXZ <- ghat2$mean
res2 <- X - EXZ

# Estimate beta_1 by regressing the residuals
# Y - E(Y | Z) on the residuals X - E(X | Z) 
# using lm
robinson <- lm(res1~res2)
summary(robinson)

# Now perform 200 trials and count how many times
# we reject the null hypothesis at alpha = 0.05
beta_1_0_count <- 0
for (k in 1:200) {
  X <- runif(100, min = 0, max = 3)
  eps <- rnorm(100, mean = 0, sd = 0.0025)
  Z <- runif(100, min = 0, max = 3)
  g <- sin(Z)
  g <- g^2
  Y <- g + eps # where beta_1 = 0
  
  # Perform local linear regression to estimate
  # the conditional expectation E(Y|Z)
  bw1 <- npregbw(xdat = Z, ydat = Y, regtype = "ll")
  ghat1 <- npreg(bw1, regtype = "ll")
  EYZ <- ghat1$mean
  res1 <- Y - EYZ
  
  # Perform local linear regression to estimate
  # the conditional expectation E(X|Z)
  bw2 <- npregbw(xdat = Z, ydat = X, regtype = "ll")
  ghat2 <- npreg(bw2, regtype = "ll")
  EXZ <- ghat2$mean
  res2 <- X - EXZ
  
  # Estimate beta_1 by regressing the residuals
  # Y - E(Y | Z) on the residuals X - E(X | Z) 
  # using lm
  robinson <- lm(res1~res2)
  s <- summary(robinson)
  
  if (s$coefficients[2, 4] < 0.05) {
    beta_1_0_count <- 1 + beta_1_0_count
  }
  
  #ggplot(NULL) + 
   # geom_point(aes(res2, res1))
}

#################
## Set beta_1 = 1
beta_1_1_count <- 0
for (k in 1:200) {
  X <- runif(100, min = 0, max = 3)
  eps <- rnorm(100, mean = 0, sd = 0.0025)
  Z <- runif(100, min = 0, max = 3)
  g <- sin(Z)
  g <- g^2
  Y <- X + g + eps # where beta_1 = 1
  
  # Perform local linear regression to estimate
  # the conditional expectation E(Y|Z)
  bw1 <- npregbw(xdat = Z, ydat = Y, regtype = "ll")
  ghat1 <- npreg(bw1, regtype = "ll")
  EYZ <- ghat1$mean
  res1 <- Y - EYZ
  
  # Perform local linear regression to estimate
  # the conditional expectation E(X|Z)
  bw2 <- npregbw(xdat = Z, ydat = X, regtype = "ll")
  ghat2 <- npreg(bw2, regtype = "ll")
  EXZ <- ghat2$mean
  res2 <- X - EXZ
  
  # Estimate beta_1 by regressing the residuals
  # Y - E(Y | Z) on the residuals X - E(X | Z) 
  # using lm
  robinson <- lm(res1~res2)
  s <- summary(robinson)
  
  if (s$coefficients[2, 4] < 0.05) {
    beta_1_1_count <- 1 + beta_1_1_count
  }
  
  #ggplot(NULL) + 
   # geom_point(aes(res2, res1))
}


