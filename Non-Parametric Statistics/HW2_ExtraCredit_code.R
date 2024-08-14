# Load library
library(dplyr)
library(ggplot2)
library(plotly)
library(reshape2)
library(readr)
library(np)

f <- function(beta_1, max_iter) {
  # Perform 200 trials and count how many times
  # we reject the null hypothesis at alpha = 0.05
  count <- 0
  for (k in 1:max_iter) {
    eps <- rnorm(100, mean = 0, sd = 0.0025)
    Z <- runif(100, min = 0, max = 3)
    X <- rnorm(100, mean = mean(Z), sd = sd(Z))
    g <- sin(Z)
    g <- g^2
    Y <- beta_1 * X + g + eps # where beta_1 = 0
    
    p <- ggplot(NULL) +
      geom_point(aes(x = Z, y = X))
    
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
      count <- 1 + count
    }
  }
  
  return(count)
  
}

# Run the process once for beta_1 = 0
beta_1 <- 0
count <- f(beta_1, 200) # got 8/200 last time
print(count)



# Run the process on a suitable grid
# of positive values of beta_1

g <- function(grid, max_iter) {
  grid <- grid
  counts <- matrix(0, nrow = length(grid), ncol = 1)
  powers <- counts
  max_iter <- max_iter
  
  k = 1
  for (beta_1 in grid) {
    counts[k] <- f(beta_1, max_iter)
    powers[k] <- counts[k] / max_iter
    k <- k + 1
  }
  
  return(powers)
  #return(counts)
  
}

grid <- seq(0.0001, 0.0020, length.out = 20)
powers <- g(grid, 200)
ggplot(NULL) + 
  geom_point(aes(x = grid, y = powers)) +
  labs(x = "beta_1", y = "power",
       title = "Power curve") +
  theme(plot.title = element_text(hjust = 0.5))
