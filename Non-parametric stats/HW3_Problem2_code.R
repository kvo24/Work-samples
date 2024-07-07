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

#############
# PROBLEM 2 #
#############

# Define g*(x)
g <- function(x) {
    10 * (x - 0.5)^2 + 2
}

# Define epsilon_i
eps <- function(n) {
    rnorm(n, mean = 0, sd = 0.0025)
}

# Define X_i
X <- function(n) {
    seq(1 / n, 1, 1 / n)
}

##########
# Part I #
##########

# Generate a sample of size n = 100 
# using the model in this equation:
#      Y_i = g*(X_i) + e_i    (1)
n <- 100
Xi <- X(n)
Yi <- g(Xi) + eps(n)
sample <- data.frame(cbind(Xi, Yi))

# Employ kNN to compute hat(g) over a dense grid
# of points between 0 and 1.
grid <- matrix(seq(0, 1, length.out = 1000))
preds <- list()
for (k in c(1, 3, 15, 100)) {
    model <- knn.reg(sample$Xi, grid,
                         y = sample$Yi, 
                         k = k)
    preds[[k]] <- model$pred
}

# Create a scatterplot of the generated sample
# and overlay the true regression function g*,
# together with the estimated functions hat(g)
# for different values of k.
ggplot(NULL) +
  geom_point(aes(x = Xi, y = Yi), size = 0.7, color = "black") +
  geom_line(aes(x = Xi, y = g(Xi), color = "true"), linewidth = 0.4) + 
  geom_line(aes(x = grid, y = preds[[1]], color = "k=1")) + 
  geom_line(aes(x = grid, y = preds[[3]], color = "k=3")) + 
  geom_line(aes(x = grid, y = preds[[15]], color = "k=15")) +
  geom_line(aes(x = grid, y = preds[[100]], color = "k=100")) +
  scale_color_manual(name = "",
                     values = c("true" = "black", "k=1" = "purple", "k=3" = "red", 
                                "k=15" = "orange", "k=100" = "yellow"),
                     breaks = c("true", "k=1", "k=3", "k=15", "k=100")) +
  labs(title = bquote("kNN estimates for " ~ hat(g)), x = bquote(~X[i]), y = bquote(~Y[i])) + 
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
  )


###########
# Part II #
###########

# For each choice of n in {10, 50, 100, 200,
# 500}, generate 1000 training data-sets of
# size n using the model in equation (1).
N <- c(10, 50, 100, 200, 500)
nsim <- 1000

# Employ kNN on each data-set, with k in 
# {1, 3, 5, 10, 20, 50, 100}, to compute
# hat(g)(x0), where x0 = 0.45.
K <- c(1, 3, 5, 10, 20, 50, 100)
x0 <- 0.45


nlist <- list()
l <- 1
for (n in N) {
  
    nkmat <- matrix(nrow = nsim, ncol = length(K))
    colnames(nkmat) <- c("k=1", "k=3", 
                         "k=5", "k=10", 
                         "k=20", "k=50", 
                         "k=100")
    
    for (t in 1:nsim) {
        
        # Generate one sample
        Xi <- X(n)
        Yi <- g(Xi) + eps(n)
        sample <- data.frame(cbind(Xi, Yi))
        
        # Employ kNN to compute hat(g)(0.45)
        kvec <- matrix(nrow = 1, ncol = length(K))
        col <- 1
        for (k in K) {
            
            if (k <= n) {
                model <- knn.reg(sample$Xi, x0,
                           y = sample$Yi, 
                           k = k)
                kvec[1, col] <- model$pred
            }
          
            col <- 1 + col
        }
        
        nkmat[t, ] <- kvec
    }
    
    nlist[[l]] <- nkmat
    l <- 1 + l
}


# Approximate Bias squared, Variance, and MSE
# at x0 = 0.45 using the 1000 predictions and
# the true value of g*(x0).

gstar <- g(x0)
actual <- rep(gstar, times = nsim)

table <- matrix(nrow = length(N) * length(K),
                ncol = 5)
colnames(table) <- c("Sample size", 
                     "k", 
                     "MSE", 
                     "Bias squared", 
                     "Variance")

for (n in 1:length(N)) {
  
    for (k in 1:length(K)) {
        
        if (K[k] <= N[n]) {
          
            row <- (n - 1) * length(K) + k
            table[row, 1] <- N[n]
            table[row, 2] <- K[k]            
            
            # select a vector of predictions
            predicted <- nlist[[n]][ , k]
            
            # calculate variance
            V <- var(predicted)
            table[row, 5] <- V
            
            # calculate bias squared
            B <- bias(actual, predicted)
            B2 <- B^2
            table[row, 4] <- B2
            
            # calculate MSE
            # should equal Bias^2 + Variance
            MSE <- mse(actual, predicted)
            table[row, 3] <- MSE
        
        }
    } 
}

table <- na.omit(table) # drop NA rows (where k > n)
View(table)

df <- data.frame(table) # for easier plotting

# Plot MSE vs Sample size
ggplot(data = df, aes(x = Sample.size, y = MSE, color = factor(k))) +
  geom_line() +
  geom_point() +
  labs(x = "Sample size", y = "MSE", color = "k") +
  scale_color_discrete(name = "k") +
  theme_minimal()

# Plot Bias squared vs Sample size
ggplot(data = df, aes(x = Sample.size, y = Bias.squared, color = factor(k))) +
  geom_line() +
  geom_point() +
  labs(x = "Sample size", y = "Bias squared", color = "k") +
  scale_color_discrete(name = "k") +
  theme_minimal()

# Plot Variance vs Sample size
ggplot(data = df, aes(x = Sample.size, y = Variance, color = factor(k))) +
  geom_line() +
  geom_point() +
  labs(x = "Sample size", y = "Variance", color = "k") +
  scale_color_discrete(name = "k") +
  theme_minimal()

# Plot MSE vs k for n = 100
df_filtered <- subset(df, Sample.size == 100)

plot1 <- ggplot(data = df_filtered, aes(x = k, y = MSE)) +
  geom_line() +
  geom_point() +
  labs(x = "k", y = "MSE", title = "MSE vs k (Sample size = 100)") +
  theme_minimal() + 
  theme(
    plot.title = element_text(hjust = 0.5)
  )

# Zoom in to see optimal k 
df_filtered <- subset(df, Sample.size == 100 & k < 20)

plot2 <- ggplot(data = df_filtered, aes(x = k, y = MSE)) +
  geom_line() +
  geom_point() +
  geom_point(data = subset(df_filtered, k == 3), 
             aes(x = k, y = MSE), 
             color = "red", shape = 1, 
             size = 6) + # Draw a circle around k = 3
  labs(x = "k", y = "MSE", title = "Local minimum at k = 3") +
  theme_minimal() + 
  theme(
    plot.title = element_text(hjust = 0.5)
  ) +
  scale_x_continuous(breaks = c(1, 3, 5, 10))

grid.arrange(plot1, plot2, ncol = 2)


############
# Part III #
############

# For each choice of n in floor(2^seq(8, 10, length.out = 10)), 
# generate 1000 training data-sets of
# size n using the model in equation (1).
N <- floor(2^seq(8, 10, length.out = 10))
nsim <- 1000

# Employ kNN on each data-set, with k = 0.2 * n^(2/3),
# to compute hat(g)(x0), where x0 = 0.45.
K <- function(n) {
    n^(2 / 3) / 5
}
x0 <- 0.45


nmat <- matrix(nrow = nsim, ncol = length(N))
colnames(nmat) <- paste("n =", N)

l <- 1
for (n in N) {

  nkvec <- matrix(nrow = nsim, ncol = 1)
  
  for (t in 1:nsim) {
    
    # Generate one sample
    Xi <- X(n)
    Yi <- g(Xi) + eps(n)
    sample <- data.frame(cbind(Xi, Yi))
    
    # Employ kNN to compute hat(g)(0.45)
    k <- K(n)
    
    if (k <= n) {
      model <- knn.reg(sample$Xi, x0,
                       y = sample$Yi, 
                       k = k)
      ghat <- model$pred
    }
    
    nkvec[t, ] <- ghat
  }
 
  nmat[ , l] <- nkvec
  l <- 1 + l
}


# Approximate the MSE at x0 = 0.45 using 
# the 1000 predictions and the true value 
# of g*(x0).

gstar <- g(x0)
actual <- rep(gstar, times = nsim)

table <- matrix(nrow = length(N),
                ncol = 2)
colnames(table) <- c("Sample size", "MSE")

for (n in 1:length(N)) {
  
      table[n, 1] <- N[n]
      
      # select a column of predictions
      predicted <- nmat[ , n]
      
      # calculate MSE
      MSE <- mse(actual, predicted)
      table[n, 2] <- MSE
      
}

View(table)
df <- data.frame(table) # for easier plotting
df2 <- log(df)

# Fit a linear regression model
lm_model <- lm(MSE ~ Sample.size, data = df2)

# Extract the slope of the line of best fit
slope <- coef(lm_model)[2]
str <- paste(round(slope, 4))

# Plot log(MSE) vs. log(n)
ggplot(data = df2, aes(x = Sample.size, y = MSE)) +
  geom_point() +
  geom_smooth(aes(color = "-0.6156"), # Mapping color aesthetic to the slope
              method = "lm", 
              formula = y ~ x, 
              se = TRUE, 
              linewidth = 0.5) +
  labs(title = bquote(~ k == Cn^{2/3}), x = "log(n)", y = "log(MSE)") +
  theme_minimal() + 
  theme(
    plot.title = element_text(hjust = 0.5)
  ) + 
  scale_color_manual(name = "slope", # Legend title
                     values = c("-0.6156" = "blue")) # Mapping color to the legend


##########################################
# Change k to be k = C * n ^(1/3) and plot
# log(MSE) vs log(n)

# For each choice of n in floor(2^seq(8, 10, length.out = 10)), 
# generate 1000 training data-sets of
# size n using the model in equation (1).
N <- floor(2^seq(8, 10, length.out = 10))
nsim <- 1000

# Employ kNN on each data-set, with k = 0.2 * n^(1/3),
# to compute hat(g)(x0), where x0 = 0.45.
K <- function(n) {
  n^(1 / 3) / 5
}
x0 <- 0.45


nmat <- matrix(nrow = nsim, ncol = length(N))
colnames(nmat) <- paste("n =", N)

l <- 1
for (n in N) {
  
  nkvec <- matrix(nrow = nsim, ncol = 1)
  
  for (t in 1:nsim) {
    
    # Generate one sample
    Xi <- X(n)
    Yi <- g(Xi) + eps(n)
    sample <- data.frame(cbind(Xi, Yi))
    
    # Employ kNN to compute hat(g)(0.45)
    k <- K(n)
    
    if (k <= n) {
      model <- knn.reg(sample$Xi, x0,
                       y = sample$Yi, 
                       k = k)
      ghat <- model$pred
    }
    
    nkvec[t, ] <- ghat
  }
  
  nmat[ , l] <- nkvec
  l <- 1 + l
}


# Approximate the MSE at x0 = 0.45 using 
# the 1000 predictions and the true value 
# of g*(x0).

gstar <- g(x0)
actual <- rep(gstar, times = nsim)

table <- matrix(nrow = length(N),
                ncol = 2)
colnames(table) <- c("Sample size", "MSE")

for (n in 1:length(N)) {
  
  table[n, 1] <- N[n]
  
  # select a column of predictions
  predicted <- nmat[ , n]
  
  # calculate MSE
  MSE <- mse(actual, predicted)
  table[n, 2] <- MSE
  
}

View(table)
df <- data.frame(table) # for easier plotting
df2 <- log(df)

# Fit a linear regression model
lm_model <- lm(MSE ~ Sample.size, data = df2)

# Extract the slope of the line of best fit
slope <- coef(lm_model)[2]
str <- paste(round(slope, 4))

# Plot log(MSE) vs. log(n)
ggplot(data = df2, aes(x = Sample.size, y = MSE)) +
  geom_point() +
  geom_smooth(aes(color = "-0.3063"), # Mapping color aesthetic to the slope
              method = "lm", 
              formula = y ~ x, 
              se = TRUE, 
              linewidth = 0.5) +
    labs(title = bquote(~ k == Cn^{1/3}), x = "log(n)", y = "log(MSE)") +
  theme_minimal() + 
  theme(
    plot.title = element_text(hjust = 0.5)
  ) + 
  scale_color_manual(name = "slope", # Legend title
                     values = c("-0.3063" = "purple")) # Mapping color to the legend
