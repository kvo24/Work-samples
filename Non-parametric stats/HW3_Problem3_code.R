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
setwd("C:/Users/Katie Overengland/Desktop/STATS 527/Hw 3")
data <- read_csv("phoneme.csv")

# Select the first log-periodogram (Y_i) 
# from the data-set. For simplicity, let 
# X_i = i/n. Combine X_i and Y_i into a 
# data frame.
she <- data[1, 2:257]
n <- length(she)
Xi <- seq(1 / n, 1, 1 / n)
Yi <- t(she)
sample <- data.frame(Xi, Yi)

# Create a rough scatter-plot of Y_i vs X_i
ggplot(sample, aes(x = Xi, y = Yi)) + 
    geom_point(size = 1)

# Function to construct the design matrix
# for the orthonormal trig basis
design <- function(M) {
  # Create the n by M design matrix, psi
  psi <- matrix(nrow = n, ncol = M)
  
  # Set the first column of psi to be the
  # first trigonometric basis function,
  # the constant 1
  psi[ , 1] <- 1
  
  # Set the remaining M - 1 columns of psi
  # to be the 2nd through Mth trigonometric
  # basis functions
  for (k in 2:M){
    if (k %% 2 == 0) {
      i <- 1
      for (x in Xi) {
        psi[i , k] <- sqrt(2) * cos(pi * k * x) 
        i <- 1 + i
      }
    } else {
      i <- 1
      for (x in Xi) {
        psi[i , k] <- sqrt(2) * sin(pi * (k - 1) * x)
        i <- 1 + i
      }
    }
  }
  
  return(psi)
}

# Verify the construction of the design matrix
# by visualizing the first few basis functions
M <- 7
psi <- design(M)

view_design <- function(psi) {
    df <- data.frame(psi)
    ggplot(NULL) +
        geom_point(aes(x = Xi, y = df$X1), size = 0.1) +
        geom_point(aes(x = Xi, y = df$X2), size = 0.1) +
        geom_point(aes(x = Xi, y = df$X3), size = 0.1) +
        geom_point(aes(x = Xi, y = df$X4), size = 0.1) +
        geom_point(aes(x = Xi, y = df$X5), size = 0.1) +
        geom_point(aes(x = Xi, y = df$X6), size = 0.1) +
        geom_point(aes(x = Xi, y = df$X7), size = 0.1)
}

view_design(psi)

# Function to compute the orthogonal
# projection estimator (with a trig
# basis)
OP_trig <- function(M) {
    
    # Create design matrix with M columns
    psi <- design(M)
  
    # Compute theta hat least-squares vector via
    # matrix multiplication of psi transpose and Y
    # (don't forget to divide by n)
    theta_hat <- t(psi) %*% Yi / n
    
    # Construct the projection estimator g_hat
    # via matrix multiplication of psi and
    # theta_hat
    g_hat <- psi %*% theta_hat

    return(g_hat)
}

# Create a scatter-plot of the observations
# Y_i and overlay the estimated regression
# function hat(g)^OP (OP stands for orthogonal
# projection)

M <- 10
psi <- design(M)
g_hat <- OP_trig(M)
ggplot(NULL) + 
    geom_point(aes(x = Xi, y = Yi), size = 0.5) + 
    geom_line(aes(x = Xi, y = g_hat, color = "trig")) +
    labs(x = bquote(~ X[i]), y = bquote(~ Y[i])) +
    scale_color_manual(name = "",
                       values = c("trig" = "purple"))
  

##
# POLYNOMIAL BASIS
#
M_poly <- 13
psi_poly <- poly(Xi, degree = M_poly, raw = TRUE)
model_poly <- lm(Yi ~ psi_poly)
g_hat_poly <- predict(model_poly, newdata = data.frame(Xi = Xi))

##
# NATURAL SPLINES BASIS
#
M_ns <- 10
psi_ns <- ns(Xi, df = M_ns)
model_ns <- lm(Yi ~ psi_ns)
g_hat_ns <- predict(model_ns, newdata = data.frame(Xi = Xi))

##
# B-SPLINES BASIS
#
M_bs <- 13
psi_bs <- bs(Xi, df = M_bs, degree = 3)
model_bs <- lm(Yi ~ psi_bs)
g_hat_bs <- predict(model_bs, newdata = data.frame(Xi = Xi))

# Combine data into a single data frame
df <- data.frame(Xi = rep(Xi, 3),
                 g_hat = c(g_hat_poly, g_hat_ns, g_hat_bs),
                 Model = factor(rep(c("poly", "ns", "bs"), each = length(Xi))))

# Plot all regression functions on a single plot
ggplot(NULL) +
  geom_point(aes(x = Xi, y = Yi, color = "black"), size = 0.5) +
  geom_line(data = df, aes(x = Xi, y = g_hat, color = Model)) +
  labs(x = bquote(~ X[i]),
       y = bquote(~ hat(g))) +
  scale_color_manual(name = "",
                     values = c("poly" = "red",
                                "ns" = "blue", 
                                "bs" = "green"))
  


##
## EXTRA CREDIT:

weights <- function(i, psi, str){
  w <- (psi %*% t(psi) / dim(psi)[1])[i, ]
  plot <- ggplot(NULL) + 
    geom_point(aes(x = Xi, y = w), size = 0.5) +
    geom_point(aes(x = Xi[i], y = 0), color = "red") + 
    labs(x = bquote(~ X[i]),
         y = "weights",
         title = str) +
    theme(plot.title = element_text(hjust = 0.5))
  
  sum <- sum(w)
  print(paste("Sum of weights = ", sum))
  dp <- t(matrix(w)) %*% Yi
  print(paste("g_hat(X_i)", dp))
  print(paste("Y_i", Yi[i]))
  return(plot)
}

# Trig basis
view_design(psi)
weights(10, psi, bquote("trig:" ~ X[10]))
weights(100, psi, bquote("trig:" ~ X[100]))

# Poly basis (modify to include intercept now)
view_design(psi_poly)
psi_poly <- poly(Xi, degree = 7, raw = FALSE)
view_design(psi_poly)
weights(10, psi_poly, bquote("poly:" ~ X[10]))
weights(100, psi_poly, bquote("poly:" ~ X[100]))

# N splines basis
view_design(psi_ns)
weights(10, psi_ns, bquote("ns:" ~ X[10]))
weights(100, psi_ns, bquote("ns:" ~ X[100]))

# B splines basis
view_design(psi_bs)
weights(10, psi_bs, bquote("bs:" ~ X[10]))
weights(100, psi_bs, bquote("bs:" ~ X[100]))

# Nadaraya-Watson
bw1 <- npregbw(xdat = sample$Xi, ydat = sample$Yi, regtype = "lc")
ghat1 <- npreg(bw1, regtype = "lc")
Yhat <- ghat1$mean

ggplot(NULL) + 
  geom_point(aes(x = sample$Xi, y = sample$Yi)) + 
  geom_point(aes(x = sample$Xi, y = Yhat), color = "green")

             