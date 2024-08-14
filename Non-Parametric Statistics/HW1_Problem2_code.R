# Load library
library(dplyr)
library(ggplot2)
library(reshape2)
library(readr)


# Load data
setwd("C:/Users/Katie Overengland/Desktop/STATS 527/Hw 1")
data_med <- read_csv("Medical_Cost_Biv.csv")

# Plot data
ggplot(data_med) +
  geom_point(aes(y = charges, x = bmi))

# Define dense grid onto which we evaluate our KDEs
# They span the possible values of Y (charges) and X (bmi)
x_grid <- seq(min(data_med$bmi), max(data_med$bmi), length = 100)
y_grid <- seq(min(data_med$charges), max(data_med$charges), length = 100)


# Define the 1D Gaussian Kernel Function
gaussian_kernel_1d <- function(x) {
  # ... your code goes here
  exp(- x^2 / 2) / sqrt(2 * pi)
}

# Define the 2D Gaussian Kernel Function
gaussian_kernel_2d <- function(x, y) {
  # ... your code goes here
  gaussian_kernel_1d(x) * gaussian_kernel_1d(y)
}



### Compute KDE for the X variable (bmi)

# Bandwidth (Smoothing parameter)
h_3 = 3
# Initialize vector to store density estimates of X
density_x <- matrix(0, nrow = length(x_grid), ncol = 1)
# Calculate density estimate for each grid point
for (i in 1:length(x_grid)) {
  # Sum the Gaussian kernel contributions from each data point
  for (k in 1:nrow(data_med)) {
    density_x[i] <- density_x[i] + gaussian_kernel_1d((x_grid[i] - data_med$bmi[k]) / h_3) / (h_3 * nrow(data_med)) #... your code goes here
  }
}

### Compute KDE for the (Y,X) variables (charges,bmi)

# Bandwidths (Smoothing parameter)
h_1 = 3
h_2 = 3

# Initialize matrix to store density estimates of (X,Y)
density_xy <- matrix(0, nrow = length(x_grid), ncol = length(y_grid))

# Calculate density estimate of (Y,X) for each grid point
for (i in 1:length(x_grid)) {
  for (j in 1:length(y_grid)) {
    # Sum the Gaussian kernel contributions from each data point
    for (k in 1:nrow(data_med)) {
      density_xy[i, j] <- density_xy[i, j] + gaussian_kernel_2d((x_grid[i] - data_med$bmi[k]) / h_2, (y_grid[j] - data_med$charges[k]) / h_1) / (h_1 * h_2 * nrow(data_med)) # ... your code goes here
    }
  }
}


# 2.4 plot: density estimate of variable X
ggplot(NULL) +
  #geom_point(aes(y = 0, x = data_med$bmi_grid), shape = 4) +
  geom_line(aes(y = density_x, x = x_grid)) +
  labs(x="bmi",
       y="estimated density",
       title = "1D KDE with Gaussian, h=3") +
  theme(text = element_text(family = 'Fira Sans'),
        plot.title = element_text(hjust = 0.5),
        panel.grid = element_blank())



# Plot density estimate of variables (Y,X)
# Unfortunately ggplot2 doesn't allow us to plot a matrix directly,
# so we need to convert in the form of (value, position_x,position_y)
density_xy_coord <- melt(density_xy, value.name = "value")
density_xy_coord$bmi_grid = x_grid[density_xy_coord$Var1]
density_xy_coord$charges_grid = y_grid[density_xy_coord$Var2]

# ... your code goes here. You can use ggplot2::geom_raster

# 2.5 plot: 2D KDE
ggplot(density_xy_coord, aes(density_xy_coord$bmi_grid, density_xy_coord$charges_grid)) +
  geom_raster(aes(fill = density_xy_coord$value)) +
  scale_fill_distiller(palette = "Spectral", direction = -1) +
  labs(x="bmi",
       y="charges",
       title = bquote("2D KDE with Gaussian," ~ h[1] ~ "=" ~ h[2] ~ "=" ~ 3),
       fill = "density") +
  theme(text = element_text(family = 'Fira Sans'),
        plot.title = element_text(hjust = 0.5),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "transparent"))
  

### Compute the density estimate of (Y|X) for a grid of values of X

density_y_cond_x <- matrix(0, nrow = length(x_grid), ncol = length(y_grid)) 
# Compute conditional density from full and marginal densities
for (i in 1:length(x_grid)) {
  for (j in 1:length(y_grid)) {
    density_y_cond_x[i, j] <- density_xy[i, j] / density_x[i] #... your code goes here
  }
}

### Plot density estimate of (Y|X) for a small, medium and large value of $X$

sml = c(10, 40, 70) # vector to specify indices for small, medium, and large $x$


# 2.6 plots:

# Small $x$
ggplot(NULL) +
  geom_line(aes(y = density_y_cond_x[sml[1],], x = y_grid)) + 
  labs(x="charges",
       y="density",
       title = bquote("Conditional density, bmi =" ~ .(round(x_grid[sml[1]], 0)))) + 
  theme(text = element_text(family = 'Fira Sans'),
        plot.title = element_text(hjust = 0.5),
        panel.grid = element_blank())

# Medium $x$
ggplot(NULL) +
  geom_line(aes(y = density_y_cond_x[sml[2],], x = y_grid)) + 
  labs(x="charges",
       y="density",
       title = bquote("Conditional density, bmi =" ~ .(round(x_grid[sml[2]], 0)))) + 
  theme(text = element_text(family = 'Fira Sans'),
        plot.title = element_text(hjust = 0.5),
        panel.grid = element_blank())

# Large $x$
ggplot(NULL) +
  geom_line(aes(y = density_y_cond_x[sml[3],], x = y_grid)) + 
  labs(x="charges",
       y="density",
       title = bquote("Conditional density, bmi =" ~ .(round(x_grid[sml[3]], 0)))) + 
  theme(text = element_text(family = 'Fira Sans'),
        plot.title = element_text(hjust = 0.5),
        panel.grid = element_blank())



### Plot the data and overlay the 0.1-, 0.5-, and 0.9-quantiles as a function of $X$.
# Given a density function (density) evaluated on a uniform dense grid (grid), the following function returns its prob-quantile
get_quantile <- function(density, grid, prob)
{
  cdata_med_prob = cumsum(density)/sum(density)
  return(grid[which.max(cdata_med_prob > prob)])
}

# ... your code goes here

# Initialize vectors to store quantiles
quantile_0.1 <- matrix(0, nrow = length(x_grid), ncol = 1)
quantile_0.5 <- matrix(0, nrow = length(x_grid), ncol = 1)
quantile_0.9 <- matrix(0, nrow = length(x_grid), ncol = 1)

# Calculate each quantile for a dense set of x values
for (i in 1:length(x_grid)) {
  quantile_0.1[i] <- get_quantile(density_y_cond_x[i,], y_grid, 0.1)
  quantile_0.5[i] <- get_quantile(density_y_cond_x[i,], y_grid, 0.5)
  quantile_0.9[i] <- get_quantile(density_y_cond_x[i,], y_grid, 0.9)
}

# 2.7 plot: original data overlaid with quantiles
ggplot(NULL) +
  geom_point(data = data_med, aes(y = charges, x = bmi)) +
  geom_line(aes(y = quantile_0.1, x = x_grid, color = "Quantile 0.1")) +
  geom_line(aes(y = quantile_0.5, x = x_grid, color = "Quantile 0.5")) +
  geom_line(aes(y = quantile_0.9, x = x_grid, color = "Quantile 0.9")) +
  labs(x = "bmi",
       y = "charges") +
  scale_color_manual(values = c("Quantile 0.1" = "blue", 
                                "Quantile 0.5" = "green", 
                                "Quantile 0.9" = "red"),
                     labels = c("0.1", "0.5", "0.9"),
                     name = "quantile") +
  theme(text = element_text(family = "Fira Sans"),
        plot.title = element_text(hjust = 0.5))
        #panel.grid = element_blank())



