# Load library
library(dplyr)
library(ggplot2)
library(plotly)
library(reshape2)
library(readr)
library(np)

# Load data
setwd("C:/Users/Katie Overengland/Desktop/STATS 527/Hw 2")
data <- read_table("fev.txt")


## QUESTION 2.1, 2.2
ggplot(data) +
  geom_point(aes(x = data$smoke, y = data$fev))

Y = data$fev
X = -data$smoke + 2 
model <- lm(Y ~ X, data = data)

# Linear model summary:
summary(model)

# Store the regression coefficients from
# the linear model
coeffs <- summary(model)$coefficients
slope <- coeffs[2, 1]
intercept <- coeffs[1, 1]

# Create a data frame with interpolated x-values
# so that we can plot the line of best fit
x_values <- seq(min(X), max(X), length.out = 100)
line_data <- data.frame(smoke = x_values)

# Calculate y-values using the equation of the line (y = mx + b)
line_data$fev <- slope * line_data$smoke + intercept

# Plot the data points and the line
ggplot(data, aes(x = X, y = Y)) +
  geom_point() +  # Plot the data points
  geom_line(data = line_data, aes(x = smoke, y = fev), color = "blue") +
  labs(x = "Smoking status", y = "FEV", title = "FEV vs. Smoking status") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid = element_blank()) +
  scale_x_continuous(breaks = seq(0, 1, by = 1))

## YES there is evidence of an association
## 
## But for one thing that association is
## positive, and for another we cannot 
## conclude causality due to confounding
## variables (height, age)


## QUESTIONS 2.4, 2.5, 2.6

# Assign the height and age columns to the 
# variable Z
Z = cbind(data$height, data$age)

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

## Plot the residuals and the line of best fit

# Store the regression coefficients from
# the linear model
coeffs <- summary(robinson)$coefficients
slope <- coeffs[2, 1]
intercept <- coeffs[1, 1]

# Create a data frame with interpolated x-values
x_values <- seq(min(res2), max(res2), length.out = 100)
line_data <- data.frame(smoke = x_values)

# Calculate y-values using the equation of the line (y = mx + b)
line_data$fev <- slope * line_data$smoke + intercept

# Plot the FEV residuals against the Smoking Status
# residuals. We now observe a negative association
# between FEV and smoking status.
ggplot(NULL) + 
  geom_point(aes(res2, res1)) + 
  geom_line(data = line_data, aes(x = line_data$smoke, y = line_data$fev), 
            color = 'blue') + 
  labs(x = "X - E(X | Z)", y = "Y - E(Y | Z)",
       title = "FEV residuals vs. Smoking Status residuals") + 
  theme(text = element_text(family = "Fira Sans"),
        plot.title = element_text(hjust = 0.5, size = 12),
        panel.grid = element_blank())
        #panel.background = element_rect(fill = "transparent"))
  
  
# Convert Z to a data frame for plotting
Zdf <- data.frame(Z)

# Function to plot E(X|Z) and E(Y|Z)
plotE <- function(Zdf, EXZ, str1, s, hjust, str2, int) {
    p <- ggplot(Zdf, aes(Zdf$X1, Zdf$X2)) +
              geom_point(aes(fill = EXZ), shape = 21, size = 3) +  # Use shape 21 for filled circles
              scale_fill_distiller(palette = "RdYlBu", direction = int) +
              labs(x = "Height",
                   y = "Age",
                   title = bquote("Expected" ~ .(str1) ~ "given Height and Age"),
                   fill = bquote(.(str2))) +
              theme(text = element_text(family = "Fira Sans"),
                    plot.title = element_text(hjust = hjust, size = s),
                    panel.grid = element_blank())
                    #panel.background = element_rect(fill = "transparent"))
    print(p)
    return(NULL)
}

# Plot E(Y|Z)
plotE(Zdf, EYZ, "FEV", 12, 0.5, "FEV", 1)

# Plot E(X|Z)
plotE(Zdf, EXZ, "Smoking Status", 10, 0.5, "Pr(Smoke)", -1)
