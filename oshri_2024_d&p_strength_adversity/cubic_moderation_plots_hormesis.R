### LOAD PACKAGES

library(dplyr)
library(tidyverse)
library(fauxnaif)
library(summarytools)
library(psych)
library(DescTools)
library(MplusAutomation)
library(ggplot2)
library(Cairo)


df <- read.csv("SPECIAL_ISSUE_RESUB_2.12.24_V3.csv")


###########################################
####### Cubic Moderation Analysis #########
###########################################


# Define the cubic regression equation with intercept
cubic_regression <- function(x, m, b0, b1, b2, b3, b4, b5, b6, b7) {
  y <- b0 + b1 * x + b2 * x^2 + b3 * x^3 + b4 * m + b5 * (x * m) + b6 * (x^2 * m) +
    b7 * (x^3 * m)
  return(y)
}


# Known coefficients
b0 <- 0.666  # intercept
b1 <- 0.121  # x
b2 <- 0.101  # x^2
b3 <- -0.149  # x^3
b4 <- 0.987  # m
b5 <- -1.673  # INTL
b6 <- -4.313  # INTQ
b7 <- 3.772  # INTC

### Generate ordered data within the range.
# I restricted the upper range to 2 SD above the mean!
x_values <- seq(-0.760, 1.26, by = 0.01)  # Adjust min_range and max_range of x
x_values <- sort(x_values)
m_values <- seq(-0.203, 0.236, by = 0.01)  # Adjust min_range and max_range of moderator
m_values <- sort(m_values)


# Use mapply to apply the cubic_regression function element-wise
y_values <- mapply(cubic_regression, x = x_values, m = 0,  # Set moderator at the mean 
                          b0 = b0, b1 = b1, b2 = b2, b3 = b3,
                          b4 = b4, b5 = b5, b6 = b6, b7 = b7)

y_values_LSD <- mapply(cubic_regression, x = x_values, m = -.04,  # set moderator at -1 SD
                   b0 = b0, b1 = b1, b2 = b2, b3 = b3,
                   b4 = b4, b5 = b5, b6 = b6, b7 = b7)

y_values_HSD <- mapply(cubic_regression, x = x_values, m = .04,  # Set moderator at +1 SD 
                   b0 = b0, b1 = b1, b2 = b2, b3 = b3,
                   b4 = b4, b5 = b5, b6 = b6, b7 = b7)

# Use smooth.spline to generate a smoother curve
AVG_curve <- smooth.spline(x_values, y_values)
LOW_curve <- smooth.spline(x_values, y_values_LSD)
HI_curve <- smooth.spline(x_values, y_values_HSD)


# Set to Times New Roman
par(family = "serif", cex.lab = 1.75, cex.axis = 1.75)

# Plot the cubic regression with grid

plot(x_values, y_values_LSD, type = "n", xlab = "Family Threat (T1)", ylab = "Internalizing Symptoms (T5)",
font.lab = 2, cex.main = 2)

#plot(x_values, y_values_LSD, type = "n", xlab = "Family Threat (T1)", ylab = "Internalizing Symptoms (T5)", main = "Cubic Family Threat x Change in DMN Coherence 
#Predicting Youth Internalizing Problems - Full Sample Simple Slopes", font.lab = 2, cex.main = 2)

# Add grid
grid()


# Add the cubic regression line with improved line style
lines(AVG_curve, col = "darkblue", lwd = 2)
lines(LOW_curve, col = "red", lwd = 2)
lines(HI_curve, col = "purple", lwd = 2)

# Add vertical lines at mean
abline(v = 0, col = "darkgray", lty = 4)


# Adjusted legend with increased text size and adjusted box size
legend("bottomright", legend = c("Average DMN Latent Change", "-1 SD DMN Latent Change**", "+1 SD DMN Latent Change"),
       fill = c("darkblue", "red", "purple") ,
       title = "Legend",
       text.font = 2,  # Use text.font to specify bold text
       cex = 1)      # Adjust the size of the legend box


### TEST MY EFFECTS 

# Define the cubic regression equation with intercept
cubic_regression <- function(x, m, b0, b1, b2, b3, b4, b5, b6, b7) {
  y <- b0 + b1 * x + b2 * x^2 + b3 * x^3 + b4 + b5 * (x * m) + b6 * (x^2 * m) +
    b7 * (x^3 * m)
  return(y)
}


# Known coefficients
b0 <- 0.666  # intercept
b1 <- 0.121  # x
b2 <- 0.101  # x^2
b3 <- -0.149  # x^3
b4 <- 0.987  # m
b5 <- -1.673  # INTL
b6 <- -4.313  # INTQ
b7 <- 3.772  # INTC

### Generate ordered data within the range.
#  2 SD above the mean bounds
x_values <- seq(-0.760, 1.26, by = 0.01)  # Adjust min_range and max_range of x
x_values <- sort(x_values)
m_values <- seq(-0.203, 0.236, by = 0.01)  # Adjust min_range and max_range of moderator
m_values <- sort(m_values)


# Use mapply to apply the cubic_regression function element-wise
y_values <- mapply(cubic_regression, x = x_values, m = 0,  # Set moderator at the mean 
                   b0 = b0, b1 = b1, b2 = b2, b3 = b3,
                   b4 = b4, b5 = b5, b6 = b6, b7 = b7)

y_values_LSD <- mapply(cubic_regression, x = x_values, m = -.04,  # set moderator at -1 SD
                       b0 = b0, b1 = b1, b2 = b2, b3 = b3,
                       b4 = b4, b5 = b5, b6 = b6, b7 = b7)

y_values_HSD <- mapply(cubic_regression, x = x_values, m = .04,  # Set moderator at +1 SD 
                       b0 = b0, b1 = b1, b2 = b2, b3 = b3,
                       b4 = b4, b5 = b5, b6 = b6, b7 = b7)

# Use smooth.spline to generate a smoother curve
AVG_curve <- smooth.spline(x_values, y_values)
LOW_curve <- smooth.spline(x_values, y_values_LSD)
HI_curve <- smooth.spline(x_values, y_values_HSD)


# Set to Times New Roman
par(family = "serif", cex.lab = 1.75, cex.axis = 1.75)

# Plot the cubic regression with grid
plot(x_values, y_values_LSD, type = "n", xlab = "Family Threat (T1)", ylab = "Internalizing Symptoms (T5)", main = "Influence of Family Threat on Youth Internalizing 
Moderated by Default Mode Network Coherence - Simple Slopes", font.lab = 2, cex.main = 2)

# Add grid
grid()


# Add the cubic regression line with improved line style
lines(AVG_curve, col = "darkblue", lwd = 2)
lines(LOW_curve, col = "red", lwd = 2)
lines(HI_curve, col = "purple", lwd = 2)

# Add vertical lines at mean
abline(v = 0, col = "black", lty = 2)

# Adjusted legend with increased text size and adjusted box size
legend("bottomright", legend = c("Average DMN Latent Change", "-1 SD DMN Latent Change", "+1 SD DMN Latent Change"),
       fill = c("darkblue", "red", "purple") ,
       title = "Legend",
       text.font = 2,  # Use text.font to specify bold text
       cex = 1)      # Adjust the size of the legend box

# Add Stars for significance

text(.25, .67, "*", col = "black")


