### LOAD PACKAGES

library(dplyr)
library(tidyverse)
library(fauxnaif)
library(summarytools)
library(psych)
library(DescTools)
library(MplusAutomation)

###########################################
#########  PLOT For internalizing #########
###########################################


# Define the cubic regression equation with intercept
cubic_regression <- function(x, b0, b1, b2, b3) {
  y <- b0 + b1 * x + b2 * x^2 + b3 * x^3
  return(y)
}

# Known coefficients
b0 <- 0.655  # Intercept
b1 <-  0.100  # X
b2 <- 0.090 # X^2
b3 <- -0.140 # X^3

### Generate ordered data within the range. 
#I restricted the upper range to 2 SD above the mean! 
x_values <- seq(-0.760, 1.20, by = 0.001)  # Adjust min_range and max_range 
x_values <- sort(x_values) 

# Calculate corresponding y values using the cubic regression equation
y_values <- cubic_regression(x_values, b0, b1, b2, b3)

## Set to Times New Roman
par(family = "serif", cex.lab = 1.75, cex.axis = 1.75)
# Plot the cubic regression with grid

#Remove Title
plot(x_values, y_values, type = "n", xlab = "Family Threat (T1)", ylab = "Internalizing Symptoms (T5)", font.lab = 2, cex.main = 2)

#plot(x_values, y_values, type = "n", xlab = "Family Threat (T1)", ylab = "Internalizing Symptoms (T5)", main = "The Influence of Family Threat 
#     on Youth Internalizing Symptoms", font.lab = 2, cex.main = 2)

# Add grid
grid()

# Add vertical lines at mean
abline(v = 0, col = "darkgrey", lty = 4)


################################################
######## Determine Inflection Points ###########
################################################

# Create functions for usage 


# Function to calculate second derivative of cubic regression
cubic_second_derivative <- function(x, b2, b3) {
  dy2_dx2 <- 2 * b2 + 6 * b3 * x
  return(dy2_dx2)
}


# Define a function to find x for a given y using uniroot
find_x_for_y <- function(target_y, coefficients) {
  uniroot(function(x) cubic_regression(x, coefficients[1], coefficients[2], coefficients[3], coefficients[4]) - target_y,
          interval = c(-10, 10))$root
}


# Save lowest and highest values for plots 
LOWBD <- min(x_values)
HIBD <- max(x_values)

## HORMETIC INFLECTION POINT (STATISTICAL)t 


# Find all inflection points using uniroot iteratively
inflection_points <- numeric(0)

for (i in seq_along(x_values)) {
  root <- try(uniroot(function(x) cubic_second_derivative(x, b2, b3), 
                      interval = c(x_values[i] - 0.1, x_values[i] + 0.1))$root, silent = TRUE)
  
  if (!inherits(root, "try-error")) {
    inflection_points <- c(inflection_points, root)
  }
}

# Print the mid inflection point
cat("Inflection Points:", inflection_points, "\n")## Find the lowest and highest points of the predicted cubic slope 
## Mid inflection  = .214

# Make Objects for Plotting

# Mid-Inflection X, and Mid-Inflection y (adjust brackets if more than 1)
MID_INF <-  inflection_points[1]
MID_INFy <- cubic_regression(MID_INF, b0, b1, b2, b3)

################### Hormetic Zone Vertex

# Subset the data for X values below inflection
subset_below_zero <- which(x_values < MID_INF)

# Find the minimum Y value below inflection
HORM_INFy <- min(y_values[subset_below_zero], na.rm = TRUE)
HORM_INFy
# Y = 0.636

# Find the index of the minimum observed Y value below inflection
index_min_observed_below_zero <- which.min(y_values[subset_below_zero])

# Extract the accompanying X value for lowest Y 
HORM_INF <- x_values[subset_below_zero][index_min_observed_below_zero]
HORM_INF
# X = -0.32


################### TOXIC Zone Vertex

# Subset the data for X values above inflection
subset_above_zero <- which(x_values > MID_INF)

# Find the maximum observed Y value above inflection
Tox_INFy <- max(y_values[subset_above_zero], na.rm = TRUE)
Tox_INFy
#Y = 0.7215625

# Find the index of the maximum observed Y value above inflection
index_max_observed_above_zero <- which.max(y_values[subset_above_zero])

# Extract the accompanying X value
Tox_INF <- x_values[subset_above_zero][index_max_observed_above_zero]
Tox_INF
# X = .75


#######################################################
#######  CONCEPTUAL HORMETIC INFLECTION POINT ######### 
#######################################################

## Find y for the most extreme low strengthing value

STR_High_y <- cubic_regression(LOWBD, b0, b1, b2, b3)
STR_High_y
# y = .6924406


## Find all the intersection points of the target y (STR_High_y)

# Define a tolerance level for comparing y-values
tolerance <- 0.0001  # Adjust as needed

# Initialize a vector to store the x-values corresponding to the target y-value
x_values_target_y <- numeric()

# Iterate through the x-values and check for the target y-value
for (i in 1:length(x_values)) {
  if (abs(y_values[i] - STR_High_y) < tolerance) {
    x_values_target_y <- c(x_values_target_y, x_values[i])
  }
}

# Print the x-values associated with the target y-value
print(x_values_target_y)

## -0.760  0.327  1.076

## Define a new point for shading that is the intersection point for the middle of
## the cubic curve

CONmidPOINT <- 0.327


## Locate the X value at which the toxic zone ends (on extreme end) - 
# aligns with Y conceptual mid-inflection 

TOXIC_END_X <- find_x_for_y(STR_High_y, c(b0, b1, b2, b3))
TOXIC_END_X
# X = 1.075751

###----------------- ADD SHADING TO PLOT ------------------------###

##### Strengthening (Change -2 to the lowest Y value on the plot)
x_darker_blue <- seq(LOWBD, HORM_INF, by = 0.01)
y_darker_blue <- cubic_regression(x_darker_blue, b0, b1, b2, b3)
bottom_points_blue <- c(LOWBD, x_darker_blue, HORM_INF)
bottom_values_blue <- c(cubic_regression(LOWBD, b0, b1, b2, b3), y_darker_blue, cubic_regression(HORM_INF, b0, b1, b2, b3))
polygon(c(bottom_points_blue, rev(bottom_points_blue)), c(rep(.63, length(bottom_points_blue)), rev(bottom_values_blue)), col = rgb(0.0, 0.0, 1.0, 0.5))

##### Buffering 
x_darker_purple <- seq(HORM_INF, CONmidPOINT, by = 0.01)
y_darker_purple <- cubic_regression(x_darker_purple, b0, b1, b2, b3)
bottom_points_purple <- c(HORM_INF, x_darker_purple, CONmidPOINT)
bottom_values_purple <- c(cubic_regression(HORM_INF, b0, b1, b2, b3), y_darker_purple, cubic_regression(CONmidPOINT, b0, b1, b2, b3))
polygon(c(bottom_points_purple, rev(bottom_points_purple)), c(rep(.63, length(bottom_points_purple)), rev(bottom_values_purple)), col = rgb(0.5, 0.2, 0.9, 0.5))

# TOXIC ZONE
x_light_red <- seq(CONmidPOINT, TOXIC_END_X, by = 0.001)
y_light_red <- cubic_regression(x_light_red, b0, b1, b2, b3)

# Define vertices for the polygon
bottom_points_red <- c(CONmidPOINT, x_light_red, TOXIC_END_X)
bottom_values_red <- pmax(cubic_regression(TOXIC_END_X, b0, b1, b2, b3), y_light_red, STR_High_y)

# Trim vectors to ensure the same length
min_length <- min(length(bottom_points_red), length(bottom_values_red))
bottom_points_red <- head(bottom_points_red, min_length)
bottom_values_red <- head(bottom_values_red, min_length)

# Create the polygon using vertices 
polygon(c(bottom_points_red, rev(bottom_points_red)), c(rep(STR_High_y, length(bottom_points_red)), rev(bottom_values_red)), col = rgb(1.0, 0.6, 0.6, 0.5))

# HORMETIC ZONE (replace -2 with the value of y at the x axis)
x_light_green <- seq(LOWBD, CONmidPOINT, by = 0.001)
y_light_green <- pmax(-2, cubic_regression(x_light_green, b0, b1, b2, b3))

# Define vertices for the polygon
top_left_corner <- c(LOWBD, cubic_regression(LOWBD, b0, b1, b2, b3))
top_right_corner <- c(CONmidPOINT, cubic_regression(CONmidPOINT, b0, b1, b2, b3))

# Create the polygon using vertices
polygon(c(top_left_corner[1], x_light_green, top_right_corner[1]),
        c(top_left_corner[2], y_light_green, top_right_corner[2]),
        col = rgb(0.0, 0.8, 0.0, 0.5),
        density = 15)  # Adjust the density value for the pattern


# Add the cubic regression line with improved line style
lines(x_values, y_values, col = "darkblue", lwd = 4, lty = 1)



########################################
## Get percentages based on cut points #
########################################

file.choose()
df <- read.csv("SPECIAL_ISSUE_HORMESIS_1.27.24_V3.csv")
names(df)

# Center the NewThreat Variable

df$CONcen1 <- (df$newThrt - mean(df$newThrt, na.rm = TRUE))
mean(df$CONcen1, na.rm = TRUE)

## Get counts of each section

TOTAL_NUM_THR <- sum(!is.na(df$CONcen1))
TOTAL_NUM_THR
#11,872

# Strengthening Category 
Strength_NUM_THR <-  sum(!is.na(df$CONcen1) & df$CONcen1 < HORM_INF)
Strength_NUM_THR

Strength_NUM_THR/TOTAL_NUM_THR
# 33% (n = 3,860)

# Buffering Category 
BUFFER_NUM_THR <- sum(!is.na(df$CONcen1) & df$CONcen1 < CONmidPOINT & df$CONcen1 > HORM_INF)
BUFFER_NUM_THR
#4,749

BUFFER_NUM_THR/TOTAL_NUM_THR
# 40% (n = 4,749)

# Toxic Category 
TOXIC_NUM_THR <- sum(!is.na(df$CONcen1) & df$CONcen1 > CONmidPOINT)
TOXIC_NUM_THR

TOXIC_NUM_THR/TOTAL_NUM_THR
# 27% (n = 3,263)

#Hormetic Zone Calcs
TotalHORM <- Strength_NUM_THR + BUFFER_NUM_THR
TotalHORM
#8,609

#Hormetic Percentage
TotalHORM/TOTAL_NUM_THR
# 72%

#Confirm sample size
TOXIC_NUM_THR + BUFFER_NUM_THR + Strength_NUM_THR
# 11872


# Adjusted legend with increased text size and adjusted box size
legend("bottomright", legend = c("Strengthening Region 33% (n = 3,860)", "Buffering Region 40% (n = 4,749)", "Toxic Zone 27% (n = 3,263)", "Hormetic Zone 72% (n = 8,609) "),
       fill = c(rgb(0.0, 0.0, 1.0, 0.5), rgb(0.5, 0.2, 0.9, 0.5), rgb(1.0, 0.6, 0.6, 0.5), rgb(0.0, 0.8, 0.0, 0.5)),
       title = "Legend",
       text.font = 2,  # Use text.font to specify bold text
       cex = 1)      # Adjust the size of the legend box

#### Calculate Z scores
# Mean
THRmean <- mean(df$CONcen1, na.rm = TRUE)
THRmean
#.04
# SD 
THRsd <- sd(df$CONcen1, na.rm = TRUE)
THRsd
#.62

#Hormetic zONE VERTEX (Threshold) (x = -.32)

((-.32) - THRmean)/THRsd
# z = -.51

#Conceptual HOREMTIC Inflection (x = .327)
((.327) - THRmean)/THRsd
# z = .53

#Statistical HORMETIC Inflection (x = .21)

((.21) - THRmean)/THRsd
# z = .34

# Add additional dots
points(HORM_INF, cubic_regression(HORM_INF, b0, b1, b2, b3), col = "black", pch = 19, cex = 1.75)
points(MID_INF, cubic_regression(MID_INF, b0, b1, b2, b3), col = "blue", pch = 19, cex = 1.75)
points(Tox_INF, cubic_regression(Tox_INF, b0, b1, b2, b3), col = "black", pch = 19, cex = 1.75)
points(CONmidPOINT,STR_High_y, col = "black", pch = 19, cex = 1.75)

text(.05, .68, "Statistical 
Hormetic Inflection: 
     .21 (Z = .34)")

text(-.31, .645, "Hormetic Zone Vertex: 
     -.32 (Z = -.51)")

text(.18, .697, "Conceptual 
Hormetic Inflection: 
     .33 (Z = .53)")

text(.75, .713, "Toxic Zone Vertex: 
     .75 (Z = 1.21)")

#####################################################
###### Create variables for moderation analysis #####
#####################################################

options(scipen = 999, digits = 10)


# Create Groups based on groups

df$INTconSP <- cut(df$CONcen1,
                        breaks = c(-Inf, -0.32, 0.327, Inf),
                        labels = c(1, 2, 3),
                        include.lowest = TRUE)

# 1 = Lower third (strengthening)
# 2 = middle values (Buffering)
# 3 = Highest Third (toxic)

#Check percentages
table(df$INTconSP)

#####################################
### Plot the externalizing effect ###
#####################################

#Descriptives
hist(df$new_DEP, na.rm = TRUE)
sd(df$new_DEP, na.rm = TRUE)
min(df$new_DEP, na.rm = TRUE)
max(df$new_DEP, na.rm = TRUE)
.711*3 

# Define the cubic regression equation with intercept
cubic_regression <- function(x, b0, b1, b2, b3) {
  y <- b0 + b1 * x + b2 * x^2 + b3 * x^3
  return(y)
}

# Known coefficients
b0 <- 0.643  # Intercept 
b1 <-  0.221  # X
b2 <- -0.008 # X^2
b3 <- -0.054 # X^3

### Generate ordered data within the observed data range. 
#I restricted the lower range to -3 SD because of low N and extreme data patterns
x_values_SES <- seq(-2.13, 1.724, by = 0.001)  # Adjust min_range and max_range 
x_values_SES <- sort(x_values_SES)

# Calculate corresponding y values using the cubic regression equation
y_values_SES <- cubic_regression(x_values_SES, b0, b1, b2, b3)

## Set to Times New Roman
par(family = "serif", cex.lab = 1.75, cex.axis = 1.75)
# Plot the cubic regression with grid

plot(x_values_SES, y_values_SES, type = "n", xlab = "Deprivation (T1)", ylab = "Externalizing Symptoms (T5)", font.lab = 2, cex.main = 2)


#plot(x_values_SES, y_values_SES, type = "n", xlab = "Deprivation (T1)", ylab = "Externalizing Symptoms (T5)", main = "The Influence of Deprivation 
#     on Youth Externalizing Symptoms", font.lab = 2, cex.main = 2)

# Add grid
grid()

# Add vertical lines at mean
abline(v = 0, col = "darkgrey", lty = 4)

################################################
######## Determine Inflection Points ###########
################################################

# Create Functions 

# Function to calculate second derivative of cubic regression
cubic_second_derivative <- function(x, b2, b3) {
  dy2_dx2 <- 2 * b2 + 6 * b3 * x
  return(dy2_dx2)
}

# Define a function to find x for a given y using uniroot
find_x_for_y <- function(target_y, coefficients) {
  uniroot(function(x) cubic_regression(x, coefficients[1], coefficients[2], coefficients[3], coefficients[4]) - target_y,
          interval = c(-10, 10))$root
}

# Save lowest and highest values for plots 
LOWBD <- min(x_values_SES)
HIBD <- max(x_values_SES)

################### Hormetic inflection point (Statistical)

# Find all inflection points using uniroot iteratively
inflection_points <- numeric(0)

for (i in seq_along(x_values_SES)) {
  root <- try(uniroot(function(x) cubic_second_derivative(x, b2, b3), 
                      interval = c(x_values_SES[i] - 0.1, x_values_SES[i] + 0.1))$root, silent = TRUE)
  
  if (!inherits(root, "try-error")) {
    inflection_points <- c(inflection_points, root)
  }
}

# Print the mid inflection point
cat("Inflection Points:", inflection_points, "\n")## Find the lowest and highest points of the predicted cubic slope 
## Mid inflection  = -.049

# Make Objects for Plotting

# Mid-Inflection X, and Mid-Inflection y (adjust brackets if more than 1)
MID_INF <-  inflection_points[1]
MID_INFy <- cubic_regression(MID_INF, b0, b1, b2, b3)
MID_INF
################### Hormetic Zone Vertex (Threshold)

# Subset the data for X values below inflection
subset_below_zero <- which(x_values_SES < MID_INF)

# Find the minimum Y value below inflection
HORM_INFy <- min(y_values_SES[subset_below_zero], na.rm = TRUE)
HORM_INFy
# Y = 0.459

# Find the index of the minimum observed Y value below inflection
index_min_observed_below_zero <- which.min(y_values_SES[subset_below_zero])

# Extract the accompanying X value for lowest Y 
HORM_INF <- x_values_SES[subset_below_zero][index_min_observed_below_zero]
HORM_INF
# X = -1.218


################### TOXIC Zone Vertex (Threshold)

# Subset the data for X values below inflection
subset_above_zero <- which(x_values_SES > MID_INF)

# Find the maximum observed Y value below inflection
Tox_INFy <- max(y_values_SES[subset_above_zero], na.rm = TRUE)
Tox_INFy
#Y = 0.80

# Find the index of the maximum observed Y value below inflection
index_max_observed_above_zero <- which.max(y_values_SES[subset_above_zero])

# Extract the accompanying X value
Tox_INF <- x_values_SES[subset_above_zero][index_max_observed_above_zero]
Tox_INF
# X = 1.12


#######################################################
########  CONCEPTUAL HORMETIC INFLECTION POINT ######## 
#######################################################

## Find y for the most extreme low strengthing value

STR_High_y <- cubic_regression(LOWBD, b0, b1, b2, b3)
STR_High_y
# y = 0.657809038


## Find all the intersection points of the target y (STR_High_y)

# Define a tolerance level for comparing y-values
tolerance <- 0.0001  # Adjust as needed

# Initialize a vector to store the x-values corresponding to the target y-value
x_values_target_y <- numeric()

# Iterate through the x-values and check for the target y-value
for (i in 1:length(x_values_SES)) {
  if (abs(y_values_SES[i] - STR_High_y) < tolerance) {
    x_values_target_y <- c(x_values_target_y, x_values_SES[i])
  }
}

# Print the x-values associated with the target y-value
print(x_values_target_y)

## -2.130  0.067

## Define a new point for shading that is the intersection point for the middle of
## the cubic curve

CONmidPOINT <- 0.067




###----------------- ADD SHADING TO PLOT ------------------------###

##### Strengthening (Change -2 to the lowest Y value on the plot)
x_darker_blue <- seq(LOWBD, HORM_INF, by = 0.01)
y_darker_blue <- cubic_regression(x_darker_blue, b0, b1, b2, b3)
bottom_points_blue <- c(LOWBD, x_darker_blue, HORM_INF)
bottom_values_blue <- c(cubic_regression(LOWBD, b0, b1, b2, b3), y_darker_blue, cubic_regression(HORM_INF, b0, b1, b2, b3))
polygon(c(bottom_points_blue, rev(bottom_points_blue)), c(rep(.43, length(bottom_points_blue)), rev(bottom_values_blue)), col = rgb(0.0, 0.0, 1.0, 0.5))

##### Buffering 
x_darker_purple <- seq(HORM_INF, CONmidPOINT, by = 0.01)
y_darker_purple <- cubic_regression(x_darker_purple, b0, b1, b2, b3)
bottom_points_purple <- c(HORM_INF, x_darker_purple, CONmidPOINT)
bottom_values_purple <- c(cubic_regression(HORM_INF, b0, b1, b2, b3), y_darker_purple, cubic_regression(CONmidPOINT, b0, b1, b2, b3))
polygon(c(bottom_points_purple, rev(bottom_points_purple)), c(rep(.43, length(bottom_points_purple)), rev(bottom_values_purple)), col = rgb(0.5, 0.2, 0.9, 0.5))



# TOXIC ZONE
x_light_red <- seq(CONmidPOINT, HIBD, by = 0.001)
y_light_red <- cubic_regression(x_light_red, b0, b1, b2, b3)

# Define vertices for the polygon
bottom_left_corner <- c(CONmidPOINT, cubic_regression(CONmidPOINT, b0, b1, b2, b3))
bottom_right_corner <- c(HIBD, cubic_regression(HIBD, b0, b1, b2, b3))

# Create the polygon using vertices
polygon(c(bottom_left_corner[1], x_light_red, bottom_right_corner[1]),
        c(bottom_left_corner[2], y_light_red, bottom_right_corner[2]),
        col = rgb(1.0, 0.6, 0.6, 0.5))



# HORMETIC ZONE (replace -2 with the value of y at the x axis)
x_light_green <- seq(LOWBD, CONmidPOINT, by = 0.01)
y_light_green <- pmax(-.4, cubic_regression(x_light_green, b0, b1, b2, b3))

# Define vertices for the polygon
top_left_corner <- c(LOWBD, cubic_regression(LOWBD, b0, b1, b2, b3))
top_right_corner <- c(CONmidPOINT, cubic_regression(CONmidPOINT, b0, b1, b2, b3))

# Create the polygon using vertices
polygon(c(top_left_corner[1], x_light_green, top_right_corner[1]),
        c(top_left_corner[2], y_light_green, top_right_corner[2]),
        col = rgb(0.0, 0.8, 0.0, 0.5),
        density = 15)  # Adjust the density value for the pattern


# Add the cubic regression line with improved line style
lines(x_values_SES, y_values_SES, col = "darkblue", lwd = 4, lty = 1)

# Add additional dots
points(HORM_INF, cubic_regression(HORM_INF, b0, b1, b2, b3), col = "black", pch = 19, cex = 1.75)
points(MID_INF, cubic_regression(MID_INF, b0, b1, b2, b3), col = "blue", pch = 19, cex = 1.75)
points(CONmidPOINT, STR_High_y, col = "black", pch = 19, cex = 1.75)
points(Tox_INF, cubic_regression(Tox_INF, b0, b1, b2, b3), col = "black", pch = 19, cex = 1.75)


text(-.20, .685, "Conceptual 
Hormetic Inflection:
     .07 (Z = .09)") 

text(-1.21,.50, "Hormetic Zone Vertex: 
     -1.22 (Z = -1.71)") 

text(-.40,.63, "Statistical 
Hormetic Inflection:
-.05 (Z = -.07)") 

text(1.1,.772, "Toxic Zone Vertex: 
     1.12 (Z = 1.67)") 

# Load in the observed data 

df <- read.csv("SPECIAL_ISSUE_HORMESIS_1.27.24_V3.csv")

#Create centered SES variable 
df$SES_CEN <- (df$new_DEP - mean(df$new_DEP, na.rm = TRUE))
mean(df$SES_CEN,na.rm = TRUE)

#### Calculate Z scores

# Mean
DEPmean <- mean(df$SES_CEN, na.rm = TRUE)
DEPmean
# SD 
DEPsd <- sd(df$SES_CEN, na.rm = TRUE)
DEPsd
#.71

#Hormetic Threshold (x = -1.218)

((-1.218) - DEPmean)/DEPsd
# z = -1.71

#Conceptual Inflection (x = 0.067)
((0.067) - DEPmean)/DEPsd
# z = 0.09422750172

#Statistical Inflection (x = -0.049)

((-0.049) - DEPmean)/DEPsd
# z = -0.06891265051

########################################
## Get percentages based on cut points #
########################################

## Get counts of each section

TOTAL_NUM_DEP <- sum(!is.na(df$SES_CEN))
TOTAL_NUM_DEP
#11,876

# Strengthening Category 
Strength_NUM_DEP <-  sum(!is.na(df$SES_CEN) & df$SES_CEN < HORM_INF)
Strength_NUM_DEP

Strength_NUM_DEP/TOTAL_NUM_DEP
# 5% (n = 578)

# Buffering Category 
BUFFER_NUM_DEP <- sum(!is.na(df$SES_CEN) & df$SES_CEN < CONmidPOINT & df$SES_CEN > HORM_INF)
BUFFER_NUM_DEP

BUFFER_NUM_DEP/TOTAL_NUM_DEP
# 48% (n = 5,652)

# Toxic Category 
TOXIC_NUM_DEP <- sum(!is.na(df$SES_CEN) & df$SES_CEN > CONmidPOINT)
TOXIC_NUM_DEP

TOXIC_NUM_DEP/TOTAL_NUM_DEP
# 48% (n = 5,646)


#Hormetic Zone 
BUFFER_NUM_DEP + Strength_NUM_DEP
#6,230
(BUFFER_NUM_DEP + Strength_NUM_DEP)/TOTAL_NUM_DEP
#52% (n = 6,230)

TOXIC_NUM_DEP + BUFFER_NUM_DEP + Strength_NUM_DEP
# 11876


# Adjusted legend with increased text size and adjusted box size
legend("bottomright", legend = c("Strengthening Region 5% (n = 578)", "Buffering Region 48% (n = 5,652)", "Toxic Zone 48% (n = 5,646)", "Hormetic Zone 52% (n = 6,230)"),
       fill = c(rgb(0.0, 0.0, 1.0, 0.5), rgb(0.5, 0.2, 0.9, 0.5), rgb(1.0, 0.6, 0.6, 0.5), rgb(0.0, 0.8, 0.0, 0.5)),
       title = "Legend",
       text.font = 2,  # Use text.font to specify bold text
       cex = 1)      # Adjust the size of the legend box


