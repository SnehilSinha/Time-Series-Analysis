# Read training data
setwd("/Users/annahusth/Documents/Studie/Universitet/DTU/Spring 2025/Time Series Analysis/Assignment 1")
D <- read.csv("DST_BIL54.csv")
str(D)

# See the help
D$time <- as.POSIXct(paste0(D$time,"-01"), "%Y-%m-%d", tz="UTC")
D$time
class(D$time)

## Year to month for each of them
D$year <- 1900 + as.POSIXlt(D$time)$year + as.POSIXlt(D$time)$mon / 12

## Make the output variable a floating point (i.e.\ decimal number)
D$total <- as.numeric(D$total) / 1E6

## Divide intro train and test set
teststart <- as.POSIXct("2024-01-01", tz="UTC")
Dtrain <- D[D$time < teststart, ]
Dtest <- D[D$time >= teststart, ]

#1.1 Plot
#Creating a time variable
Dtrain$year <- 1900 + as.POSIXlt(Dtrain$time)$year + as.POSIXlt(Dtrain$time)$mon / 12
x <- Dtrain$year

#Plotting the data
plot(x, Dtrain$total, type="o", xlab="Time (Years)", ylab="Total Vehicles (millions)", 
     main="Motor Driven Vehicles in Denmark (Training Set)", col="blue")

#2.2 Estimates

#Parameter estimates - theta
# Define the output vector y
y <- c(2930483,2934044, 2941422)

# Define the design matrix X
X <- matrix(c(1, 1, 1, 
              2018, 2018.083, 2018.167), 
            ncol = 2, byrow = FALSE)

# Compute the least squares estimates
theta_hat <- solve(t(X) %*% X) %*% t(X) %*% y

# Print the parameter estimates
theta_hat

#Standard errors - sigma
# Compute predicted values
y_hat <- X %*% theta_hat

# Compute residuals
residuals <- y - y_hat

# Estimate variance of residuals (sigma^2)
sigma2_hat <- sum(residuals^2) / (length(y) - ncol(X))

#Print unbiased estimator
sigma2_hat

# Compute variance-covariance matrix of theta_hat
var_theta <- sigma2_hat * solve(t(X) %*% X)
var_theta

# Compute standard errors (square root of diagonal elements)
sigma_hat <- sqrt(diag(var_theta))

# Print standard errors
sigma_hat

#Plotting the data points
# Load ggplot2 package
library(ggplot2)

# Create a data frame for the observations
data <- data.frame(
  time = c(2018.083, 2018.167, 2018.250), # x-values
  total = c(2934044, 2941422, 2951498)    # y-values
)

# Create a sequence of time points for the regression line
time_seq <- seq(min(data$time), max(data$time), length.out = 100)

# Compute fitted values for the regression line
fitted_values <- theta_hat[1] + theta_hat[2] * time_seq

# Create a data frame for the regression line
line_data <- data.frame(time = time_seq, total = fitted_values)

# Plot using ggplot2
ggplot() +
  geom_point(data = data, aes(x = time, y = total), color = "blue", size = 3) +  # Observed points
  geom_line(data = line_data, aes(x = time, y = total), color = "red", linewidth = 1) +  # Regression line
  labs(x = "Time (Fractional Years)", y = "Total Vehicles", 
       title = "Regression Line with Estimated Mean") +
  theme_minimal()


# 2.3 Prediction
# Define new time points for the next 12 months
#Creating a time variable
Dtest$year <- 1900 + as.POSIXlt(Dtest$time)$year + as.POSIXlt(Dtest$time)$mon / 12
new_time <- Dtest$year
new_time


# Construct design matrix for future points
X_new <- cbind(1, new_time)

# Compute predicted values using the given formula
y_pred <- X_new %*% theta_hat

# Print predictions
print(y_pred)

# Compute standard errors for predictions
se_pred <- sqrt(sigma2_hat * (1 + rowSums(X_new %*% solve(t(X) %*% X) * X_new)))

# Compute 95% prediction intervals
t_val <- qt(0.975, df = length(y) - ncol(X))  # Critical t-value
lower_bound <- y_pred - t_val * se_pred
upper_bound <- y_pred + t_val * se_pred

print(lower_bound)
print(upper_bound)


# 2.4 Plot

# Plot training data (scaled to millions)
plot(Dtrain$year, Dtrain$total, type = "o", pch = 16, col = "blue",
     xlab = "Time (Years)", ylab = "Total Vehicles (millions)",
     main = "Motor Driven Vehicles in Denmark (2018–2024)",
     xlim = c(2018, 2025), ylim = c(min(Dtrain$total), max(upper_bound) / 1E6))

# Add fitted regression line for training data
abline(lm(Dtrain$total ~ Dtrain$year), col = "red", lwd = 2)

# Add predicted values for 2024 (also scaled to millions)
points(new_time, y_pred / 1E6, col = "orange", pch = 16)

# Add prediction intervals (scaled to millions)
polygon(c(new_time, rev(new_time)),
        c(lower_bound / 1E6, rev(upper_bound / 1E6)),
        col = rgb(0.7, 0.7, 0.7, 0.5), border = NA)

# Add legend
legend("topleft", legend = c("Training Data", "Fitted Model", "Predictions", "95% Prediction Interval"),
       col = c("blue", "red", "orange", "gray"), pch = c(16, NA, 16, NA), lty = c(NA, 1, NA, NA), lwd = c(NA, 2, NA, NA))


# 2.6 Residuals of the model
#Residuals for training data
# Define design matrix X for all training data
X_full <- cbind(1, Dtrain$year)

# Define output vector y for all training data
y_full <- Dtrain$total

# Estimate parameters (theta_hat) using least squares
theta_hat_full <- solve(t(X_full) %*% X_full) %*% t(X_full) %*% y_full

# Predicted values
y_hat_full <- X_full %*% theta_hat_full

# Residuals
residuals_full <- y_full - y_hat_full

# Check residuals
print(residuals_full)

# Residual plot
plot(Dtrain$year, residuals_full, type = "o", pch = 16, col = "blue",
     xlab = "Time (Years)", ylab = "Residuals",
     main = "Residual Plot for Full Training Data")
abline(h = 0, col = "red", lwd = 2)

hist(residuals_full, breaks = 10, col = "lightblue",
     main = "Histogram of Residuals",
     xlab = "Residuals")

qqnorm(residuals_full, main = "Q-Q Plot of Residuals")
qqline(residuals_full, col = "red")

acf(residuals_full, main = "ACF of Residuals")

#Residual standard error
sqrt((1/70)*sum(residuals_full^2)) 


#4.2
# Define training data
x <- Dtrain$year  # Time variable
y <- Dtrain$total # Observations (in millions)

# Initial values
theta_hat <- matrix(c(0, 0), ncol = 1)  # Initial theta as a column matrix
R <- matrix(c(0.1, 0, 0, 0.1), nrow = 2, byrow = TRUE)  # Initial R

# Recursive estimation
for (t in 2:4) {
  Xt <- matrix(c(1, x[t]), ncol = 1)  # Ensure Xt is a column matrix
  
  # Update R and theta
  R <- R + Xt %*% t(Xt)  # Xt %*% t(Xt) results in a 2x2 matrix
  theta_hat <- theta_hat + solve(R) %*% Xt %*% (y[t] - t(Xt) %*% theta_hat)
  
  cat("t=", t-1)
  print(format(theta_hat, scientific = FALSE, digits = 4))
  cat("t=", t-1)
  print(theta_hat)
}

#4.3
x <- Dtrain$year  # Time variable
y <- Dtrain$total # Observations (in millions)

# Initial values
theta_hat <- matrix(c(0, 0), ncol = 1)  # Initial theta as a column matrix
R <- matrix(c(0.1, 0, 0, 0.1), nrow = 2, byrow = TRUE)  # Initial R

# Recursive estimation
for (t in 2:73) {
  Xt <- matrix(c(1, x[t]), ncol = 1)  # Ensure Xt is a column matrix
  
  # Update R and theta
  R <- R + Xt %*% t(Xt)  # Xt %*% t(Xt) results in a 2x2 matrix
  theta_hat <- theta_hat + solve(R) %*% Xt %*% (y[t] - t(Xt) %*% theta_hat)
  
  cat("t=", t-1)
  print(format(theta_hat, scientific = FALSE, digits = 4))
  cat("t=", t-1)
  print(theta_hat)
}

#Decreasing the difference
x <- Dtrain$year  # Time variable
y <- Dtrain$total # Observations (in millions)

# Initial values
theta_hat <- matrix(c(0, 0), ncol = 1)  # Initial theta as a column matrix
R <- matrix(c(0.000000001, 0, 0, 0.00000001), nrow = 2, byrow = TRUE)  # Initial R

# Recursive estimation
for (t in 2:73) {
  Xt <- matrix(c(1, x[t]), ncol = 1)  # Ensure Xt is a column matrix
  
  # Update R and theta
  R <- R + Xt %*% t(Xt)  # Xt %*% t(Xt) results in a 2x2 matrix
  theta_hat <- theta_hat + solve(R) %*% Xt %*% (y[t] - t(Xt) %*% theta_hat)
  
  cat("t=", t-1)
  print(format(theta_hat, scientific = FALSE, digits = 4))
  cat("t=", t-1)
  print(theta_hat)
}

#5.4
#lambda = 0.7
x <- Dtrain$year  # Time variable
y <- Dtrain$total # Observations (in millions)

# Initial values
theta_hat <- matrix(c(0, 0), ncol = 1)  # Initial theta as a column matrix
R <- matrix(c(0.1, 0, 0, 0.1), nrow = 2, byrow = TRUE)  # Initial R

# Recursive estimation
for (t in 2:73) {
  Xt <- matrix(c(1, x[t]), ncol = 1)  # Ensure Xt is a column matrix
  
  # Update R and theta
  R <- 0.7*R + Xt %*% t(Xt)  # Xt %*% t(Xt) results in a 2x2 matrix
  theta_hat <- theta_hat + solve(R) %*% Xt %*% (y[t] - t(Xt) %*% theta_hat)
  
  cat("t=", t-1)
  print(theta_hat)
}

#lambda = 0.99
x <- Dtrain$year  # Time variable
y <- Dtrain$total # Observations (in millions)

# Initial values
theta_hat <- matrix(c(0, 0), ncol = 1)  # Initial theta as a column matrix
R <- matrix(c(0.1, 0, 0, 0.1), nrow = 2, byrow = TRUE)  # Initial R

# Recursive estimation
for (t in 2:73) {
  Xt <- matrix(c(1, x[t]), ncol = 1)  # Ensure Xt is a column matrix
  
  # Update R and theta
  R <- 0.99*R + Xt %*% t(Xt)  # Xt %*% t(Xt) results in a 2x2 matrix
  theta_hat <- theta_hat + solve(R) %*% Xt %*% (y[t] - t(Xt) %*% theta_hat)
  
  cat("t=", t-1)
  print(theta_hat)
}



library(ggplot2)
library(dplyr)

# Initialize storage for theta estimates
results <- data.frame(t = integer(), lambda = numeric(), theta1 = numeric(), theta2 = numeric())

lambdas <- c(0.7, 0.99)

for (lambda in lambdas) {
  x <- Dtrain$year  # Time variable
  y <- Dtrain$total # Observations (in millions)
  
  # Initial values
  theta_hat <- matrix(c(0, 0), ncol = 1)  # Initial theta as a column matrix
  R <- matrix(c(0.1, 0, 0, 0.1), nrow = 2, byrow = TRUE)  # Initial R
  
  # Recursive estimation
  for (t in 2:73) {
    Xt <- matrix(c(1, x[t]), ncol = 1)  # Ensure Xt is a column matrix
    
    # Update R and theta
    R <- lambda * R + Xt %*% t(Xt)  # Xt %*% t(Xt) results in a 2x2 matrix
    theta_hat <- theta_hat + solve(R) %*% Xt %*% (y[t] - t(Xt) %*% theta_hat)
    
    # Store results
    results <- rbind(results, data.frame(t = t, lambda = lambda, theta1 = theta_hat[1,1], theta2 = theta_hat[2,1]))
  }
}

# Convert lambda to a factor for better plotting
results$lambda <- as.factor(results$lambda)

# Plot theta1 estimates
p1 <- ggplot(results, aes(x = t, y = theta1, color = lambda, group = lambda)) +
  geom_line() +
  labs(title = expression(hat(theta)[1] ~ " Estimates for Different " ~ lambda),
       x = "Time Index", y = expression(hat(theta)[1])) +
  theme_minimal()

# Plot theta2 estimates
p2 <- ggplot(results, aes(x = t, y = theta2, color = lambda, group = lambda)) +
  geom_line() +
  labs(title = expression(hat(theta)[2] ~ " Estimates for Different " ~ lambda),
       x = "Time Index", y = expression(hat(theta)[2])) +
  theme_minimal()

# Display the plots
print(p1)
print(p2)

