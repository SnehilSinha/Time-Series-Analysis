# Code used for Time Series Analysis assignment 1 - Questions 3 and (in part) question 5

# Plot the weights:

##################### 1 Create a vector storing the weights #####################

l <- 0.9 # Set the lambda parameter
d <- rep(1, 72) # Make a vector filled with ones
i <- 0 # Set an iteration parameter

# Multiply with lambda to create the array of increasing mononomials:
for (x in seq_along(d)) {
  d[x] = d[x] * l**i
  i <- i+1
}

# Reverse vector order:
d <- rev(d)

##################### 2 Make the weight matrix:  #####################

W <- diag(d)

##################### 3 Plot the weights with time #####################

# From Anna: 
#Creating a time variable
Dtrain$year <- 1900 + as.POSIXlt(Dtrain$time)$year + as.POSIXlt(Dtrain$time)$mon / 12

# Plot the weights vs. time:

plot(Dtrain$year, d, main="Local linear trend model weights",
     xlab="Time [years] ", ylab="Weights", pch=20)

##################### 4 Compute the sum of vector elements ######################

s <- sum(d)

##################### 5 Make a model ######################

data <- as.numeric(Dtrain$total) # Set data vector

# Set up the equations of conditions matrix:
G <- cbind(rep(1, 72), Dtrain$year)

# Find a model:

GWG <- solve(t(G) %*% W %*% G)
#RHS <- t(G) %*% W %*% data

length(data)

theta <-  GWG %*% t(G) %*% W %*% data


##################### Make a prediction #####################

# Set up new equations of conditions matrix for the new year:
Dtest$year <- 1900 + as.POSIXlt(Dtest$time)$year + as.POSIXlt(Dtest$time)$mon / 12
G_new <- cbind(rep(1, 12), Dtest$year)


dpred_WLS <- G_new %*% theta

##################### Make an OLS #####################
# For comparison, make an OLS:
model <- lm(Dtrain$total ~ Dtrain$year)

dpred_OLS <- G_new %*% model$coefficients
#predictions <- predict(model, newdata = data.frame(year = Dtest$year), 
                       interval = "prediction", level = 0.95)

# Extract the fitted values, lower and upper bounds
#fitted_values <- predictions[, 1]      # Predicted values
#lower_bound <- predictions[, 2]        # Lower bound of the prediction interval
#upper_bound <- predictions[, 3]        # Upper bound of the prediction interval
##################### Plot data and predictions #####################

plot(Dtrain$year, data, col = "black", xlim=range(Dtrain$year, Dtest$year), ylim = range(Dtrain$total, dpred+0.1),main = "Comparing WLS and OLS predictions", xlab = "Time [years]", ylab = "No. of motor driven vehicles in Denmark")
points(Dtest$year, Dtest$total, col = "black", pch = 21, bg = "black")

# WLS:
points(Dtest$year, dpred_WLS, col = rgb(1, 0, 0, 0.1), pch = 21, bg="red")

# Add regression line
abline(theta, lw = 1, col = "red")

# OLS:
points(Dtest$year, dpred_OLS, col = "green", pch = 21, bg="green")

abline(model$coefficients, lw = 1, col = "green")

legend("topleft", legend=c("Training Data", "Test Data", "WLS Predictions", 
                            "WLS Regression Line", "OLS Predictions", "OLS Regression Line"), 
       col=c("black", "black", "red", "red", "green", "green"), 
       pch=c(1, 21, 21, NA, 21, NA), 
       pt.bg=c(NA, "black", "red", NA, "green", NA), 
       lty=c(NA, NA, NA, 1, NA, 1), 
       lwd=c(NA, NA, NA, 1, NA, 1), 
       bty="n")

# Add axis labels:


