# Problem 5.5
# Load necessary libraries
library(ggplot2)

# Load dataset
file_path <- "C:\\Users\\snehi\\Documents\\DST_BIL54.csv"
df <- read.csv(file_path)

df$time <- as.Date(paste0(df$time, "-01"), format="%Y-%m-%d")

df <- df[1:72, ] # Select first 72 months (Jan 2018 - Dec 2023)
df$months_since_2018 <- as.numeric(difftime(df$time, as.Date("2018-01-01"), units="days")) / 30

# Set up data
X <- cbind(1, df$months_since_2018)  # Feature matrix with intercept
y <- df$total                         # Target variable
n <- length(y)                        # Number of observations

# Function to compute one-step predictions and residuals
evaluate_residuals <- function(lambda) {
  Theta <- matrix(NA, nrow=n, ncol=2)  # Store parameter estimates
  OneStepPred <- rep(NA, n)  # One-step-ahead predictions
  Residuals <- rep(NA, n)  # One-step-ahead residuals
  
  # First step initialization
  R <- diag(1e-6, 2)  # Regularization to avoid singularity
  h <- rep(0, 2)
  
  # Recursive Least Squares Estimation
  for (t in 2:(n - 1)) {
    x_t <- X[t, ]
    y_t <- y[t]
    
    # Update R and h
    R <- lambda * R + x_t %*% t(x_t)
    h <- lambda * h + x_t * y_t
    
    # Estimate parameters
    Theta[t, ] <- solve(R) %*% h
    
    # Compute one-step-ahead prediction
    OneStepPred[t + 1] <- X[t + 1, ] %*% Theta[t, ]
    
    # Compute residual
    Residuals[t + 1] <- OneStepPred[t + 1] - y[t + 1]
  }
  
  return(data.frame(time=df$time[-(1:4)], residuals=Residuals[-(1:4)]))
}

# Evaluate residuals for lambda = 0.7 and 0.99
residuals_07 <- evaluate_residuals(0.7)
residuals_099 <- evaluate_residuals(0.99)

# Plot residuals over time
ggplot() +
  geom_line(data=residuals_07, aes(x=time, y=residuals, color="Lambda = 0.7")) +
  geom_line(data=residuals_099, aes(x=time, y=residuals, color="Lambda = 0.99")) +
  labs(title="One-Step-Ahead Residuals Over Time",
       x="Time",
       y="Residuals",
       color="Forgetting Factor") +
  theme_minimal()
