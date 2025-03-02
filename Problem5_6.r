# Problem 5.6
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

# Define lambda values to test
lambda_values <- seq(0.50, 0.99, by=0.01)
k_values <- 1:12  # Forecast horizons

# Create a matrix to store RMSE for each (lambda, k) pair
RMSE_matrix <- matrix(NA, nrow=length(lambda_values), ncol=length(k_values))
rownames(RMSE_matrix) <- lambda_values
colnames(RMSE_matrix) <- k_values

# Iterate over lambda values
for (l in seq_along(lambda_values)) {
  lambda <- lambda_values[l]
  
  # Initialize RLS matrices
  Theta <- matrix(NA, nrow=n, ncol=2)  # Store parameter estimates
  OneStepPred <- matrix(NA, nrow=n, ncol=max(k_values))  # k-step predictions
  
  # First step initialization
  R <- diag(1e-6, 2)  # Regularization to avoid singularity
  h <- rep(0, 2)
  
  # Recursive Least Squares Estimation
  for (t in 1:(n - max(k_values))) {
    x_t <- X[t, ]
    y_t <- y[t]
    
    # Update R and h
    R <- lambda * R + x_t %*% t(x_t)
    h <- lambda * h + x_t * y_t
    
    # Estimate parameters
    Theta[t, ] <- solve(R) %*% h
    
    # Compute k-step-ahead predictions
    for (k in k_values) {
      if (t + k <= n) {
        OneStepPred[t, k] <- X[t + k, ] %*% Theta[t, ]
      }
    }
  }
  
  # Compute RMSE for each k-step horizon
  for (k in k_values) {
    valid_indices <- (1:(n-k))  # Ensure we do not exceed bounds
    residuals <- OneStepPred[valid_indices, k] - y[valid_indices + k]
    RMSE_matrix[l, k] <- sqrt(mean(residuals^2, na.rm=TRUE))
  }
}

# Convert RMSE matrix to a data frame for plotting
RMSE_df <- as.data.frame(as.table(RMSE_matrix))
colnames(RMSE_df) <- c("lambda", "k", "RMSE")
RMSE_df$lambda <- as.numeric(as.character(RMSE_df$lambda))
RMSE_df$k <- as.numeric(as.character(RMSE_df$k))

# Plot RMSE vs lambda for different k-step horizons
ggplot(RMSE_df, aes(x=lambda, y=RMSE, color=factor(k))) +
  geom_line() +
  labs(title="RMSE vs Forgetting Factor (Lambda)", 
       x="Forgetting Factor (Lambda)", 
       y="Root Mean Square Error (RMSE)", 
       color="Forecast Horizon (k)") +
  theme_minimal()