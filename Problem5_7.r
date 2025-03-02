#Problem 5.7
# Load necessary libraries
library(ggplot2)

# Load dataset
file_path <- "C:\\Users\\snehi\\Documents\\DST_BIL54.csv"
df <- read.csv(file_path)

df$time <- as.Date(paste0(df$time, "-01"), format="%Y-%m-%d")

df_train <- df[1:72, ]  # Training set (Jan 2018 - Dec 2023)
df_test <- df[73:nrow(df), ]  # Test set (Jan 2024 - Dec 2024)
df_train$months_since_2018 <- as.numeric(difftime(df_train$time, as.Date("2018-01-01"), units="days")) / 30
df_test$months_since_2018 <- as.numeric(difftime(df_test$time, as.Date("2018-01-01"), units="days")) / 30

# Set up data
X_train <- cbind(1, df_train$months_since_2018)  # Feature matrix with intercept
y_train <- df_train$total                        # Target variable for training
n_train <- length(y_train)
X_test <- cbind(1, df_test$months_since_2018)   # Feature matrix for test set
y_test <- df_test$total                         # Target variable for test
n_test <- length(y_test)

# Function to compute k-step forecasts using RLS
predict_rls <- function(lambda, k_max) {
  Theta <- matrix(NA, nrow=n_train, ncol=2)
  Forecasts <- matrix(NA, nrow=n_test, ncol=k_max)
  
  # Initialize RLS parameters
  R <- diag(1e-6, 2)
  h <- rep(0, 2)
  
  # Train the model with RLS
  for (t in 1:(n_train - 1)) {
    x_t <- X_train[t, ]
    y_t <- y_train[t]
    
    # Update R and h
    R <- lambda * R + x_t %*% t(x_t)
    h <- lambda * h + x_t * y_t
    
    # Estimate parameters
    Theta[t, ] <- solve(R) %*% h
  }
  
  # Generate forecasts for different horizons
  for (k in 1:k_max) {
    for (t in 1:(n_test - k + 1)) {  # Ensure index does not exceed test size
      Forecasts[t, k] <- X_test[t + k - 1, ] %*% Theta[n_train - 1, ]
    }
  }
  
  return(Forecasts)
}

# Generate forecasts for lambda = 0.7 and 0.99
forecasts_07 <- predict_rls(0.7, 12)
forecasts_099 <- predict_rls(0.99, 12)

# Convert forecasts to data frame for plotting
forecast_df <- data.frame(time=rep(df_test$time, 12), 
                          horizon=rep(1:12, each=n_test),
                          forecast=c(forecasts_07, forecasts_099),
                          lambda=rep(c("Lambda = 0.7", "Lambda = 0.99"), each=n_test * 12))

# Remove rows with missing values
forecast_df <- na.omit(forecast_df)

# Plot forecasts for different horizons
ggplot(forecast_df, aes(x=time, y=forecast, color=factor(horizon))) +
  geom_line() +
  facet_wrap(~lambda) +
  labs(title="RLS Forecasts for Different Horizons", 
       x="Time", 
       y="Predicted Vehicles (Total)", 
       color="Forecast Horizon") +
  theme_minimal()
