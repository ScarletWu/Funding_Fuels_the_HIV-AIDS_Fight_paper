# Load necessary libraries
library(MASS)

# Data simulation based on original data structure
# Identify numeric columns
numeric_columns <- sapply(data_filtered[, predictors, drop = FALSE], is.numeric)

# Filter numeric columns
numeric_data <- data_filtered[, predictors, drop = FALSE][, numeric_columns]

# Calculate mean and covariance
mu <- colMeans(numeric_data)
sigma <- cov(numeric_data)

# Generate simulated data
simulated_data <- as.data.frame(mvrnorm(n = nrow(data_filtered), mu = mu, Sigma = sigma))


# Saving the simulated dataset
write.csv(simulated_data, "/cloud/project/data/cleaned/simulated_data.csv", row.names = FALSE)
