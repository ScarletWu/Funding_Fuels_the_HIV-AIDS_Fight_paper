# Load necessary libraries
library(MASS)

# Data simulation based on original data structure
simulated_data <- as.data.frame(mvrnorm(n = nrow(data_filtered), mu = colMeans(data_filtered[, predictors, drop = FALSE]), Sigma = cov(data_filtered[, predictors, drop = FALSE])))
colnames(simulated_data) <- predictors

# Add noise to the data
simulated_data <- mutate(simulated_data, tran_inc_normal = tran_inc_normal + rnorm(n(), mean = 0, sd = sd(data$tran_inc_normal)))

# Saving the simulated dataset
write.csv(simulated_data, "/cloud/project/data/simulated_data.csv", row.names = FALSE)
