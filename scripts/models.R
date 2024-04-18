# Load necessary libraries
library(dplyr)
library(glmnet)

# Set the data path and load the data
data_path <- "/cloud/project/data/cleaned/"
data_file <- paste0(data_path, "Filtered_Covid_Gender_Data.csv")
filtered_data <- read.csv(data_file)

# Data preprocessing: Adding squared and cubic terms for cases and deaths
filtered_data <- filtered_data %>%
  mutate(
    cases_sq = cases_per_100000^2,
    cases_cube = cases_per_100000^3,
    deaths_sq = deaths_per_100000^2,
    deaths_cube = deaths_per_100000^3
  )

# Ensuring no NA values in the dataset for the variables used in modeling
filtered_data <- filtered_data %>%
  filter(
    !is.na(ind_fem_depression_change) & 
      !is.na(dist_prop_covid_zone) & 
      !is.na(cases_per_100000) & 
      !is.na(deaths_per_100000) & 
      !is.na(geo_state) & 
      !is.na(fem_resp_age)
  )

# Basic linear regression model with specified controls
basic_model <- lm(ind_fem_depression_change ~ dist_prop_covid_zone + cases_per_100000 + deaths_per_100000 + geo_state + fem_resp_age, data = filtered_data)
print(summary(basic_model))

# Model with polynomial terms
poly_model <- lm(ind_fem_depression_change ~ dist_prop_covid_zone + cases_per_100000 + cases_sq + cases_cube + deaths_per_100000 + deaths_sq + deaths_cube + geo_state + fem_resp_age, data = filtered_data)
print(summary(poly_model))

# Preparing data for Lasso model
x <- model.matrix(ind_fem_depression_change ~ dist_prop_covid_zone + cases_per_100000 + cases_sq + cases_cube + deaths_per_100000 + deaths_sq + deaths_cube + geo_state + fem_resp_age - 1, data = filtered_data)
y <- filtered_data$ind_fem_depression_change

# Fit Lasso model
lasso_model <- cv.glmnet(x, y, alpha = 1)  # alpha=1 for lasso
print(coef(lasso_model, s = "lambda.min"))

# Plot the cross-validated error
plot(lasso_model)
