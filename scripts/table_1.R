library(plm)
library(lmtest)
library(sandwich)
library(stargazer)
library(glmnet)
library(dplyr)
library(haven)  # For read_dta if haven is used

# Set file paths
data_path <- "/cloud/project/data/raw_data/"
data_file <- paste0(data_path, "covid_gender_data.dta")

# Load the data
data <- read_dta(data_file)

# Convert appropriate columns to factors
data$geo_state <- as.factor(data$geo_state)
data$fem_resp_age <- as.factor(data$fem_resp_age)
data$final_status <- as.factor(data$final_status)

# Filter data with complete cases for required variables
data_filtered <- data %>%
  filter(!is.na(cases_per_100000) & !is.na(deaths_per_100000) & !is.na(red_zone) & 
           !is.na(orange_zone) & !is.na(geo_state) & !is.na(fem_resp_age)) %>%
  filter(!is.na(ind_fem_depression_change) & !is.na(ind_fem_tired_change) &
           !is.na(ind_fem_worried_change) & !is.na(ind_fem_safety_change) &
           final_status %in% c("1", "2"))

# Check the number of remaining observations
print(paste("Filtered data contains", nrow(data_filtered), "observations."))

# Define response and predictor variables
response_vars <- c("ind_fem_depression_change", "ind_fem_tired_change", "ind_fem_worried_change", "ind_fem_safety_change", "mental_index_change")

# Generate dummy variables for categorical predictors
dummies <- model.matrix(~ geo_state + fem_resp_age - 1, data = data_filtered)

# Combine the dummies and other predictors
predictors <- c("dist_prop_covid_zone", "red_zone", "orange_zone", "cases_per_100000", "deaths_per_100000", "tran_inc_normal", "asset_index", "ind_fem_resp_edu")
x <- cbind(dummies, data_filtered[, predictors])

# Check for constant columns in x to avoid issues in glmnet
x <- x[, sapply(x, function(col) length(unique(col)) > 1)]

# Convert to matrix for glmnet
x_matrix <- as.matrix(x)

x_matrix_imputed <- apply(x_matrix, 2, function(column) {
  if (any(is.na(column))) {
    column[is.na(column)] <- median(column, na.rm = TRUE)
  }
  return(column)
})

# Convert back to matrix if needed
x_matrix_imputed <- as.matrix(x_matrix_imputed)

# Initialize list for storing models

fit_model <- function(response_variable) {
  y <- data_filtered[[response_variable]]
  
  if (length(unique(y)) > 1) {  # Check for variation in the response
    set.seed(123)  # For reproducibility
    cv_fit <- cv.glmnet(x_matrix_imputed, y, alpha = 1)
    
    if (!is.null(cv_fit$lambda.min)) {
      best_lambda <- cv_fit$lambda.min
      lasso_fit <- glmnet(x_matrix_imputed, y, alpha = 1, lambda = best_lambda)
      
      lasso_coefs <- coef(lasso_fit, s = "lambda.min")
      active_vars <- rownames(lasso_coefs)[lasso_coefs[, 1] != 0]
      active_vars <- active_vars[active_vars != "(Intercept)"]  # Exclude intercept
      
      if (length(active_vars) > 0) {
        # Subset x_matrix to include only active variables
        active_x_matrix <- x_matrix_imputed[, active_vars, drop = FALSE]
        
        # Fit the final linear model using PLM for pooled model with robust SE
        final_model <- lm(y ~ active_x_matrix - 1)  # No intercept, using matrix directly
        robust_se <- vcovHC(final_model, type = "HC1", cluster = "geo_district")
        
        return(list(model = final_model, robust_se = robust_se))
      } else {
        message("No active variables found for ", response_variable, " after LASSO.")
        return(NULL)
      }
    } else {
      message("No valid lambda found for ", response_variable)
      return(NULL)
    }
  } else {
    message("No variation in response variable: ", response_variable)
    return(NULL)
  }
}

# Run the models
final_models <- lapply(set_names(response_vars), fit_model)
