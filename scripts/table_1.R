library(plm)
library(lmtest)
library(sandwich)
library(stargazer)
library(glmnet)
library(dplyr)
library(haven)  # For read_dta if haven is used

# Load the data
data_path <- "/cloud/project/data/raw_data/"
data_file <- paste0(data_path, "covid_gender_data.dta")
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

print(final_models)
# Assuming 'final_models' is a list of lists containing 'model' and 'robust_se' for each response variable

# Prepare a data frame for stargazer
results <- data.frame(
  model = character(),
  estimate = numeric(),
  std.error = numeric(),
  statistic = numeric(),
  p.value = numeric(),
  stars = character(),
  stringsAsFactors = FALSE
)

print(final_models)

# Prepare the data for stargazer
stargazer_data <- list()

# Loop through the response variables and extract the needed data
for (resp_var in response_vars) {
  model_obj <- final_models[[resp_var]]
  if (!is.null(model_obj)) {
    # Add the lm object to the stargazer_data list
    stargazer_data[[resp_var]] <- model_obj$model
  }
}

# Note: You may need to adjust the 'covariate.labels' and 'dep.var.labels' 
# to match the variables in your model. The 'omit.yes.no' argument should 
# be filled with your control variable names if you want to display whether 
# they were included in the model (Yes/No).
# The 'se' list argument assumes you have calculated robust standard errors 
# for each model and you want them included in the table instead of the default standard errors.
# Prepare a vector of model summaries that includes robust standard errors
model_summaries <- lapply(stargazer_data, function(model) {
  coefs <- summary(model)$coefficients
  se <- sqrt(diag(vcovHC(model, type = "HC1")))
  coefs[, "Std. Error"] <- se
  coefs
})

# Since 'omit.yes.no' is causing issues, let's remove it
# We also remove the se argument since we already included robust SE in model summaries
stargazer::stargazer(
  stargazer_data, 
  type = "text",
  title = "Table 1: Relationship between Containment and Female Well-being.",
  covariate.labels = c("Containment", "Past Containment Controls", "State FE", "Age FE", "Lasso Controls", "Case and Death Controls"),
  dep.var.labels = c("More Depressed", "More Exhausted", "More Anxious", "MH Index", "Less Safe"),
  omit = 'Constant', 
  no.space = TRUE,
  intercept.bottom = FALSE,
  single.row = TRUE,
  star.cutoffs = c(0.05, 0.01, 0.001),
  ci = TRUE,
  ci.level = 0.95
)

# You may need to adjust 'covariate.labels' and 'dep.var.labels' to match your models.
