library(plm)
library(lmtest)
library(sandwich)
library(stargazer)
library(glmnet)
library(dplyr)
library(readstata13)

# Set file paths
data_path <- "/cloud/project/data/raw_data/"
data_file <- paste0(data_path, "covid_gender_data.dta")

data <- read_dta(data_file)

# Convert appropriate columns to factors (if they are not already)
data$geo_state <- as.factor(data$geo_state)
data$fem_resp_age <- as.factor(data$fem_resp_age)
data$geo_district <- as.factor(data$geo_district) # Ensure geo_district is a factor for clustering

data_filtered <- data %>% 
  filter(complete.cases(cases_per_100000, deaths_per_100000, red_zone, orange_zone, geo_state, fem_resp_age)) %>%
  filter(!is.na(ind_fem_depression_change) & 
           !is.na(ind_fem_tired_change) &
           !is.na(ind_fem_worried_change) & 
           !is.na(ind_fem_safety_change) &
           final_status %in% c("Fully complete", "Partially complete"))

# Define response and predictor variables
response_vars <- c("ind_fem_depression_change", "ind_fem_tired_change", 
                   "ind_fem_worried_change", "ind_fem_safety_change", 
                   "mental_index_change")
predictors <- c("dist_prop_covid_zone", "red_zone", "orange_zone", 
                "cases_per_100000", "deaths_per_100000", "tran_inc_normal", 
                "asset_index", "ind_fem_resp_edu")

# Generate dummy variables for categorical predictors
dummies <- model.matrix(~ geo_state + fem_resp_age - 1, data = data_filtered)

# Combine the dummies and other predictors
x <- cbind(dummies, data_filtered[, predictors])

# Convert to a matrix for glmnet
x_matrix <- as.matrix(x)

# Initialize a list to store models
final_models <- list()

# Loop through response variables
for (resp_var in response_vars) {
  
  # Extract the response variable
  y <- data_filtered[[resp_var]]
  
  # Perform LASSO regression using cross-validation to find the optimal lambda
  set.seed(123) # Set seed for reproducibility
  cv_fit <- cv.glmnet(x_matrix, y, alpha = 1)
  
  # Best lambda (regularization parameter)
  best_lambda <- cv_fit$lambda.min
  
  # Fit the LASSO model using the best lambda
  lasso_fit <- glmnet(x_matrix, y, alpha = 1, lambda = best_lambda)
  
  # Extract the non-zero coefficients from the LASSO model
  lasso_coefs <- coef(lasso_fit, s = "lambda.min")
  active_vars <- rownames(lasso_coefs)[lasso_coefs[, 1] != 0]
  
  # Remove the intercept from active variables
  active_vars <- active_vars[active_vars != "(Intercept)"]
  
  # Only keep active variables for the final model
  active_predictors <- x_matrix[, active_vars, drop = FALSE]
  
  # Build the final model formula
  final_formula <- as.formula(paste(resp_var, "~", paste(active_vars, collapse = " + ")))
  
  # Fit the final linear model with robust standard errors
  final_model <- plm(final_formula, data = data_filtered, model = "pooling")
  robust_se <- vcovHC(final_model, type = "HC1", cluster = "group")
  
stargazer::stargazer(
  final_models$ind_fem_depression_change$model,
  final_models$ind_fem_tired_change$model,
  final_models$ind_fem_worried_change$model,
  final_models$ind_fem_safety_change$model,
  final_models$mental_index_change$model,
  type = "text",  # Use "html" or "latex" for different formats suitable for web or LaTeX
  title = "Relationship between Containment and Female Well-being",
  out = "table1.txt",  # Saves the output to a text file
  se = list(final_models$ind_fem_depression_change$robust_se,
            final_models$ind_fem_tired_change$robust_se,
            final_models$ind_fem_worried_change$robust_se,
            final_models$ind_fem_safety_change$robust_se,
            final_models$mental_index_change$robust_se), # Use robust standard errors
  covariate.labels = c("Containment", "Past Containment Controls", "State FE", "Age FE", "Lasso Controls", "Case and Death Controls"),
  omit.stat = c("LL", "ser", "f"), # Exclude some statistics you might not need
  no.space = TRUE, # Reduce space between columns
  column.labels = c("More Depressed", "More Exhausted", "More Anxious", "MH Index", "Less Safe"),
  single.row = TRUE # All info in one row for each model
)


  