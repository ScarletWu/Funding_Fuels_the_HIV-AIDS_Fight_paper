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



# First, ensure the necessary columns are properly formatted
data$geo_state <- as.factor(data$geo_state)
data$fem_resp_age <- as.factor(data$fem_resp_age)
data$final_status <- as.factor(data$final_status)

# Apply complete.cases to ensure all specified columns are complete
data_complete <- data %>%
  filter(complete.cases(cases_per_100000, deaths_per_100000, red_zone, orange_zone, geo_state, fem_resp_age))

# Check how many rows are left after applying complete.cases
print(paste("After complete.cases:", nrow(data_complete)))

# Now filter out rows with any NA in the specified response variables
data_filtered <- data_complete %>%
  filter(!is.na(ind_fem_depression_change) & 
           !is.na(ind_fem_tired_change) &
           !is.na(ind_fem_worried_change) & 
           !is.na(ind_fem_safety_change))



# Finally, filter by final_status
data_filtered <- data_filtered %>%
  filter(final_status %in% c("1", "2"))

# Check the number of rows
print(paste("After filtering by final_status:", nrow(data_filtered)))

# If data_filtered is empty, you may need to relax some conditions or check the data quality
if(nrow(data_filtered) == 0) {
  cat("No data left after filtering. Check criteria or data quality.")
}


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

x_matrix[is.na(x_matrix)] <- apply(x_matrix, 2, function(col) mean(col, na.rm = TRUE))



# Initialize a list to store models
final_models <- list()

# Loop through response variables
final_models <- list()

# Loop through each response variable
for (resp_var in response_vars) {
  
  # Extract the response variable
  y <- data_filtered[[resp_var]]
  
  # Check for variability in y
  if (length(unique(y)) > 1) {
    # Proceed with LASSO if y is not constant
    set.seed(123) # Reproducibility
    cv_fit <- cv.glmnet(x_matrix, y, alpha = 1)
    
    # Best lambda (regularization parameter)
    best_lambda <- cv_fit$lambda.min
    
    # Fit the LASSO model using the best lambda
    lasso_fit <- glmnet(x_matrix, y, alpha = 1, lambda = best_lambda)
    
    # Extract non-zero coefficients
    lasso_coefs <- coef(lasso_fit, s = "lambda.min")
    active_vars <- rownames(lasso_coefs)[lasso_coefs[, 1] != 0]
    active_vars <- active_vars[active_vars != "(Intercept)"] # Exclude intercept
    
    # Check if there are any active variables left after LASSO
    if (length(active_vars) > 0) {
      # Fit the final linear model using PLM for pooled model with robust SE
      final_formula <- as.formula(paste(resp_var, "~", paste(active_vars, collapse = " + ")))
      final_model <- plm(final_formula, data = data_filtered, model = "pooling")
      robust_se <- vcovHC(final_model, type = "HC1", cluster = "group")
      
      # Store the final model and its robust SE
      final_models[[resp_var]] <- list(model = final_model, robust_se = robust_se)
    } else {
      message("No active variables found for ", resp_var, " after LASSO.")
    }
  } else {
    message("No variation in response variable: ", resp_var)
  }
}

# If you want to check the results or any outputs, you might use print or view commands
print(final_models)

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


  