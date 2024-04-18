library(haven)       # for reading Stata data files
library(dplyr)       # for data manipulation
library(glmnet)      # for LASSO regression
library(hdm)         # for high-dimensional metrics, including double selection LASSO
library(stargazer)   # for producing publication-quality regression tables
library(plm)         # for panel data analysis
library(tidyr)
library(ggplot2)
library(fixest)       # for fixed-effects regressions
library(lfe)          # for regressions with multiple fixed effects and clustering
library(modelsummary)

data_path <- "/cloud/project/data/cleaned/"
data_file <- paste0(data_path, "Filtered_Covid_Gender_Data.csv")
data <- read.csv(data_file)

# Ensure factor variables are correctly formatted
data <- mutate(data,
               across(c(geo_state, color_zone, demo_caste, demo_religion, fem_marital_status), as.factor))

# Recoding '99' to NA for relevant factor variables
data$demo_caste <- as.character(data$demo_caste)  # Convert to character to manipulate
data$demo_caste[data$demo_caste == "99"] <- NA    # Change '99' to NA
data$demo_caste <- factor(data$demo_caste)        # Convert back to factor

data$demo_religion <- as.character(data$demo_religion)
data$demo_religion[data$demo_religion == "99"] <- NA
data$demo_religion <- factor(data$demo_religion)

data$fem_marital_status <- as.character(data$fem_marital_status)
data$fem_marital_status[data$fem_marital_status == "99"] <- NA
data$fem_marital_status <- factor(data$fem_marital_status)



# Handling NA values in key factor variables

response_vars <- c("ind_fem_depression_change", "ind_fem_tired_change",
                   "ind_fem_worried_change", "ind_fem_safety_change", "mental_index_change")

predictors <- c("cases_per_100000", "deaths_per_100000", "dist_prop_covid_zone", 
                "red_zone", "orange_zone", "tran_inc_normal", "asset_index")



# Create an empty list to store models
models <- list()

# Loop through each response variable and fit a logistic regression model
for (response in response_vars) {
  formula <- as.formula(paste(response, "~", paste(predictors, collapse = " + ")))
  model <- glm(formula, family = binomial(), data = data)
  models[[response]] <- model
}

# Output the summary of each model
lapply(models, summary)


# Print model summary
summary(model)

# Visualization of the distribution of the recoded variable 'demo_caste'
ggplot(data, aes(x = demo_caste)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Distribution of Caste after Recoding", x = "Caste", y = "Count")

# Additional visualization to check another factor
ggplot(data, aes(x = fem_marital_status)) +
  geom_bar(fill = "darkgreen") +
  labs(title = "Distribution of Marital Status after Recoding", x = "Marital Status", y = "Count")

ggplot(data, aes(x = demo_caste)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Distribution of Caste after Recoding", x = "Caste", y = "Count")

data2t <- data

data2t$geo_state <- as.factor(data$geo_state)
data2t$fem_resp_age <- as.factor(data$fem_resp_age)
data2t$final_status <- as.factor(data$final_status)

# Filter data with complete cases for required variables
data2t <- data2t %>%
  filter(!is.na(cases_per_100000) & !is.na(deaths_per_100000) & !is.na(red_zone) & 
           !is.na(orange_zone) & !is.na(geo_state) & !is.na(fem_resp_age)) %>%
  filter(!is.na(ind_fem_depression_change) & !is.na(ind_fem_tired_change) &
           !is.na(ind_fem_worried_change) & !is.na(ind_fem_safety_change) &
           final_status %in% c("1", "2"))

results <- list()

variables_of_interest <- c("hh_num_inc_reduced", "ind_meals_reduced", "newf_index", "newm_index")

for (var in variables_of_interest) {
  # Without lasso - only state and age FEs
  formula1 <- as.formula(paste0(var, " ~ dist_prop_covid_zone + red_zone + orange_zone + roster_age_yrs + geo_state"))
  model1 <- felm(formula1, data = data2t, cluster = c("geo_district"))
  results[[paste0(var, "1")]] <- model1
  
  # With lasso
  # Assuming 'xselected' is a vector of selected variables from lasso regression
  # Please replace 'lasso_selected_variables' with actual selected variables from lasso
  formula2 <- as.formula(paste0(var, " ~ dist_prop_covid_zone + cases_per_100000 + deaths_per_100000 + red_zone + orange_zone + roster_age_yrs + geo_state + ", paste(lasso_selected_variables, collapse = "+")))
  model2 <- felm(formula2, data = data, cluster = c("geo_district"))
  results[[paste0(var, "2")]] <- model2
}

# Now, using modelsummary to create the tables similar to esttab in Stata
# This is an example and will need adjustments to match your exact table specifications
msummary(
  list(results[["hh_num_inc_reduced1"]], results[["hh_num_inc_reduced2"]], results[["ind_meals_reduced1"]], results[["ind_meals_reduced2"]]),
  output = paste0(output_path, "/table2.tex"),
  title = "Relationship between Containment and Socioeconomic and Nutritional Outcomes",
  stars = c(0.1, 0.05, 0.01)
)

# Assuming all necessary libraries are loaded, if not, uncomment below
# library(glmnet)
# library(sandwich)

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
        
        # Fit the final linear model using active variables
        final_model <- lm(y ~ ., data = as.data.frame(active_x_matrix))
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
final_models <- lapply(response_vars, fit_model)
