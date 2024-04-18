# Load necessary libraries
library(ggplot2)
library(dplyr)
library(lfe)  
library(broom)

# Set the data path and read the data
data_path <- "/cloud/project/data/cleaned/"
data_file <- paste0(data_path, "Filtered_Covid_Gender_Data.csv")
data <- read.csv(data_file)

# Prepare data: factor conversion and recoding
prepare_data <- function(data) {
  data %>%
    mutate(across(c(geo_state, color_zone, demo_caste, demo_religion, fem_marital_status), as.factor),
           demo_caste = factor(recode(as.character(demo_caste), '99' = NA)),
           demo_religion = factor(recode(as.character(demo_religion), '99' = NA)),
           fem_marital_status = factor(recode(as.character(fem_marital_status), '99' = NA)))
}

data <- prepare_data(data)

# Filter data to exclude missing data on necessary variables
filter_data <- function(data, variables) {
  data %>%
    filter(complete.cases(data[, variables])) %>%
    filter(final_status %in% c("1", "2"))
}

# Define general predictors and response variables
general_predictors <- c("cases_per_100000", "deaths_per_100000", "dist_prop_covid_zone", 
                        "red_zone", "orange_zone", "tran_inc_normal", "asset_index")
response_vars <- c("ind_fem_depression_change", "ind_fem_tired_change",
                   "ind_fem_worried_change", "ind_fem_safety_change", "mental_index_change")

data2t <- filter_data(data, c(response_vars, general_predictors, "geo_district"))

rename_vars <- c(
  ind_fem_depression_change = "More Depressed",
  ind_fem_tired_change = "More Tired",
  ind_fem_worried_change = "More Worried",
  ind_fem_safety_change = "Feel Less Safe",
  mental_index_change = "Mental Health Index Change",
  hh_num_inc_reduced = "Household Income Reduced",
  ind_meals_reduced = "Meals Reduced",
  newf_index = "New Financial Index",
  newm_index = "New Mental Health Index",
  cases_per_100000 = "Cases per 100,000",
  deaths_per_100000 = "Deaths per 100,000",
  dist_prop_covid_zone = "District Proportion in Covid Zone",
  red_zone = "Red Zone",
  orange_zone = "Orange Zone",
  tran_inc_normal = "Transition Income Normal",
  asset_index = "Asset Index",
  hh_num_inc_reduced_with_lasso = "Household Income Reduced with LASSO",
  hh_num_inc_reduced_without_lasso = "Household Income Reduced without LASSO",
  ind_meals_reduced_with_lasso = "Meals Reduced with LASSO",
  ind_meals_reduced_without_lasso = "Meals Reduced without LASSO",
  newf_index_with_lasso = "New Financial Index with LASSO",
  newf_index_without_lasso = "New Financial Index without LASSO",
  newm_index_with_lasso = "New Mental Health Index with LASSO",
  newm_index_without_lasso = "New Mental Health Index without LASSO"
)

# Use the updated map in the summarize_models function
summarize_models <- function(models) {
  bind_rows(
    lapply(names(models), function(model_name) {
      model_data <- tidy(models[[model_name]])
      model_data$model <- rename_vars[model_name] %>% coalesce(model_name)  # Rename the model name
      model_data$term <- rename_vars[model_data$term] %>% coalesce(model_data$term)  # Rename the terms
      return(model_data)
    }),
    .id = "model_id"
  ) %>%
    filter(term != "(Intercept)")
}

# Model fitting functions
fit_models <- function(data, response_vars, predictors) {
  models <- list()
  for (response in response_vars) {
    formula <- as.formula(paste(response, "~", paste(predictors, collapse = " + ")))
    model <- glm(formula, family = binomial(), data = data)
    models[[response]] <- model
  }
  return(models)
}

# Fit models for first set of variables
models_response <- fit_models(data2t, response_vars, general_predictors)

# Second set of variables of interest
variables_of_interest <- c("hh_num_inc_reduced", "ind_meals_reduced", "newf_index", "newm_index")
data2t_vars_interest <- filter_data(data, c(variables_of_interest, general_predictors, "geo_district"))

fit_felm_models <- function(data, variables_of_interest, lasso_vars, other_vars) {
  results <- list()
  for (var in variables_of_interest) {
    formula1 <- as.formula(paste0(var, " ~ ", paste(other_vars, collapse = "+"), " | 0 | 0 | geo_district"))
    formula2 <- as.formula(paste0(var, " ~ ", paste(c(other_vars, lasso_vars), collapse = "+"), "| 0 | 0 | geo_district"))
    model1 <- felm(formula1, data = data)
    model2 <- felm(formula2, data = data)
    results[[paste0(var, "_without_lasso")]] <- model1
    results[[paste0(var, "_with_lasso")]] <- model2
  }
  return(results)
}

# Fit models for second set of variables
results_vars_interest <- fit_felm_models(data2t_vars_interest, variables_of_interest, general_predictors, c("dist_prop_covid_zone", "red_zone", "orange_zone"))

# Plotting functions
plot_coefficients_logistic <- function(results) {
  ggplot(results, aes(x = term, y = estimate, color = model)) +
    geom_point(position = position_dodge(width = 0.5)) +
    geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), width = 0.2, position = position_dodge(width = 0.5)) +
    coord_flip() +
    labs(x = "Predictors", y = "Estimated Coefficients", title = "Effect Sizes of Predictors in Mental Health Models") +
    theme_minimal() +
    theme(legend.title = element_blank())
}

plot_coefficients_felm <- function(results) {
  ggplot(results, aes(x = term, y = estimate, color = model)) +
    geom_point(position = position_dodge(width = 0.5)) +
    geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), width = 0.2, position = position_dodge(width = 0.5)) +
    coord_flip() +
    labs(x = "Predictors", y = "Estimated Coefficients", title = "Effect Sizes of Predictors in Economic and Nutritional Impact Models") +
    theme_minimal() +
    theme(legend.title = element_blank())
}

# Generate and display plots
results_logistic <- summarize_models(models_response)
plot_logistic <- plot_coefficients_logistic(results_logistic)
print(plot_logistic)  # Display the first plot

results_felm <- summarize_models(results_vars_interest)
plot_felm <- plot_coefficients_felm(results_felm)
print(plot_felm)