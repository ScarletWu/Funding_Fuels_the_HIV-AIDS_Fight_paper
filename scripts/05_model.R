# Load necessary libraries
library(ggplot2)
library(dplyr)
library(lfe)  
library(broom)

# Set the data path and read the data
data_path <- "/cloud/project/data/cleaned/"
data_file <- paste0(data_path, "Filtered_Covid_Gender_Data.csv")
data <- read.csv(data_file)

# Ensure factor variables are correctly formatted
data <- mutate(data,
               across(c(geo_state, color_zone, demo_caste, demo_religion, fem_marital_status), as.factor))

# Recoding '99' to NA for relevant factor variables
data$demo_caste <- factor(recode(as.character(data$demo_caste), '99' = NA))
data$demo_religion <- factor(recode(as.character(data$demo_religion), '99' = NA))
data$fem_marital_status <- factor(recode(as.character(data$fem_marital_status), '99' = NA))

# Define response and predictor variables
response_vars <- c("ind_fem_depression_change", "ind_fem_tired_change",
                   "ind_fem_worried_change", "ind_fem_safety_change", "mental_index_change")

predictors <- c("cases_per_100000", "deaths_per_100000", "dist_prop_covid_zone", 
                "red_zone", "orange_zone", "tran_inc_normal", "asset_index")



results1 <- list()

for (resvar in response_vars) {
  formula1 <- as.formula(paste0(resvar, " ~ dist_prop_covid_zone + red_zone + orange_zone + roster_age_yrs + geo_state | 0 | 0 | geo_district"))
  model1 <- felm(formula1, data = data)
  results1[[paste0(resvar, "1")]] <- model1
  
  # Fit the model with LASSO-selected variables
  formula2 <- as.formula(paste0(resvar, " ~ dist_prop_covid_zone + cases_per_100000 + deaths_per_100000 + red_zone + orange_zone + roster_age_yrs + geo_state + ", paste(lasso_selected_variables, collapse = "+"), "| 0 | 0 | geo_district"))
  model2 <- felm(formula2, data = data)
  results1[[paste0(resvar, "2")]] <- model2
  
}
print(results1)



# Assuming 'models' is your list of fitted model objects
results1 <- bind_rows(
  lapply(names(models), function(model_name) {
    tidy(models[[model_name]]) %>%
      mutate(model = model_name)
  }),
  .id = "model_id"
)

# Filter to avoid intercepts and focus on predictors
results1 <- results1 %>% 
  filter(term != "(Intercept)")

# Create the coefficient plot
ggplot(results1, aes(x = term, y = estimate, color = model)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), width = 0.2, position = position_dodge(width = 0.5)) +
  coord_flip() +
  labs(x = "Predictors", y = "Estimated Coefficients", title = "Effect Sizes of Predictors Across Models") +
  theme_minimal() +
  theme(legend.title = element_blank())



# Filter data to exclude any missing data on the specified variables
data2t <- data %>%
  filter(complete.cases(data[, c(response_vars, predictors, "geo_district")])) %>%
  filter(final_status %in% c("1", "2"))

# Create an empty list to store models
models <- list()

# Loop through each response variable and fit a logistic regression model
for (response in response_vars) {
  formula <- as.formula(paste(response, "~", paste(predictors, collapse = " + ")))
  model <- glm(formula, family = binomial(), data = data2t)
  models[[response]] <- model
}

# Output the summary of each model
lapply(models, summary)


# Model fitting with clustering and LASSO-selected variables (assuming variables from a LASSO model)
variables_of_interest <- c("hh_num_inc_reduced", "ind_meals_reduced", "newf_index", "newm_index")
lasso_selected_variables <- c("cases_per_100000", "deaths_per_100000", "dist_prop_covid_zone", 
                              "red_zone", "orange_zone", "tran_inc_normal", "asset_index") 

results2 <- list()

for (var in variables_of_interest) {
  # Fit the model without LASSO-selected variables
  formula1 <- as.formula(paste0(var, " ~ dist_prop_covid_zone + red_zone + orange_zone + roster_age_yrs + geo_state | 0 | 0 | geo_district"))
  model1 <- felm(formula1, data = data2t)
  results2[[paste0(var, "1")]] <- model1
  
  # Fit the model with LASSO-selected variables
  formula2 <- as.formula(paste0(var, " ~ dist_prop_covid_zone + cases_per_100000 + deaths_per_100000 + red_zone + orange_zone + roster_age_yrs + geo_state + ", paste(lasso_selected_variables, collapse = "+"), "| 0 | 0 | geo_district"))
  model2 <- felm(formula2, data = data2t)
  results2[[paste0(var, "2")]] <- model2
  
}
print(results2)



# Assuming 'models' is your list of fitted model objects
results2 <- bind_rows(
  lapply(names(models), function(model_name) {
    tidy(models[[model_name]]) %>%
      mutate(model = model_name)
  }),
  .id = "model_id"
)

# Filter to avoid intercepts and focus on predictors
results2 <- results2 %>% 
  filter(term != "(Intercept)")

# Create the coefficient plot
ggplot(results2, aes(x = term, y = estimate, color = model)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), width = 0.2, position = position_dodge(width = 0.5)) +
  coord_flip() +
  labs(x = "Predictors", y = "Estimated Coefficients", title = "Effect Sizes of Predictors Across Models") +
  theme_minimal() +
  theme(legend.title = element_blank())

