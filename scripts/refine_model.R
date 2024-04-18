library(haven)       # for reading Stata data files
library(dplyr)       # for data manipulation
library(glmnet)      # for LASSO regression
library(hdm)         # for high-dimensional metrics, including double selection LASSO
library(stargazer)   # for producing publication-quality regression tables
library(plm)         # for panel data analysis
library(tidyr)



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

necessary_columns <- c(response_vars, predictors)


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

