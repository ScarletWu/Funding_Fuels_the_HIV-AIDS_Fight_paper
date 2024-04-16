library(haven)       # For read_dta
library(dplyr)       # For data manipulation
library(plm)         # For panel data models
library(stargazer)   # For creating LaTeX tables
library(lfe)         # For high-dimensional fixed effects (similar to lasso controls in Stata)

# Paths to the data
data_path <- "/cloud/project/data/raw_data/"
data_file <- paste0(data_path, "covid_gender_data.dta")

# Load the data
data <- read_dta(data_file)

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

# Define the panel data structure
pdata <- pdata.frame(data_filtered, index = c("geo_state", "time"))

# Placeholder for models (adjust according to your actual variable names and fixed effects)
# Remember, the `lfe` package allows you to incorporate high-dimensional fixed effects
models <- list()
outcome_vars <- c("ind_fem_depression_change", "ind_fem_tired_change", "ind_fem_worried_change", "ind_fem_safety_change")

for (outcome_var in outcome_vars) {
  # Without lasso controls - only state and age FEs
  formula_no_lasso <- as.formula(paste0(outcome_var, " ~ dist_prop_covid_zone + geo_state + fem_resp_age"))
  model_no_lasso <- plm(formula_no_lasso, data = pdata, model = "within")
  models[[paste0(outcome_var, "_no_lasso")]] <- model_no_lasso
  
  # With lasso - using lfe package
  # Note: You'll need to determine how to select your controls, `lfe` can handle many fixed effects
  formula_with_lasso <- as.formula(paste0(outcome_var, " ~ dist_prop_covid_zone + getfe(felm(", outcome_var, "~ geo_state + fem_resp_age + red_zone + orange_zone))"))
  model_with_lasso <- felm(formula_with_lasso, data = pdata)
  models[[paste0(outcome_var, "_with_lasso")]] <- model_with_lasso
}

# Creating the table (adjust the names according to the actual models)
stargazer(models$ind_fem_depression_change_no_lasso, models$ind_fem_depression_change_with_lasso, ...,
          type = "latex", out = "table1.tex",
          title = "Relationship between Containment and Female Well-being",
          align = TRUE,
          column.labels = c("(1)", "(2)"),
          covariate.labels = c("Containment", "State FE", "Age FE", "Lasso Controls", "Case and Death Controls"),
          omit.stat = c("LL", "AIC", "BIC"),
          label = "tab:mentalhealth",
          table.placement = "H")
