# Load necessary libraries
library(dplyr)
library(haven)

# Load data
data_path <- "/cloud/project/data/raw_data/"
data_file <- paste0(data_path, "covid_gender_data.dta")
data <- read_dta(data_file)

# Clean and prepare data
data$geo_state <- as.factor(data$geo_state)
data$geo_district <- as.factor(data$geo_district)
data$final_status <- as_factor(data$final_status)
data$fem_resp_age <- as.factor(data$fem_resp_age)

# Ensure no missing values in key variables
data_filtered <- data %>%
  filter(complete.cases(data[, c("ind_fem_depression_change", "ind_fem_tired_change", "ind_fem_worried_change", "ind_fem_safety_change", "mental_index_change", "geo_state", "fem_resp_age")]))

# Save the cleaned data to a CSV file
cleaned_data_file <- "/cloud/project/data/cleaned/cleaned_data.csv"
write.csv(data_filtered, file = cleaned_data_file, row.names = FALSE)
