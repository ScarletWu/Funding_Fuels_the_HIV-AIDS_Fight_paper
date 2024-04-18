# Load necessary libraries
library(dplyr)
library(ggplot2)
library(lme4)
library(Metrics)
library(caret)
library(randomForest)

# Load data
data_path <- "/cloud/project/data/raw_data/"
data_file <- paste0(data_path, "covid_gender_data.dta")
data <- read_dta(data_file)

# Filter data based on non-missing values for certain variables and other conditions
filtered_data <- data %>%
  filter(
    !is.na(ind_fem_depression_change) &  # Ensure no missing data on depression change indicator
      !is.na(ind_fem_tired_change) &       # Ensure no missing data on tiredness change indicator
      !is.na(ind_fem_worried_change) &     # Ensure no missing data on worry change indicator
      !is.na(ind_fem_safety_change) &      # Ensure no missing data on safety change indicator
      final_status == 1 | final_status == 2  # Final status must be 1 or 2
  )

# You might also need to include epidemiological variables if they are used as part of the conditions
# For example, ensuring non-missing values for cases and deaths per 100,000 if they are used in the models
filtered_data <- filtered_data %>%
  filter(
    !is.na(cases_per_100000) & 
      !is.na(deaths_per_100000) &
      cases_per_100000 >= 0 &   # Example condition: cases must be non-negative
      deaths_per_100000 >= 0    # Example condition: deaths must be non-negative
  )

# Assuming additional demographic controls are needed:
filtered_data <- filtered_data %>%
  filter(
    !is.na(geo_state) &       # Ensure geographic state data is not missing
      !is.na(fem_resp_age) &    # Ensure respondent's age is not missing
      fem_resp_age >= 18        # Example condition: respondent must be an adult
  )

# Output filtered data for inspection or further analysis
write.csv(filtered_data, "/cloud/project/data/cleaned/Filtered_Covid_Gender_Data.csv", row.names = FALSE)

# Summary to check how many rows are left after filtering
print(paste("Filtered dataset contains", nrow(filtered_data), "observations."))
