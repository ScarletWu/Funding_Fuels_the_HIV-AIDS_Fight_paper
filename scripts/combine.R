# Load necessary libraries
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("haven", quietly = TRUE)) install.packages("haven")

library(dplyr)
library(haven)

# Define your data location (change paths as necessary)
data_loc <- "/path/to/your/data"

# Load your data
seer_info_by_city <- read_dta(file.path(data_loc, "interim/seer_info_by_city.dta"))
seer_info_by_city_hisp <- read_dta(file.path(data_loc, "interim/seer_info_by_city_hisp.dta"))
vs_city_year <- read_dta(file.path(data_loc, "interim/vs_city_year.dta"))
laus_city_data <- read_dta(file.path(data_loc, "interim/laus_city_data.dta"))
t1years <- read_dta(file.path(data_loc, "interim/t1years.dta"))
aids_cases_reported <- read_dta(file.path(data_loc, "interim/aids_cases_reported.dta"))
aids_cases_diagnosed <- read_dta(file.path(data_loc, "interim/aids_cases_diagnosed.dta"))
cdc_city_aids_prev <- read_dta(file.path(data_loc, "interim/cdc_city_aids_prev.dta"))
cdc_city_aids_diagnosis <- read_dta(file.path(data_loc, "interim/cdc_city_aids_diagnosis.dta"))
rwca_non_city <- read_dta(file.path(data_loc, "interim/rwca_non_city.dta"))
agg_title1_funding_by_city <- read_dta(file.path(data_loc, "interim/agg_title1_funding_by_city.dta"))
state_year_controls <- read_dta(file.path(data_loc, "interim/state_year_controls.dta"))
vs_city_age_adjusted <- read_dta(file.path(data_loc, "interim/vs_city_age_adjusted.dta"))
ann_years_lost_aids <- read_dta(file.path(data_loc, "interim/ann_years_lost_aids.dta"))

# Example merge and manipulation (repeating this pattern for each merge)
merged_data <- seer_info_by_city %>%
  left_join(seer_info_by_city_hisp, by = c("year", "cityfip")) %>%
  left_join(vs_city_year, by = c("cityfip", "year")) %>%
  left_join(laus_city_data, by = c("cityfip", "year")) %>%
  # Continue with the other datasets as above
  # After all merges, perform any necessary mutations or filtering
  mutate(aids_dths_all_100k = aids_dths_all / pop_all * 100000,
         l_aids_dths_all_100k = log(aids_dths_all_100k))

# Continue with your data manipulation and analysis
# Save the final processed data
write_dta(merged_data, file.path(data_loc, "cleaned/rwca_sample.dta"))
