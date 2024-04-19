# Load necessary libraries

data_path <- "/cloud/project/data/cleaned/"
data_file <- paste0(data_path, "Filtered_Covid_Gender_Data.csv")
data <- read.csv(data_file)
# Check for Missing Values
test_missing_values <- any(is.na(data))

# Test for Unique IDs
test_unique_ids <- length(unique(data$geo_uq_hhid_str)) == nrow(data)

# Validate Final Status Values
test_final_status <- all(data$final_status %in% c(1, 2)) # Assuming 1 and 2 are the only valid values

# Confirm State IDs Correspond to States
test_state_ids <- all(data$geo_state == substr(data$geo_state_id_str, 1, 2))

# District Consistency Check
test_district_consistency <- all(data$geo_district == substr(data$geo_district_id_str, 1, 3))

# Check Population Figures
test_population_figures <- all(data$district_population > 0 & data$district_population < 1000000) # Example range

# Case Numbers Sanity Check
test_case_numbers <- all(data$total_cases >= 0 & data$total_deaths >= 0)

# Verify Proportion in COVID Zone
test_covid_zone_proportion <- all(data$dist_prop_covid_zone >= 0 & data$dist_prop_covid_zone <= 1)

# Check Zone Assignments
test_zone_assignments <- all(!(data$red_zone == 1 & data$orange_zone == 1))

# Test Sample Sizes
test_sample_sizes <- all(data$district_sample_size <= data$district_population)

# Combine all tests into a list
test_results <- list(
  test_missing_values = test_missing_values,
  test_unique_ids = test_unique_ids,
  test_final_status = test_final_status,
  test_state_ids = test_state_ids,
  test_district_consistency = test_district_consistency,
  test_population_figures = test_population_figures,
  test_case_numbers = test_case_numbers,
  test_covid_zone_proportion = test_covid_zone_proportion,
  test_zone_assignments = test_zone_assignments,
  test_sample_sizes = test_sample_sizes
)

# Output the results
test_results
