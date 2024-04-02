library(readxl)
library(dplyr)
library(haven)

data_loc <- "/cloud/project/data"

# Import Excel file
ssa_life_tables <- read_excel(file.path(data_loc, "ssa_life_tables_2005.xlsx"), sheet = "Sheet1")

# Temporarily save the data frame to an RDS file
tempfile1 <- tempfile(fileext = ".rds")
saveRDS(ssa_life_tables, tempfile1)

library(readr)

# Define the path to your raw data file
raw_file_path <- "/cloud/project/data/Vs88mort.raw"


vs_raw_data <- read_delim(raw_file_path, 
                          delim = " ", 
                          col_names = c("year", "aids_dths", "age_years", "male", "female"), 
                          col_types = cols(
                            year = col_integer(),
                            aids_dths = col_integer(),
                            age_years = col_integer(),
                            male = col_integer(),
                            female = col_integer()
                          ),
                          trim_ws = TRUE)

# Check the first few rows to ensure it was read correctly
head(vs_raw_data)




# Read the Stata data file
vs_raw_data <- read_csv(file.path(data_loc, "interim/vs_raw_data.csv")) %>%
  filter(year > 1987 & year < 2019, aids_dths == 1, !is.na(age_years)) %>%
  select(year, age_years, male, female) %>%
  rename(age = age_years)

# Load the life tables data from the temporary file
life_tables <- readRDS(tempfile1)

# Merge the data frames
merged_data <- vs_raw_data %>%
  left_join(life_tables, by = c("age" = "age")) %>%
  filter(!is.na(male_life_expectancy), !is.na(female_life_expectancy)) %>%
  mutate(
    from_2018 = 2019 - year,
    years_lost_proj = if_else(male == 1, male_life_expectancy, female_life_expectancy),
    years_lost_proj_10y = if_else(years_lost_proj > 10, 10, years_lost_proj),
    years_lost_through_2018 = if_else(years_lost_proj > from_2018, from_2018, years_lost_proj),
    years_lost_through_2018_10y = if_else(years_lost_through_2018 > 10, 10, years_lost_through_2018)
  ) %>%
  group_by(year) %>%
  summarise(
    mean_years_lost_through_2018 = mean(years_lost_through_2018, na.rm = TRUE),
    mean_years_lost_through_2018_10y = mean(years_lost_through_2018_10y, na.rm = TRUE)
  ) %>%
  ungroup()

# Save the processed data back to a csv file
write_csv(merged_data, file.path(data_loc, "interim/ann_years_lost_aids.csv"))
