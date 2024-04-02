library(readr)
library(dplyr)

# Define the path to your SEER population data file and other files
seer_file_path <- "/cloud/project/data/us.1969_2018.singleages.adjusted.txt"
city_county_cw_path <- "/path/to/your/city_county_cw.csv"
apids_cities_path <- "/path/to/your/apids_cities.csv"
vs_cty_year_path <- "/path/to/your/vs_cty_year.csv"

# Define column positions and types for the SEER population data
col_types <- fwf_positions(
  start = c(1, 5, 7, 9, 12, 14, 15, 16, 17, 19),
  end = c(4, 6, 8, 11, 13, 14, 15, 16, 18, 26),
  col_names = c("year", "state_postal", "statefip", "countyfip", "registry", "race", "origin", "sex", "age", "population"),
  col_types = cols(
    year = col_integer(),
    state_postal = col_character(),
    statefip = col_integer(),
    countyfip = col_integer(),
    registry = col_integer(),
    race = col_integer(),
    origin = col_integer(),
    sex = col_integer(),
    age = col_integer(),
    population = col_double()
  )
)

# Import SEER at State level
seer_data <- read_fwf(seer_file_path, col_positions = col_types) %>%
  filter(year > 1987) %>%
  mutate(
    pop_male = if_else(sex == 1, population, NA_real_),
    pop_female = if_else(sex == 2, population, NA_real_),
    pop_white = if_else(race == 1, population, NA_real_),
    pop_black = if_else(race == 2, population, NA_real_),
    pop_othrace = if_else(race == 3, population, NA_real_),
    pop_0_17 = if_else(age < 18, population, NA_real_),
    pop_18_64 = if_else(age >= 18 & age < 65, population, NA_real_),
    pop_65p = if_else(age >= 65, population, NA_real_),
    pop_all = population
  ) %>%
  group_by(year, countyfip, statefip) %>%
  summarise(across(starts_with("pop"), sum, na.rm = TRUE), .groups = "drop")

city_county_cw <- read_csv(city_county_cw_path)
apids_cities <- read_csv(apids_cities_path)

