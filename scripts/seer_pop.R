library(readr)

col_positions <- fwf_positions(
  start = c(1, 5, 7, 9, 12, 14, 15, 16, 17, 19),
  end = c(4, 6, 8, 11, 13, 14, 15, 16, 18, 26),
  col_names = c("year", "state_postal", "statefip", "countyfip", "registry", 
                "race", "origin", "sex", "age", "population")
)

file_path <- "/cloud/project/data/us.1969_2018.singleages.adjusted.txt"

# Read the data
seer_population_data <- read_fwf(file = file_path, col_positions = col_positions)


write_csv(seer_data, "path/to/your/processed_data.csv")
