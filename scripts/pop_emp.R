library(readr)

data_file_path <- paste0(data_loc, "/data_public/us.1969_2018.singleages.adjusted.txt")

seer_population_data <- read_fwf(file = data_file_path,
                                 col_positions = fwf_positions(
                                   start = c(1, 5, 7, 9, 12, 14, 15, 16, 17, 19),
                                   end = c(4, 6, 8, 11, 13, 14, 15, 16, 18, 26),
                                   col_names = c("year", "state_postal", "statefip", "countyfip", "registry", "race", "origin", "sex", "age", "population")
                                 ),
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



