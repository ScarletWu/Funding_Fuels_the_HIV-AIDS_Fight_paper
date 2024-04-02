library(readr)


col_widths <- fwf_widths(c(5, 3, 2, 4, 1, 3, 2, 4, 3),
                         c("STATEOCC", "CONTYOCC", "ICD34", "RACE_DET", "DEATHMO", "SEX", "ICD9", "AGEREC12", "AGEDET"))

file_path_87 <- "/cloud/project/data/MORT87.PUB"
file_path_88 <- "/cloud/project/data/Vs88mort.raw" 

# Read the files
mort_87_data <- read_fwf(file_path_87, col_positions = col_widths)
mort_88_data <- read_fwf(file_path_88, col_positions = col_widths)

