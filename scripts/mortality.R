library(dplyr)
library(readr)
library(readxl)
library(stringr)

# Define the data location
data_loc <- "path/to/your/data"
do_loc <- "path/to/dictionary/location"

# Adjust the paths to your actual files
mortality_file_path <- file.path(data_loc, "data_restricted/mortality_from_cdc/MortAC1995/MULT1995.AllCnty/MULT1995.AllCnty.txt")
vital_stats_1995_dict_path <- file.path(do_loc, "build/vital_stats_1995_dict.dct")
