library(dplyr)
library(readr)
library(readxl)
library(haven)
library(tidyr)


data_loc <- "/cloud/project/data" 


# 1994 AIDS cases
aids_cases_1994 <- read_tsv(file.path(data_loc, "aids_cases_rwca_1994.txt")) %>%
  filter(!is.na(`Location Code`)) %>%
  select(Cases, `Location Code`, Location) %>%
  rename(cityfip = `Location Code`, aids_reported_by_1995 = Cases)
write_csv(aids_cases_1994, file.path(data_loc, "interim/aids_cases_rwca_1994.csv"))

# 1995 AIDS cases
aids_cases_1995 <- read_tsv(file.path(data_loc, "aids_cases_rwca_1995.txt")) %>%
  filter(!is.na(`Location Code`)) %>%
  select(Cases, `Location Code`, Location) %>%
  rename(cityfip = `Location Code`, aids_reported_in_1995 = Cases)
write_csv(aids_cases_1995, file.path(data_loc, "interim/aids_cases_rwca_1995.csv"))

title1_info <- read_excel(file.path(data_loc, "t1years.xlsx"), sheet = "Sheet1") %>%
  filter(city != "Honolulu, HI", city != "San Juan, PR") %>%
  mutate(
    state_postal = substr(city, nchar(city)-1, nchar(city)),
    state_postal = if_else(state_postal == "M.", "NM", state_postal),
    cityfip = as.character(cityfip)  
  ) %>%
  left_join(read_csv(file.path(data_loc, "interim/aids_cases_rwca_1995.csv"), col_types = cols(cityfip = col_character())), by = "cityfip") %>%
  left_join(read_csv(file.path(data_loc, "interim/aids_cases_rwca_1994.csv"), col_types = cols(cityfip = col_character())), by = "cityfip") %>%
  mutate(
    aids_ever_1995_imp = round(aids_reported_by_1995 + 0.25 * aids_reported_in_1995),
    rank_1995 = row_number()
  ) %>%
  select(cityfip, year_rwca_status, aids_ever_1995_imp, rank_1995, state_postal)

write_csv(title1_info, file.path(data_loc, "interim/t1years.csv"))


# AIDS cases reported by year
aids_cases_reported <- read_tsv(file.path(data_loc, "aids_cases_rep_year.txt"), show_col_types = FALSE) %>%
  select(-Notes, `Year Reported Code`, Location, `Location Code`, Cases) %>%
  rename(year = `Year Reported Code`, 
         cityfip = `Location Code`, 
         aids_rep = Cases) %>%
  mutate(year = as.integer(year),
         cityfip = as.character(cityfip), 
         aids_rep = as.integer(aids_rep))
write_csv(aids_cases_reported, file.path(data_loc, "interim/aids_cases_reported.csv"))

# AIDS cases diagnosed by year
aids_cases_diagnosed <- read_tsv(file.path(data_loc, "aids_cases_diag_year.txt"), show_col_types = FALSE) %>%
  mutate(
    aids_diag_apids = as.integer(Cases),
    year = as.integer(`Year Diagnosed Code`),
    cityfip = as.character(`Location Code`)  
  ) %>%
  filter(!is.na(cityfip)) %>%
  select(aids_diag_apids, cityfip, year)
write_csv(aids_cases_diagnosed, file.path(data_loc, "interim/aids_cases_diagnosed.csv"))

cdc_aids_diagnosis <- read_csv(paste0(data_loc, "/data/AIDS Diagnosis by MSA.csv")) %>%
  pivot_longer(cols = !locationcode, names_to = "year", values_to = "aids_diagnosis_all") %>%
  mutate(cityfip = as.numeric(locationcode))

# Save processed data
write_csv(cdc_aids_diagnosis, paste0(data_loc, "/interim/cdc_city_aids_diagnosis.csv"))


aids_prev <- read_csv(paste0(data_loc, "/AIDS Prevalence by MSA.csv")) %>%
  pivot_longer(cols = -locationcode, names_to = "year", values_to = "aids_prev_all") %>%
  mutate(cityfip = as.numeric(locationcode),
         year = as.numeric(year)) %>%
  filter(year %in% c(1990, 2006, 2012, 2018)) %>%
  mutate(aids_prev_all = if_else(cityfip == 3160, NA_real_, aids_prev_all))

# Save the processed data
write_csv(aids_prev, paste0(data_loc, "/interim/cdc_city_aids_prev.csv"))


hiv_prev <- read_csv(paste0(data_loc, "/HIV Prevalence by MSA.csv")) %>%
  select(locationcode, hiv_prev_all2008) %>%
  rename(cityfip = locationcode,
         hiv_prev = hiv_prev_all2008)

# Save the processed data
write_csv(hiv_prev, paste0(data_loc, "/interim/cdc_city_hiv_prev.csv"))





