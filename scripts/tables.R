library(foreign)    # For reading .dta files
library(ggplot2)    # For graphs
library(dplyr)      # For data manipulation
library(lfe)        # For linear models with fixed effects
library(stargazer)  # For creating LaTeX/HTML tables
library(sandwich)   # For robust standard errors

# Set file paths
data_path <- "/cloud/project/data/raw_data/"

data <- read_dta("/cloud/project/data/raw_data/covid_gender_data.dta")

# Prepare data
data$geo_state <- as.factor(data$geo_state)

# Figure 1a: Histogram of Income
ggplot(data, aes(x = tran_inc_normal)) +
  geom_histogram(bins = 30, fill = "gray", color = "black") +
  labs(title = "Distributions of Transformed Income", x = "Income", y = "Density") +
  theme_minimal() +
  ggsave(paste0(output_path, "fig1a.png"))

# Regression Analysis: Impact of COVID-19 on employment
data <- data %>% 
  mutate(inc_lost = tran_inc_normal - tran_inc_current, 
         ind_inc_lost = if_else(inc_lost > 0, 1, NA_real_))

# Fixed Effects Model
model <- felm(ind_inc_lost ~ dist_prop_covid_zone | geo_state, data = data, weights = data$district_sample_size)

# Export Regression Table with robust standard errors
stargazer(model, type = "text", out = paste0(output_path, "regression_results.txt"), se = new("list", list(vcovHC(model, type = "HC1"))))

# Figure 1b: Scatter Plot with Regression Line
ggplot(data, aes(x = dist_prop_covid_zone, y = inc_lost)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Impact of COVID-19 Containment on Income Loss", x = "Containment Level", y = "Income Loss") +
  theme_minimal() +
  ggsave(paste0(output_path, "fig1b.png"))

# Clean up and save workspace
rm(list = setdiff(ls(), "output_path"))
save.image(paste0(output_path, "workspace.RData"))
