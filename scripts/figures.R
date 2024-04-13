library(haven)
library(fixest)
library(foreign)
library(ggplot2)
library(dplyr)
library(lfe)
library(stargazer)
library(coefplot)
library(broom)

data_path <- "/cloud/project/data/raw_data/"
output_path <- "/cloud/project/outputs"

# Load data
data <- read_dta("/cloud/project/data/raw_data/covid_gender_data.dta")

# Prepare data
data$geo_state <- as.factor(data$geo_state)
data$geo_district <- as.factor(data$geo_district)

# Figure 1a: Histogram of Income
p1a <- ggplot(data, aes(x = tran_inc_normal)) +
  geom_histogram(aes(y = ..density..), color = "gray", fill = "gray", bins = 30) +
  geom_histogram(data = data, aes(x = tran_inc_current, y = ..density..), 
                 color = "black", fill = NA, bins = 30) +
  labs(title = "Distributions of Transformed Income",
       x = "Inverse Hyperbolic Sin of Income (in Rs.)",
       y = "Density") +
  scale_fill_manual(values = c("gray", "black"),
                    labels = c("Normal Month", "Current Month")) +
  scale_color_manual(values = c("gray", "black"),
                     labels = c("Normal Month", "Current Month")) +
  theme(legend.title = element_blank(), legend.position = "bottom")

ggsave("/cloud/project/outputs/figures/fig1a.png", plot = p1a, width = 10, height = 8, dpi = 300)


# Regression Analysis: Impact of COVID-19 on employment

data <- mutate(data,
               tran_inc_normal = as.numeric(tran_inc_normal),
               tran_inc_current = as.numeric(tran_inc_current),
               inc_lost = tran_inc_normal - tran_inc_current,
               ind_inc_lost = if_else(inc_lost > 0, 1, NA_real_)
)

# Fixed Effects Model
model <- felm(ind_inc_lost ~ dist_prop_covid_zone | geo_state, data = data)

# Tidy model and extract coefficients
tidy_model <- tidy(model, conf.int = TRUE) %>%
  filter(term == "dist_prop_covid_zone")

# Coefficient plot (Based on felm model results)
coef_plot <- ggplot(tidy_model, aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_pointrange() +
  coord_flip() +
  theme_minimal() +
  labs(title = "Coefficient Plot", x = "Term", y = "Estimate")

# Fixed Effects Model

ggsave("/cloud/project/outputs/figures/fig1b.png", plot = p1b, width = 10, height = 8, dpi = 300)

