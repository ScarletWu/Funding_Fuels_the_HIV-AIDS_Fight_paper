library(ggplot2)
library(dplyr)
library(haven)
library(fixest)


covid_data <- read_dta("/cloud/project/data/raw_data/covid_gender_data.dta")
covid_data <- covid_data %>%
  filter(!is.na(tran_inc_normal), !is.na(tran_inc_current))

theme_set(theme_minimal(base_size = 15) +
            theme(text = element_text(family = "Times New Roman"),
                  plot.title = element_text(size = 20),
                  plot.subtitle = element_text(size = 18),
                  axis.title = element_text(size = 18),
                  axis.text = element_text(size = 15)))

# fig1
p1a <- ggplot(covid_data, aes(x = tran_inc_normal)) +
  geom_histogram(aes(y = ..density..), color = "gray", fill = "gray", bins = 30) +
  geom_histogram(data = covid_data, aes(x = tran_inc_current, y = ..density..), 
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


output_path <- "/cloud/project/outputs"

# Load data
data <- read_dta("/cloud/project/data/raw_data/covid_gender_data.dta")

# Prepare the variables
data <- data %>%
  mutate(
    inc_lost = tran_inc_normal - tran_inc_current,
    ind_inc_lost = ifelse(inc_lost > 0, 1, NA),
    ind_meals_reduced = as.numeric(ind_meals_reduced),
    ind_fem_depression_change = as.numeric(ind_fem_depression_change),
    ind_fem_worried_change = as.numeric(ind_fem_worried_change),
    ind_fem_tired_change = as.numeric(ind_fem_tired_change),
    ind_fem_safety_change = as.numeric(ind_fem_safety_change)
  )

# Function to fit model and get tidy output
get_model_summary <- function(dependent_var) {
  model <- lm(as.formula(paste(dependent_var, "~ 1")), data = data)
  tidy_model <- tidy(model, conf.int = TRUE)
  tidy_model$term <- dependent_var
  return(tidy_model)
}

# Run models for each outcome
model_summaries <- bind_rows(
  get_model_summary("ind_inc_lost"),
  get_model_summary("ind_meals_reduced"),
  get_model_summary("ind_fem_depression_change"),
  get_model_summary("ind_fem_worried_change"),
  get_model_summary("ind_fem_tired_change"),
  get_model_summary("ind_fem_safety_change")
)

y_labels <- c(
  "ind_inc_lost" = "Lost Income",
  "ind_meals_reduced" = "Reduced Meals",
  "ind_fem_depression_change" = "More Depressed",
  "ind_fem_worried_change" = "More Anxious",
  "ind_fem_tired_change" = "More Exhausted",
  "ind_fem_safety_change" = "Less Safe"
)

# Plotting
coef_plot <- ggplot(model_summaries, aes(x = estimate, y = term, xmin = conf.low, xmax = conf.high)) +
  geom_pointrange() +
  scale_y_discrete(labels = y_labels) +
  labs(title = "Effect of COVID-19 on Various Outcomes", x = "Percent", y = "") +
  theme_minimal()

# Save the plot
ggsave(paste0(output_path, "/figures/fig1b.png"), plot = coef_plot, width = 10, height = 5, dpi = 300)


# fig2
covid_data <- covid_data %>%
  mutate(
    inc_lost = tran_inc_normal - tran_inc_current,
    ind_inc_lost = ifelse(!is.na(inc_lost) & inc_lost > 0, 1, NA)
  )

est <- feols(ind_inc_lost ~ dist_prop_covid_zone | geo_state, data = covid_data, cluster = ~geo_district)

if (!is.null(est)) {
  print(summary(est))
} else {
  cat("Model estimation was unsuccessful.\n")
}

# 95% confidence interval
results_df <- data.frame(
  Term = "dist_prop_covid_zone",
  Estimate = 0.035612,
  `Std. Error` = 0.043979,
  `Lower CI` = 0.035612 - 1.96 * 0.043979,  
  `Upper CI` = 0.035612 + 1.96 * 0.043979
)

p2 <- ggplot(results_df, aes(x = Estimate, y = Term)) +
  geom_point() +
  geom_errorbarh(aes(xmin = `Lower CI`, xmax = `Upper CI`), height = 0.2) +
  labs(x = "Percent Experiencing This", y = "") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 12),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())

# Save the plot
ggsave("/cloud/project/outputs/figures/fig2.png", plot = p2, width = 10, height = 8, dpi = 300)

# Display the plot
print(p2)