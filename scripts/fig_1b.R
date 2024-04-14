# Load the necessary packages
library(foreign) 
library(ggplot2) 
library(dplyr)  
library(broom) 
library(lmtest)
library(sandwich) 
1
# Set file paths
data_path <- "/cloud/project/data/raw_data/"
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
