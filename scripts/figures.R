library(haven)
library(fixest)
library(foreign)
library(ggplot2)
library(dplyr)
library(lfe)
library(stargazer)
library(coefplot)
library(broom)
library(lmtest)
library(sandwich)

# Set file paths
data_path <- "/cloud/project/data/raw_data/"
output_path <- "/cloud/project/outputs/"

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


# figure 1b
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

coef_plot <- ggplot(model_summaries, aes(x = estimate, y = term, xmin = conf.low, xmax = conf.high)) +
  geom_pointrange() +
  scale_y_discrete(labels = y_labels) +
  labs(title = "Effect of COVID-19 on Various Outcomes", x = "Percent", y = "") +
  theme_minimal()

ggsave(paste0(output_path, "/figures/fig1b.png"), plot = coef_plot, width = 10, height = 5, dpi = 300)


# figure 2
data$final_status <- as_factor(data$final_status)

data2 <- data %>%
  filter(final_status %in% c("Fully complete", "Partially complete")) %>% 
  group_by(geo_district) %>%
  mutate(
    geo_district = as.factor(geo_district),
    Containment = ave(ind_covid_zone, geo_district, FUN = mean, na.rm = TRUE),
    Depression = mean(ind_fem_depression_change, na.rm = TRUE),
    Exhaustion = mean(ind_fem_tired_change, na.rm = TRUE),
    Anxiety = mean(ind_fem_worried_change, na.rm = TRUE),
    Safety = mean(ind_fem_safety_change, na.rm = TRUE),
    No_Inc = mean(hh_num_inc_reduced, na.rm = TRUE),
    Reduced_meals = mean(ind_meals_reduced, na.rm = TRUE)
  ) %>%
  ungroup()

outcome_vars <- c("Depression", "Exhaustion", "Anxiety", "Safety", "No_Inc", "Reduced_meals")

model_results <- list()

for (var in outcome_vars) {
  model <- lm(as.formula(paste(var, "~ Containment")), data = data2)
  model_results[[var]] <- tidy(model, conf.int = TRUE)
}

plots <- list()
for (var in outcome_vars) {
  summary_df <- model_results[[var]]
  coef_info <- summary_df %>% filter(term == "Containment")
  beta <- coef_info$estimate
  se <- coef_info$std.error
  
  # Create scatter plot with regression line and annotation
  p2 <- ggplot(data2, aes_string(x = "Containment", y = var)) +
    geom_point(aes(weight = district_sample_size), alpha = 0.5) +
    geom_smooth(method = "lm", se = FALSE, color = "blue") +
    geom_text(aes(label = paste0("Beta: ", round(beta, 3), 
                                 "\nSE: ", round(se, 3))),
              x = Inf, y = Inf, hjust = 1.1, vjust = 1.1, size = 3.5) +
    labs(title = paste("Scatter Plot of", var, "vs. Containment"),
         x = "Containment", y = var) +
    theme_minimal() +
    theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
  
  plots[[var]] <- p2
}

for (var in outcome_vars) {
  filename <- paste0(output_path, "figures/plot_2", tolower(var), ".png")
  ggsave(filename, plots[[var]], width = 10, height = 6, dpi = 300)
}







# 2
data$final_status <- factor(data$final_status)

# Filter and summarise data
data2 <- data %>%
  filter(final_status %in% c("Fully complete", "Partially complete")) %>% 
  group_by(geo_district) %>%
  summarise(
    Containment = mean(ind_covid_zone, na.rm = TRUE),
    Depression = mean(ind_fem_depression_change, na.rm = TRUE),
    Exhaustion = mean(ind_fem_tired_change, na.rm = TRUE),
    Anxiety = mean(ind_fem_worried_change, na.rm = TRUE),
    Safety = mean(ind_fem_safety_change, na.rm = TRUE),
    No_Inc = mean(hh_num_inc_reduced, na.rm = TRUE),
    Reduced_meals = mean(ind_meals_reduced, na.rm = TRUE)
  )

outcome_vars <- c("Depression", "Exhaustion", "Anxiety", "Safety", "No_Inc", "Reduced_meals")

model_results <- list()

for (var in outcome_vars) {
  model <- lm(as.formula(paste(var, "~ Containment")), data = data2)
  model_results[[var]] <- tidy(model, conf.int = TRUE)
}

plots <- list()
for (var in outcome_vars) {
  summary_df <- model_results[[var]]
  coef_info <- summary_df %>% filter(term == "Containment")
  beta <- coef_info$estimate
  se <- coef_info$std.error
  
  # Create scatter plot with regression line and annotation
  p <- ggplot(data2, aes(x = Containment, y = get(var))) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", se = FALSE, color = "blue") +
    annotate("text", x = Inf, y = Inf, label = paste0("Beta: ", round(beta, 3), "\nSE: ", round(se, 3)), 
             hjust = 1.05, vjust = 1.05, size = 3.5, color = "black") +
    labs(title = paste("Scatter Plot of", var, "vs. Containment"),
         x = "Containment", y = var) +
    theme_minimal() +
    theme(legend.position = "none", plot.title = element_text(hjust = 0.5), 
          plot.background = element_rect(fill = "white"),
          panel.background = element_rect(fill = "white"), 
          axis.text = element_text(color = "black"), 
          axis.title = element_text(color = "black"), 
          text = element_text(color = "black"))
  
  plots[[var]] <- p
}

for (var in outcome_vars) {
  filename <- paste0(output_path, "figures/plot_2", tolower(var), ".png")
  ggsave(filename, plots[[var]], width = 10, height = 6, dpi = 300)
}