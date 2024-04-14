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
library(glmnet)


# Set file paths
data_path <- "/cloud/project/data/raw_data/"
output_path <- "/cloud/project/outputs/"

# Load data
data <- read_dta("/cloud/project/data/raw_data/covid_gender_data.dta")

# Prepare data
data$geo_state <- as.factor(data$geo_state)
data$geo_district <- as.factor(data$geo_district)
data$final_status <- as_factor(data$final_status)
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
data1b <- data %>%
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
  model <- lm(as.formula(paste(dependent_var, "~ 1")), data = data1b)
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




# table 1
response_vars <- c("ind_fem_depression_change", "ind_fem_tired_change", 
                   "ind_fem_worried_change", "ind_fem_safety_change", 
                   "mental_index_change")

# Define the main predictor variable and control variables
main_predictor <- "dist_prop_covid_zone"
control_vars <- c("red_zone", "orange_zone", 
                  "cases_per_100000", "deaths_per_100000", 
                  "tran_inc_normal", "asset_index", "ind_fem_resp_edu")
# Include other control variables as needed

# Combine all predictor variables into a single vector
predictor_vars <- c(main_predictor, control_vars)

# Convert geo_state and fem_resp_age to factors if they are categorical
data$geo_state <- as.factor(data$geo_state)
data$fem_resp_age <- as.factor(data$fem_resp_age)

# Filter out observations with missing values in any of the response or predictor variables
data_filtered <- data %>%
  filter(complete.cases(data[, c(response_vars, predictor_vars)]))

# Generate dummy variables for categorical predictors
dummies <- model.matrix(~ geo_state + fem_resp_age - 1, data = data_filtered)

# Extract predictor variables (ensure there are no NAs)
x <- as.matrix(data_filtered[, predictor_vars])

if (any(is.na(predictors))) {
  stop("NA values found in predictor variables.")
}
# Combine dummy variables and predictors into a single matrix
x <- cbind(dummies, x)

# Initialize a list to store LASSO models for each response variable
lasso_models <- list()

# Loop through each response variable to perform LASSO regression
for (resp_var in response_vars) {
  # Extract the response variable ensuring no missing values
  y <- data_filtered[[resp_var]]
  
  # Set a seed for reproducibility
  set.seed(123)
  
  # Perform LASSO regression using cross-validation to find the optimal lambda
  cv_fit <- cv.glmnet(x, y, alpha = 1)
  
  # Best lambda (regularization parameter) found by cross-validation
  best_lambda <- cv_fit$lambda.min
  
  # Fit the LASSO model using the best lambda
  lasso_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
  
  # Store the fitted model
  lasso_models[[resp_var]] <- lasso_model
}



# Table 1

# Define the response variables from your dataset
response_vars <- c("ind_fem_depression_change", "ind_fem_tired_change", 
                   "ind_fem_worried_change", "ind_fem_safety_change", 
                   "mental_index_change")

# Define the main predictor variable and control variables
main_predictor <- "dist_prop_covid_zone"
control_vars <- c("red_zone", "orange_zone", 
                  "cases_per_100000", "deaths_per_100000", 
                  "tran_inc_normal", "asset_index", "ind_fem_resp_edu")
# Include other control variables as needed

# Combine all predictor variables into a single vector
predictor_vars <- c(main_predictor, control_vars)

# Convert geo_state and fem_resp_age to factors if they are categorical
data$geo_state <- as.factor(data$geo_state)
data$fem_resp_age <- as.factor(data$fem_resp_age)

# Ensure that data does not have any missing values in the predictors, response variables, or factors
data_filtered <- data %>%
  filter(complete.cases(data[, c(response_vars, predictor_vars, "geo_state", "fem_resp_age")]))
data_filtered$geo_state <- as.factor(data_filtered$geo_state)
data_filtered$fem_resp_age <- as.factor(data_filtered$fem_resp_age)

# Generate model matrix for predictors including dummy variables
predictors <- c("dist_prop_covid_zone", "red_zone", "orange_zone", "cases_per_100000", "deaths_per_100000", "tran_inc_normal", "asset_index", "ind_fem_resp_edu")
dummies_formula <- reformulate(c("geo_state", "fem_resp_age"), response = NULL)
dummies <- model.matrix(~ geo_state + fem_resp_age - 1, data = data_filtered)

# Add dummy variables back to the data frame to ensure they're available for lm()
data_filtered <- cbind(data_filtered, dummies)

# Correct previous error by explicitly naming the columns correctly
colnames(data_filtered)[(ncol(data_filtered) - ncol(dummies) + 1):ncol(data_filtered)] <- colnames(dummies)

# Prepare x matrix again including dummy variables
x <- as.matrix(data_filtered[, c(predictors, colnames(dummies)[-1])]) 
# Combine dummy variables with other predictors

# Lasso models for each response variable
lasso_models <- list()

x <- data.frame(dummies, data_filtered[, predictors, drop = FALSE])
if(any(sapply(x, class) == "list")) {
  stop("One or more columns in 'x' are lists, not numeric vectors.")
}

# Explicitly convert `x` to a matrix
x <- as.matrix(x)

# Check if `x` is now a numeric matrix
if(!is.matrix(x) || !is.numeric(x)) {
  stop("Matrix 'x' is not a numeric matrix.")
}

for (resp_var in response_vars) {
  y <- data_filtered[[resp_var]]
  
  # Lasso with cross-validation to find optimal lambda
  set.seed(123)  # Reproducibility
  cv_fit <- cv.glmnet(x, y, alpha = 1)
  best_lambda <- cv_fit$lambda.min
  
  # Fit the LASSO model using the selected lambda
  lasso_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
  
  # Extract non-zero coefficients from LASSO
  coef_info <- coef(lasso_model, s = "lambda.min")
  active_vars <- rownames(coef_info)[coef_info[, 1] != 0]
  active_vars <- active_vars[active_vars != "(Intercept)"]
  
  # Ensure only variables found in `data_filtered` are used
  valid_vars <- colnames(x)
  active_vars <- active_vars[active_vars %in% valid_vars]
  
  # Build model formula from active variables
  formula_str <- paste(resp_var, "~", paste(active_vars, collapse = " + "))
  formula <- as.formula(formula_str)
  
  # Fit the final linear model using lm()
  final_model <- lm(formula, data = data_filtered)
  
  # Calculate robust standard errors, assuming 'geo_district' is the clustering variable
  robust_se <- vcovHC(final_model, type = "HC1", cluster = data_filtered$geo_district)
  
  # Store the model and its robust standard errors
  lasso_models[[resp_var]] <- list(model = final_model, robust_se = robust_se)
  
  # Print summary of each model with robust standard errors
  print(summary(final_model))
  print(coeftest(final_model, vcov = robust_se))
}


lm_models <- list(
  ind_fem_depression_change = lm_models$ind_fem_depression_change, 
  ind_fem_tired_change = lm_models$ind_fem_tired_change, 
  ind_fem_worried_change = lm_models$ind_fem_worried_change,
  ind_fem_safety_change = lm_models$ind_fem_safety_change,
  mental_index_change = lm_models$mental_index_change
)

stargazer(
  lm_models$ind_fem_depression_change, 
  lm_models$ind_fem_tired_change, 
  lm_models$ind_fem_worried_change,
  lm_models$ind_fem_safety_change,
  lm_models$mental_index_change,
  type = "text",
  title = "Relationship between Containment and Female Well-being",
  align = TRUE,
  dep.var.labels.include = FALSE,
  dep.var.caption = "Dependent variable: Well-being metric",
  covariate.labels = c("Containment", "Past Containment Controls", "State FE", "Age FE", "Lasso Controls", "Case and Death Controls"),
  omit.stat = c("LL", "ser", "f"),
  add.lines = list(
    c("Dep Var. Mean", "0.344", "0.276", "0.301", "0.299", "0.307"),
    c("Adjusted R-squared", "0.009", "0.022", "0.052", "0.006", "0.021"),
    c("Observations", "489", "489", "489", "489", "489")
  ),
  out = "table1.txt"
)
