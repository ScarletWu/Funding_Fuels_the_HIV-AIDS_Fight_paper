library(haven)       # For read_dta
library(dplyr)       # For data manipulation
library(glmnet)      # For lasso regression
library(corrplot)

# Paths to the data
data_path <- "/cloud/project/data/raw_data/"
data_file <- paste0(data_path, "covid_gender_data.dta")

# Load the data
data <- read_dta(data_file)

# Identify all predictors including the ones starting with 'asset_'
predictors <- c("dist_prop_covid_zone", "geo_state", "fem_resp_age", "red_zone", "orange_zone", "cases_per_100000", "deaths_per_100000", "tran_inc_normal", "asset_index", grep("^asset_", names(data), value = TRUE), "ind_fem_resp_edu")

# Filter out rows with any NA values across all these predictors and the response variable
data_filtered <- data %>%
  filter(!is.na(ind_fem_worried_change)) %>%
  filter(rowSums(is.na(select(., all_of(predictors)))) == 0)

# Build the predictor string for the model matrix
predictor_formula <- paste(predictors, collapse = " + ")
full_formula <- as.formula(paste("~", predictor_formula, "- 1"))

# Create the model matrix from the filtered data
x_matrix <- model.matrix(full_formula, data = data_filtered)

# Create y_vector from the same filtered data
y_vector <- data_filtered$ind_fem_worried_change

# Check dimensions to confirm they match
if (nrow(x_matrix) == length(y_vector)) {
  # Proceed with lasso model
  lasso_model <- cv.glmnet(x_matrix, y_vector, alpha = 1, family = "binomial")
  cat("Lasso model successfully fitted.\n")
} else {
  cat("Mismatch in dimensions found: x_matrix rows:", nrow(x_matrix), ", y_vector length:", length(y_vector), "\n")
}

# Check and add in-sample1 variable if lasso model is fitted
if (exists("lasso_model")) {
  selected <- which(coef(lasso_model, s = "lambda.min")[-1] != 0)
  data_filtered$in_sample1 <- ifelse(seq_len(nrow(data_filtered)) %in% selected, 1, 0)
}

# Plotting the coefficient path
plot(lasso_model, xvar = "lambda", label = TRUE)

# Plotting cross-validated error
plot(lasso_model$glmnet.fit, main = "Lasso Model - Cross-Validated Error")
with(lasso_model, plot(lambda, cvm, type='l', lwd=2, xlab="Log(Lambda)", ylab="Mean Squared Error", main="Cross-Validation Plot"))
abline(v=log(lasso_model$lambda.min), col="red", lwd=2)

# Extracting coefficients at the lambda that minimizes cross-validation error
coef_min_lambda <- coef(lasso_model, s = "lambda.min")
selected_vars <- coef_min_lambda[coef_min_lambda[, 1] != 0, , drop = FALSE]

# Print the selected variables
print(selected_vars)

# Extracting coefficients at the lambda that minimizes cross-validation error
coef_min_lambda <- coef(lasso_model, s = "lambda.min")
selected_vars <- coef_min_lambda[coef_min_lambda[, 1] != 0, , drop = FALSE]

# Print the selected variables
print(selected_vars)

# Convert the sparse matrix to a regular matrix (or vector) and create a data frame
selected_df <- as.data.frame(as.matrix(selected_vars))
selected_df$Variable <- rownames(selected_df)
colnames(selected_df)[1] <- "Coefficient"

# Replace underscores with spaces in variable names
selected_df$Variable <- gsub("_", " ", selected_df$Variable)

# Plot non-zero coefficients
ggplot(selected_df, aes(x = Variable, y = Coefficient)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(x = "Predictor Variables", y = "Coefficient Value", title = "Non-Zero Coefficients from Lasso Model")
