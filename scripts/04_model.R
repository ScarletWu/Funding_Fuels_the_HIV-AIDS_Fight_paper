# Load necessary libraries
library(haven)
library(dplyr)
library(glmnet)
library(ggplot2)
library(broom)  
library(caret)

# Paths to the data
data_path <- "/cloud/project/data/raw_data/"
data_file <- paste0(data_path, "covid_gender_data.dta")

# Load the data
data <- read_dta(data_file)

# Identify all predictors including the ones starting with 'asset_'
# Adding a unique call to prevent duplicate names
predictors <- c("dist_prop_covid_zone", "geo_state", "fem_resp_age", "red_zone", "orange_zone", 
                "cases_per_100000", "deaths_per_100000", "tran_inc_normal", "asset_index", 
                grep("^asset_", names(data), value = TRUE))

# Ensure uniqueness of predictors
predictors <- unique(predictors)

# Filter out rows with any NA values across all these predictors and the response variable
data_filtered <- data %>%
  mutate(across(c(geo_state, fem_resp_age, red_zone, orange_zone), as.factor)) %>%
  filter(!is.na(ind_fem_worried_change)) %>%
  filter(rowSums(is.na(select(., all_of(predictors)))) == 0)

# Create the model matrix excluding intercept to mimic Stata's xi: behavior
x_matrix <- model.matrix(~ . - 1, data = data_filtered[, predictors])
y_vector <- data_filtered$ind_fem_worried_change

# Lasso model fitting with cross-validation
lasso_model <- cv.glmnet(x_matrix, y_vector, alpha = 1, family = "binomial")


coef_matrix <- as.matrix(coef(lasso_model, s = "lambda.min"))

# Find the non-zero coefficients
nonzero_coef <- coef_matrix[coef_matrix[, 1] != 0, , drop = FALSE]

# Extract the names of variables corresponding to non-zero coefficients
selected_vars <- rownames(nonzero_coef)

# Print the selected variables
print(selected_vars)




# Display coefficients with their values
selected_coefficients <- nonzero_coef
print(selected_coefficients)

# Validation using a different subset or further cross-validation
# For demonstration, we split the data into a training and test set
set.seed(123)  # for reproducibility
train_indices <- sample(1:nrow(data_filtered), 0.7 * nrow(data_filtered))
train_data <- data_filtered[train_indices, ]
test_data <- data_filtered[-train_indices, ]

# Create model matrix for train and test data
x_train <- model.matrix(~ . - 1, data = train_data[, predictors])
y_train <- train_data$ind_fem_worried_change
x_test <- model.matrix(~ . - 1, data = test_data[, predictors])
y_test <- test_data$ind_fem_worried_change

# Refit Lasso model using only training data
train_lasso_model <- cv.glmnet(x_train, y_train, alpha = 1, family = "binomial")

# Predict on test data
predictions <- predict(train_lasso_model, newx = x_test, s = "lambda.min", type = "response")
predicted_classes <- ifelse(predictions > 0.5, 1, 0)

# Assess model performance
conf_matrix <- confusionMatrix(as.factor(predicted_classes), as.factor(y_test))
print(conf_matrix)

# Plotting the coefficient path
plot(lasso_model, xvar = "lambda", label = TRUE)

# Cross-validated error plot
plot(lasso_model$glmnet.fit, main = "Cross-Validated Error across Lambda Values")
with(lasso_model, plot(lambda, cvm, type='l', lwd=2, xlab="Log(Lambda)", ylab="Mean Squared Error", main="Cross-Validation Plot"))
abline(v=log(lasso_model$lambda.min), col="red", lwd=2)

# Plot non-zero coefficients using ggplot2
coefficients_df <- as.data.frame(nonzero_coef)
coefficients_df$Variable <- rownames(coefficients_df)  # Add variable names as a new column
colnames(coefficients_df)[1] <- "Coefficient"  # Rename the first column to 'Coefficient' for clarity
coefficients_df$Variable <- gsub("_", " ", coefficients_df$Variable)

# Use ggplot2 to plot non-zero coefficients
ggplot(coefficients_df, aes(x = Variable, y = Coefficient)) +
  geom_bar(stat = "identity", fill = "steelblue") +  # Create the bar plot
  geom_text(aes(label = sprintf("%.2f", Coefficient)), vjust = -0.3, size = 3.5, color = "black") +  # Add text labels on bars
  theme_minimal() +  # Apply a minimal theme
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +  # Adjust the text of the x-axis for better visibility
  labs(x = "Predictor Variables", y = "Coefficient Value", title = "Non-Zero Coefficients from Lasso Model")  # Add labels and title


# Save model for future use
saveRDS(train_lasso_model, file = "/cloud/project/outputs/models/lasso_model.rds")

