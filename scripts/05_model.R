# Load necessary libraries
library(glmnet)

# Preparing data for modeling
predictors <- c("geo_state", "fem_resp_age", "red_zone", "orange_zone", "cases_per_100000", "deaths_per_100000", "tran_inc_normal", "asset_index")
dummies <- model.matrix(~ geo_state + fem_resp_age - 1, data = data_filtered)
x <- cbind(dummies, as.matrix(data_filtered[, predictors]))

# Response variable
y <- data_filtered$ind_fem_worried_change

# LASSO model
set.seed(123)  # for reproducibility
cv_fit <- cv.glmnet(x, y, alpha = 1)
lasso_model <- glmnet(x, y, alpha = 1, lambda = cv_fit$lambda.min)


# Save the LASSO model
saveRDS(lasso_model, file = "/cloud/project/outputs/models/lasso_model.rds")
