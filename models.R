# Load libraries
library(data.table)
library(glmnet)
library(rpart)

# Load data
train_interim <- fread("./project/volume/data/interim/train_interim.csv")
test_interim <- fread("./project/volume/data/interim/test_interim.csv")

# Cross-validation setup
train_control <- trainControl(method = "cv", number = 5)

# Define model formula
model_formula <- ic50_Omicron ~ dose_3 + dose_2 + age + age_squared + dose_age_interaction + log_age + .

# Ridge model
ridge_model <- train(
  model_formula,
  data = train_interim,
  method = "glmnet",
  trControl = train_control,
  tuneGrid = expand.grid(alpha = 0, lambda = seq(0.001, 0.2, by = 0.005))
)

# Lasso model
lasso_model <- train(
  model_formula,
  data = train_interim,
  method = "glmnet",
  trControl = train_control,
  tuneGrid = expand.grid(alpha = 1, lambda = seq(0.001, 0.2, by = 0.005))
)

# Tree model
tree_model <- rpart(
  model_formula,
  data = train_interim,
  control = rpart.control(cp = 0.01, minsplit = 15, maxdepth = 6)
)

# Model predictions
ridge_predictions <- predict(ridge_model, newdata = test_interim)
lasso_predictions <- predict(lasso_model, newdata = test_interim)
tree_predictions <- predict(tree_model, newdata = test_interim)

# Model RMSEs
ridge_rmse <- min(ridge_model$results$RMSE)
lasso_rmse <- min(lasso_model$results$RMSE)
tree_rmse <- RMSE(predict(tree_model, train_interim), train_interim$ic50_Omicron)

# Model weights
ridge_weight <- 1 / ridge_rmse
lasso_weight <- 1 / lasso_rmse
tree_weight <- 1 / tree_rmse

# Total weight
total_weight <- ridge_weight + lasso_weight + tree_weight

# Ensemble predictions
ensemble_predictions <- (ridge_weight * ridge_predictions + lasso_weight * lasso_predictions + tree_weight * tree_predictions) / total_weight

# Save models
saveRDS(ridge_model, "./project/volume/models/ridge_model.rds")
saveRDS(lasso_model, "./project/volume/models/lasso_model.rds")
saveRDS(tree_model, "./project/volume/models/tree_model.rds")

# Save results
test <- fread("./project/volume/data/interim/test.csv")
results <- data.table(sample_id = test$sample_id, ic50_Omicron = ensemble_predictions)
fwrite(results, "./project/volume/data/processed/submit.csv")