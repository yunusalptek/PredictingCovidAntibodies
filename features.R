# Load libraries
library(data.table)
library(caret)
library(Metrics)

# Load datasets
train <- fread("./project/volume/data/raw/Stat_380_train.csv")
test <- fread("./project/volume/data/raw/Stat_380_test.csv")
covar <- fread("./project/volume/data/raw/covar_data.csv")

# Identify numeric columns
numeric_cols_train <- sapply(train, is.numeric)
numeric_cols_test <- sapply(test, is.numeric)

# Calculate median for each numeric column
medians_train <- sapply(train[, numeric_cols_train, with = FALSE], median, na.rm = TRUE)
medians_test <- sapply(test[, numeric_cols_test, with = FALSE], median, na.rm = TRUE)

# Replace NAs with median
for (col in names(medians_train)) {
  train[[col]][is.na(train[[col]])] <- medians_train[col]
}
for (col in names(medians_test)) {
  test[[col]][is.na(test[[col]])] <- medians_test[col]
}

# Ensure "age" is numeric
train$age <- as.numeric(train$age)
test$age <- as.numeric(test$age)

# Encode categorical features
train$dose_3[is.na(train$dose_3)] <- 'Unknown'
train$dose_3 <- as.numeric(factor(train$dose_3))
train$dose_2 <- as.numeric(factor(train$dose_2))
test$dose_3[is.na(test$dose_3)] <- 'Unknown'
test$dose_3 <- as.numeric(factor(test$dose_3))
test$dose_2 <- as.numeric(factor(test$dose_2))

# Merge covar
train <- merge(train, covar, by = "sample_id", all.x = TRUE)
test <- merge(test, covar, by = "sample_id", all.x = TRUE)

# Remove columns with one unique value
unique_counts_train <- apply(train, 2, unique)
columns_to_keep_train <- sapply(unique_counts_train, length) > 1
train <- train[, columns_to_keep_train, with = FALSE]
unique_counts_test <- apply(test, 2, unique)
columns_to_keep_test <- sapply(unique_counts_test, length) > 1
test <- test[, columns_to_keep_test, with = FALSE]

# Save test
fwrite(test, "./project/volume/data/interim/test.csv")

# Feature engineering
train[, age_squared := age ^ 2]
train[, dose_age_interaction := dose_3 * age]
train[, log_age := log(age + 1)]
test[, age_squared := age ^ 2]
test[, dose_age_interaction := dose_3 * age]
test[, log_age := log(age + 1)]

# Remove "sample_id"
train <- train[, !"sample_id", with = FALSE]
test <- test[, !"sample_id", with = FALSE]

# Handle missing values in "ic50_Omicron"
train$ic50_Omicron[is.na(train$ic50_Omicron)] <- median(train$ic50_Omicron, na.rm = TRUE)

# Identify numeric columns
numeric_cols_train <- sapply(train, is.numeric)
numeric_cols_test <- sapply(test, is.numeric)

# Calculate median for each numeric column
medians_train <- sapply(train[, numeric_cols_train, with = FALSE], median, na.rm = TRUE)
medians_test <- sapply(test[, numeric_cols_test, with = FALSE], median, na.rm = TRUE)

# Replace NAs with median
for (col in names(medians_train)) {
  train[[col]][is.na(train[[col]])] <- medians_train[col]
}
for (col in names(medians_test)) {
  test[[col]][is.na(test[[col]])] <- medians_test[col]
}

# Save data
fwrite(train, "./project/volume/data/interim/train_interim.csv")
fwrite(test, "./project/volume/data/interim/test_interim.csv")