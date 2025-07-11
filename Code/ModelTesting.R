
library(tidymodels)


#######################################################################################################################################################################################
##### ML Comparison 

# Read in the raw data 
raw_matrix<- read.csv(here("/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/data/LA_Data/Preprocessed_ts_matrices/NatalToMarine_Processed_RAW.csv"))

# Filter out any where the Fish_id contains 2022 
raw_matrix <- raw_matrix[!grepl("2022", raw_matrix$Fish_id), ]




# Choose random indices for training and test split 80/20

#### These same seeds will be used for all datasets, so as to ensure consistency 

set.seed(123)  # For reproducibility
train_indices <- sample(1:nrow(raw_matrix), size = 0.8 * nrow(raw_matrix))
test_indices <- setdiff(1:nrow(raw_matrix), train_indices)

###############
#### RAW 


# split up train/test 
train_data_raw <- raw_matrix[train_indices, ]
test_data_raw <- raw_matrix[test_indices, ]

# pull out train and test metadata , first 8 columns 
train_metadata <- train_data_raw[, 1:8]
test_metadata<- test_data_raw[, 1:8]

# pullout the data matrix, which is the rest of the data 
train_matrix_raw <- train_data_raw[, -c(1:8)]
test_matrix_raw <- test_data_raw[, -c(1:8)]

################
######### RF
###############


## Train Random Forest 
# Prepare data for tidymodels
train_df <- train_data_raw %>%
  mutate(Watershed = as.factor(Watershed))  # Ensure Watershed is a factor
test_df <- test_data_raw %>%
  mutate(Watershed = as.factor(Watershed))  # Ensure Watershed is a factor
# Simple Random Forest model
rf_model <- rand_forest(trees = 500) %>%
  set_engine("ranger") %>%
  set_mode("classification")
# Simple recipe
rf_recipe <- recipe(Watershed ~ ., data = train_df) %>%
  update_role(c(Fish_id, Year, Natal_Iso), new_role = "ID")

# Create and fit workflow
rf_workflow <- workflow() %>%
  add_recipe(rf_recipe) %>%
  add_model(rf_model) %>%
  fit(train_df)

# Make predictions
rf_predictions <- rf_workflow %>%
  predict(test_df) %>%
  bind_cols(test_df %>% select(Watershed))

# Check accuracy
rf_accuracy <- mean(rf_predictions$Watershed == rf_predictions$.pred_class)
cat("Random Forest Test Accuracy:", round(rf_accuracy, 3), "\n")

# Confusion matrix
rf_conf_mat <- conf_mat(rf_predictions, truth = Watershed, estimate = .pred_class)
print(rf_conf_mat)


# All class-specific metrics in one go
class_metrics <- rf_predictions %>%
  precision(truth = Watershed, estimate = .pred_class, estimator = "macro") %>%
  bind_rows(rf_predictions %>% recall(truth = Watershed, estimate = .pred_class, estimator = "macro")) %>%
  bind_rows(rf_predictions %>% f_meas(truth = Watershed, estimate = .pred_class, estimator = "macro"))

print(class_metrics)

# Calculate class-specific accuracy manually
class_specific_accuracy <- rf_predictions %>%
  mutate(correct = .pred_class == Watershed) %>%
  group_by(Watershed) %>%
  summarise(
    n = n(),
    correct = sum(correct),
    accuracy = correct / n
  )

print(class_specific_accuracy)
