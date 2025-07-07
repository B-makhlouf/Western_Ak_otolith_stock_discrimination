# Fixed Combined RF - ensures exact same results
library(tidyverse)
library(tidymodels)
library(here)

# Load exact split and data
split_info <- readRDS(here("data/consistent_split_info.rds"))
combined_data <- read_csv(here("data/preprocessed_matrices/preprocessed_Combined.csv"))

# Apply exact split
train_data <- combined_data %>% filter(Fish_id %in% split_info$train_fish_ids) %>% mutate(Watershed = as.factor(Watershed))
test_data <- combined_data %>% filter(Fish_id %in% split_info$test_fish_ids) %>% mutate(Watershed = as.factor(Watershed))

# Create workflow (don't fit yet)
rf_workflow <- workflow() %>%
  add_recipe(recipe(Watershed ~ ., data = train_data) %>%
               update_role(Fish_id, Year, Natal_Iso, new_role = "ID") %>%
               step_normalize(all_predictors(), -all_nominal(), -has_role("ID"))) %>%
  add_model(rand_forest() %>% set_engine("ranger") %>% set_mode("classification"))

# Set seed RIGHT before fitting (critical for reproducibility)
set.seed(123)
rf_fit <- rf_workflow %>% fit(data = train_data)

# Make predictions
predictions <- predict(rf_fit, test_data) %>%
  bind_cols(test_data %>% select(Fish_id, Watershed)) %>%
  bind_cols(predict(rf_fit, test_data, type = "prob")) %>%
  mutate(correct = Watershed == .pred_class)

# Results
accuracy <- mean(predictions$correct)
confusion_matrix <- table(Actual = predictions$Watershed, Predicted = predictions$.pred_class)

# Display
cat("Combined RF Accuracy:", round(accuracy, 4), "\n")
print(confusion_matrix)


library(caret)

# Remove columns 1, 3, 4 from train_data
train_data <- train_data %>% select(-c(Fish_id, Year, Natal_Iso))


#run rf 
# Run Random Forest with caret
rf_model <- train(Watershed ~ ., data = train_data, method = "rf", trControl = trainControl(method = "none"))

# Make predictions on test data
predictions_caret <- predict(rf_model, newdata = test_data)

# Make sure watershed is a factor and calculate accuracy
test_data$Watershed <- as.factor(test_data$Watershed)

accuracy_caret <- mean(predictions_caret == test_data$Watershed)

conf_matrix <- confusionMatrix(predictions_caret, test_data$Watershed)


##############################################################################################




