library(tidymodels)
library(tidyverse)
library(here)

#######################################################################################################################################################################################
##### ML Comparison Across Multiple Data Types
#######################################################################################################################################################################################

# Function to load and prepare data
load_data <- function(data_type) {
  file_path <- here(paste0("/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/data/LA_Data/Preprocessed_ts_matrices/NatalToMarine_Processed_", data_type, ".csv"))
  
  if (!file.exists(file_path)) {
    warning(paste("File not found:", file_path))
    return(NULL)
  }
  
  data <- read.csv(file_path) %>%
    mutate(Watershed = as.factor(Watershed))
  
  return(data)
}

# Function to train and evaluate Random Forest model
train_evaluate_rf <- function(train_data, test_data, data_type) {
  
  # Simple Random Forest model
  rf_model <- rand_forest(trees = 500) %>%
    set_engine("ranger") %>%
    set_mode("classification")
  
  # Simple recipe
  rf_recipe <- recipe(Watershed ~ ., data = train_data) %>%
    update_role(c(Fish_id, Year, Natal_Iso), new_role = "ID")
  
  # Create and fit workflow
  rf_workflow <- workflow() %>%
    add_recipe(rf_recipe) %>%
    add_model(rf_model) %>%
    fit(train_data)
  
  # Make predictions
  rf_predictions <- rf_workflow %>%
    predict(test_data) %>%
    bind_cols(test_data %>% select(Watershed))
  
  # Calculate overall accuracy
  rf_accuracy <- mean(rf_predictions$Watershed == rf_predictions$.pred_class)
  
  # Confusion matrix
  rf_conf_mat <- conf_mat(rf_predictions, truth = Watershed, estimate = .pred_class)
  
  # Class-specific metrics
  precision_metric <- rf_predictions %>%
    precision(truth = Watershed, estimate = .pred_class, estimator = "macro")
  
  recall_metric <- rf_predictions %>%
    recall(truth = Watershed, estimate = .pred_class, estimator = "macro")
  
  f1_metric <- rf_predictions %>%
    f_meas(truth = Watershed, estimate = .pred_class, estimator = "macro")
  
  # Calculate class-specific accuracy
  class_specific_accuracy <- rf_predictions %>%
    mutate(correct = .pred_class == Watershed) %>%
    group_by(Watershed) %>%
    summarise(
      n = n(),
      correct = sum(correct),
      accuracy = correct / n,
      .groups = "drop"
    )
  
  # Return results
  list(
    data_type = data_type,
    overall_accuracy = rf_accuracy,
    confusion_matrix = rf_conf_mat,
    precision = precision_metric$.estimate,
    recall = recall_metric$.estimate,
    f1_score = f1_metric$.estimate,
    class_accuracy = class_specific_accuracy,
    predictions = rf_predictions,
    model = rf_workflow
  )
}

# Function to train multiple model types
train_multiple_models <- function(train_data, test_data, data_type) {
  
  # Prepare recipe (common for all models)
  base_recipe <- recipe(Watershed ~ ., data = train_data) %>%
    update_role(c(Fish_id, Year, Natal_Iso), new_role = "ID") %>%
    step_normalize(all_predictors(), -all_nominal())
  
  # Define models
  models <- list(
    rf = rand_forest(trees = 500) %>%
      set_engine("ranger") %>%
      set_mode("classification"),
    
    svm = svm_rbf() %>%
      set_engine("kernlab") %>%
      set_mode("classification"),
    
    knn = nearest_neighbor(neighbors = 5) %>%
      set_engine("kknn") %>%
      set_mode("classification")
  )
  
  results <- list()
  
  for (model_name in names(models)) {
    cat(paste("Training", model_name, "for", data_type, "data...\n"))
    
    # Create and fit workflow
    workflow_obj <- workflow() %>%
      add_recipe(base_recipe) %>%
      add_model(models[[model_name]]) %>%
      fit(train_data)
    
    # Make predictions
    predictions <- workflow_obj %>%
      predict(test_data) %>%
      bind_cols(test_data %>% select(Watershed))
    
    # Calculate metrics
    accuracy <- mean(predictions$Watershed == predictions$.pred_class)
    
    # Class-specific accuracy
    class_accuracy <- predictions %>%
      mutate(correct = .pred_class == Watershed) %>%
      group_by(Watershed) %>%
      summarise(
        n = n(),
        correct = sum(correct),
        accuracy = correct / n,
        .groups = "drop"
      )
    
    results[[model_name]] <- list(
      model_type = model_name,
      data_type = data_type,
      overall_accuracy = accuracy,
      class_accuracy = class_accuracy,
      predictions = predictions,
      model = workflow_obj
    )
  }
  
  return(results)
}

#######################################################################################################################################################################################
##### MAIN ANALYSIS
#######################################################################################################################################################################################

# Set seed for reproducibility
set.seed(123)

# Data types to compare
data_types <- c("RAW", "GAM", "MA")

# Load all data types
all_data <- list()
for (data_type in data_types) {
  cat(paste("Loading", data_type, "data...\n"))
  all_data[[data_type]] <- load_data(data_type)
  
  if (is.null(all_data[[data_type]])) {
    cat(paste("Skipping", data_type, "due to loading error\n"))
    next
  }
  
  cat(paste("Loaded", nrow(all_data[[data_type]]), "samples for", data_type, "\n"))
}

# Remove any NULL entries
all_data <- all_data[!sapply(all_data, is.null)]

if (length(all_data) == 0) {
  stop("No data files could be loaded. Please check file paths.")
}

# Use the first available dataset to determine train/test split
reference_data <- all_data[[1]]
train_indices <- sample(1:nrow(reference_data), size = 0.8 * nrow(reference_data))
test_indices <- setdiff(1:nrow(reference_data), train_indices)

cat(paste("Train/test split:", length(train_indices), "training,", length(test_indices), "testing\n"))

#######################################################################################################################################################################################
##### RANDOM FOREST COMPARISON
#######################################################################################################################################################################################

cat("\n=== RANDOM FOREST COMPARISON ===\n")

rf_results <- list()

for (data_type in names(all_data)) {
  cat(paste("\nTraining Random Forest for", data_type, "data...\n"))
  
  # Split data
  train_data <- all_data[[data_type]][train_indices, ]
  test_data <- all_data[[data_type]][test_indices, ]
  
  # Train and evaluate
  rf_results[[data_type]] <- train_evaluate_rf(train_data, test_data, data_type)
  
  # Print results
  cat(paste("Random Forest Test Accuracy for", data_type, ":", round(rf_results[[data_type]]$overall_accuracy, 3), "\n"))
  print(rf_results[[data_type]]$class_accuracy)
}

#######################################################################################################################################################################################
##### MULTIPLE MODEL COMPARISON
#######################################################################################################################################################################################

cat("\n=== MULTIPLE MODEL COMPARISON ===\n")

all_model_results <- list()

for (data_type in names(all_data)) {
  cat(paste("\nTraining multiple models for", data_type, "data...\n"))
  
  # Split data
  train_data <- all_data[[data_type]][train_indices, ]
  test_data <- all_data[[data_type]][test_indices, ]
  
  # Train multiple models
  all_model_results[[data_type]] <- train_multiple_models(train_data, test_data, data_type)
  
  # Print results summary
  for (model_name in names(all_model_results[[data_type]])) {
    accuracy <- all_model_results[[data_type]][[model_name]]$overall_accuracy
    cat(paste(model_name, "accuracy for", data_type, ":", round(accuracy, 3), "\n"))
  }
}

#######################################################################################################################################################################################
##### RESULTS SUMMARY
#######################################################################################################################################################################################

cat("\n=== RESULTS SUMMARY ===\n")

# Create summary table
summary_results <- data.frame()

for (data_type in names(all_model_results)) {
  for (model_name in names(all_model_results[[data_type]])) {
    result <- all_model_results[[data_type]][[model_name]]
    
    summary_results <- rbind(summary_results, data.frame(
      Data_Type = data_type,
      Model = model_name,
      Overall_Accuracy = result$overall_accuracy,
      stringsAsFactors = FALSE
    ))
  }
}

# Print summary table
print(summary_results)

# Find best performing combination
best_combination <- summary_results[which.max(summary_results$Overall_Accuracy), ]
cat(paste("\nBest performing combination:", 
          best_combination$Data_Type, "+", best_combination$Model, 
          "with accuracy:", round(best_combination$Overall_Accuracy, 3), "\n"))

