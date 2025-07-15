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

# Updated data types to compare - all requested types
data_types <- c("RAW", "GAM", "MA", "Sr88", "Combined")

# Create descriptive names for results
data_type_names <- list(
  "RAW" = "Sr87/86 Raw",
  "GAM" = "Sr87/86 GAM",
  "MA" = "Sr87/86 Moving Average", 
  "Sr88" = "Sr88 Corrected",
  "Combined" = "Combined (Sr87/86 Raw + Sr88)"
)

# Load all data types
all_data <- list()
for (data_type in data_types) {
  cat(paste("Loading", data_type_names[[data_type]], "data...\n"))
  all_data[[data_type]] <- load_data(data_type)
  
  if (is.null(all_data[[data_type]])) {
    cat(paste("Skipping", data_type, "due to loading error\n"))
    next
  }
  
  cat(paste("Loaded", nrow(all_data[[data_type]]), "samples for", data_type_names[[data_type]], "\n"))
  
  # Check for any rows with all NA predictors
  predictor_cols <- all_data[[data_type]] %>% 
    select(-Fish_id, -Watershed, -Year, -Natal_Iso, -starts_with("Natal"), -starts_with("Marine"), -Original_Data_Points, -Interpolated_Points)
  
  rows_with_all_na <- rowSums(is.na(predictor_cols)) == ncol(predictor_cols)
  if (any(rows_with_all_na)) {
    cat(paste("Warning: Found", sum(rows_with_all_na), "rows with all NA predictors in", data_type, "- removing them\n"))
    all_data[[data_type]] <- all_data[[data_type]][!rows_with_all_na, ]
  }
}

# Remove any NULL entries
all_data <- all_data[!sapply(all_data, is.null)]

if (length(all_data) == 0) {
  stop("No data files could be loaded. Please check file paths.")
}

# Check that all datasets have the same Fish_id values for consistent splitting
fish_ids <- lapply(all_data, function(x) x$Fish_id)
common_fish_ids <- Reduce(intersect, fish_ids)

if (length(common_fish_ids) < length(fish_ids[[1]])) {
  cat("Warning: Not all datasets have the same fish IDs. Using common fish IDs for consistent splitting.\n")
  cat(paste("Common fish IDs:", length(common_fish_ids), "\n"))
  
  # Filter all datasets to common fish IDs
  for (data_type in names(all_data)) {
    all_data[[data_type]] <- all_data[[data_type]] %>%
      filter(Fish_id %in% common_fish_ids)
  }
}

# Use the first available dataset to determine train/test split based on Fish_id
reference_data <- all_data[[1]]
unique_fish_ids <- unique(reference_data$Fish_id)
train_fish_ids <- sample(unique_fish_ids, size = 0.8 * length(unique_fish_ids))
test_fish_ids <- setdiff(unique_fish_ids, train_fish_ids)

cat(paste("Train/test split:", length(train_fish_ids), "training fish,", length(test_fish_ids), "testing fish\n"))


#######################################################################################################################################################################################
##### MULTIPLE MODEL COMPARISON
#######################################################################################################################################################################################

cat("\n=== MULTIPLE MODEL COMPARISON ===\n")

all_model_results <- list()

for (data_type in names(all_data)) {
  cat(paste("\nTraining multiple models for", data_type_names[[data_type]], "...\n"))
  
  # Split data by Fish_id
  train_data <- all_data[[data_type]][all_data[[data_type]]$Fish_id %in% train_fish_ids, ]
  test_data <- all_data[[data_type]][all_data[[data_type]]$Fish_id %in% test_fish_ids, ]
  
  # Train multiple models
  all_model_results[[data_type]] <- train_multiple_models(train_data, test_data, data_type_names[[data_type]])
  
  # Print results summary
  for (model_name in names(all_model_results[[data_type]])) {
    accuracy <- all_model_results[[data_type]][[model_name]]$overall_accuracy
    cat(paste(model_name, "accuracy for", data_type_names[[data_type]], ":", round(accuracy, 3), "\n"))
  }
}

#######################################################################################################################################################################################
##### RESULTS SUMMARY
#######################################################################################################################################################################################

cat("\n=== RESULTS SUMMARY ===\n")

# Create comprehensive summary table
summary_results <- data.frame()

for (data_type in names(all_model_results)) {
  for (model_name in names(all_model_results[[data_type]])) {
    result <- all_model_results[[data_type]][[model_name]]
    
    summary_results <- rbind(summary_results, data.frame(
      Data_Type = data_type_names[[data_type]],
      Model = model_name,
      Overall_Accuracy = result$overall_accuracy,
      stringsAsFactors = FALSE
    ))
  }
}

# Sort by accuracy (descending)
summary_results <- summary_results[order(-summary_results$Overall_Accuracy), ]

# Print summary table
cat("\nPerformance Summary (sorted by accuracy):\n")
print(summary_results, row.names = FALSE)

# Find best performing combination
best_combination <- summary_results[1, ]
cat(paste("\nBest performing combination:", 
          best_combination$Data_Type, "+", best_combination$Model, 
          "with accuracy:", round(best_combination$Overall_Accuracy, 3), "\n"))

# Create a performance comparison by data type
cat("\n=== PERFORMANCE BY DATA TYPE ===\n")
data_type_summary <- summary_results %>%
  group_by(Data_Type) %>%
  summarise(
    Best_Model = Model[which.max(Overall_Accuracy)],
    Best_Accuracy = max(Overall_Accuracy),
    Mean_Accuracy = mean(Overall_Accuracy),
    .groups = "drop"
  ) %>%
  arrange(-Best_Accuracy)

print(data_type_summary, row.names = FALSE)

# Create a performance comparison by model type
cat("\n=== PERFORMANCE BY MODEL TYPE ===\n")
model_type_summary <- summary_results %>%
  group_by(Model) %>%
  summarise(
    Best_Data_Type = Data_Type[which.max(Overall_Accuracy)],
    Best_Accuracy = max(Overall_Accuracy),
    Mean_Accuracy = mean(Overall_Accuracy),
    .groups = "drop"
  ) %>%
  arrange(-Best_Accuracy)

print(model_type_summary, row.names = FALSE)

#######################################################################################################################################################################################
##### DETAILED CLASS-SPECIFIC PERFORMANCE
#######################################################################################################################################################################################

cat("\n=== CLASS-SPECIFIC PERFORMANCE FOR BEST MODEL ===\n")

best_data_type <- names(data_type_names)[data_type_names == best_combination$Data_Type]
best_model_name <- best_combination$Model

if (best_data_type %in% names(all_model_results) && 
    best_model_name %in% names(all_model_results[[best_data_type]])) {
  
  best_class_accuracy <- all_model_results[[best_data_type]][[best_model_name]]$class_accuracy
  
  cat(paste("Class-specific accuracy for", best_combination$Data_Type, "+", best_combination$Model, ":\n"))
  print(best_class_accuracy, row.names = FALSE)
  
  # Calculate additional metrics
  best_predictions <- all_model_results[[best_data_type]][[best_model_name]]$predictions
  
  cat("\nConfusion Matrix:\n")
  conf_matrix <- table(Predicted = best_predictions$.pred_class, Actual = best_predictions$Watershed)
  print(conf_matrix)
  
  # Calculate precision, recall, F1 for each class
  cat("\nPer-class metrics:\n")
  class_metrics <- best_predictions %>%
    group_by(Watershed) %>%
    summarise(
      n_actual = n(),
      n_predicted = sum(best_predictions$.pred_class == Watershed),
      true_positive = sum(.pred_class == Watershed & Watershed == Watershed),
      .groups = "drop"
    ) %>%
    mutate(
      precision = true_positive / pmax(n_predicted, 1),
      recall = true_positive / n_actual,
      f1_score = 2 * (precision * recall) / pmax(precision + recall, 1e-10)
    )
  
  print(class_metrics, row.names = FALSE)
}