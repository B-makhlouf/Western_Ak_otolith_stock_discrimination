######### Model_Probability_Calibration.R #########
# Calibrates model probabilities to ensure reliable probability estimates
# Provides pre/post calibration visualization and performance metrics
# Saves calibrated models for deployment

# Load necessary libraries
library(tidyverse)      # For data manipulation
library(tidymodels)     # For modeling framework
library(probably)       # For probability calibration
library(here)           # For file path handling
library(ggplot2)        # For visualization
library(gridExtra)      # For arranging multiple plots
library(viridis)        # For color scales

# Create necessary directories
dirs <- c("Data/Calibrated_Models", "Figures/Calibration", "Data/Calibration_Results")
for (dir in dirs) {
  if (!dir.exists(here(dir))) {
    dir.create(here(dir), recursive = TRUE)
  }
}

#############################################################
# Configuration - Set your parameters here
#############################################################

# Model configuration
# Edit these parameters to match your desired model setup
config <- list(
  data_type = "GAM",            # Options: "RAW", "MA", "GAM", "Sr88", "Combined"
  model_method = "rf",          # Options: "rf", "svmRadial", "knn"
  landmark_filter = c("Core", "Fw"),  # Combination of landmarks to use
  calibration = TRUE,           # Whether to perform probability calibration
  use_saved_datasets = TRUE     # Whether to use datasets from MultiModel comparison
)

# Set random seed for reproducibility
set.seed(123)

#############################################################
# Data Loading and Preparation
#############################################################

# Function to load processed data with appropriate path
load_processed_data <- function(data_type, landmark_filter) {
  # Create filename
  landmark_str <- paste(landmark_filter, collapse = "_")
  
  # Handle different data types with appropriate filenames
  if (data_type == "Combined") {
    filename <- paste0("Processed_", landmark_str, "_Combined.csv")
  } else if (data_type == "Sr88") {
    filename <- paste0("Processed_", landmark_str, "_Sr88.csv")
  } else {
    filename <- paste0("Processed_", landmark_str, "_", data_type, ".csv")
  }
  
  # Check in multiple possible locations (ordered by preference)
  possible_paths <- c(
    # MultiModel_Comparison standard paths
    here("Data/Classification_ts_matrices", paste0(data_type, "/Processed_", landmark_str, "_", data_type, ".csv")),
    here("Data/Classification_ts_matrices/Sr8786", paste0("Processed_", landmark_str, "_", data_type, ".csv")),
    
    # Other common paths in the project
    here("Data/Preprocessed_ts_matrices", filename),
    here("Data/02_Preprocessed_ts_matrices", filename),
    here("Data/Processed/Preprocessed_ts_matrices", filename)
  )
  
  # Special handling for Combined and Sr88 data types
  if (data_type == "Combined") {
    possible_paths <- c(
      here("Data/Classification_ts_matrices/Sr88", paste0("Processed_", landmark_str, "_Sr88_Iso.csv")),
      possible_paths
    )
  }
  
  if (data_type == "Sr88") {
    possible_paths <- c(
      here("Data/Classification_ts_matrices/Sr88", paste0("Processed_", landmark_str, "_Sr88.csv")),
      possible_paths
    )
  }
  
  # Try each path
  for (path in possible_paths) {
    if (file.exists(path)) {
      message("Loading data from: ", path)
      return(read.csv(path))
    }
  }
  
  # If no files found
  stop(paste("File not found for", data_type, "data with", 
             paste(landmark_filter, collapse=","), "landmarks.",
             "Tried paths:", paste(possible_paths, collapse=", ")))
}

# Load metadata
message("Loading metadata...")
All_Metadata <- read.csv(here("Data/Final/Metadata_and_QC.csv"))

# Load the processed data based on config
message(paste("Loading", config$data_type, "data with", 
              paste(config$landmark_filter, collapse=","), "landmarks..."))
processed_data <- load_processed_data(config$data_type, config$landmark_filter)

# Merge with metadata and filter for quality control
message("Merging with metadata and applying QC filter...")
AnalysisDataAll <- processed_data %>%
  left_join(All_Metadata %>% select(-Year), by = c("Fish_id" = "Fish_ID")) %>%
  select((ncol(.)-12):ncol(.), everything()) %>%
  filter(QC_Grade == "Yes")

# Separate metadata and time series data
Analysis_metadata <- AnalysisDataAll[, 1:17]
Analysis_ts_data <- AnalysisDataAll[, 18:ncol(AnalysisDataAll)]

# Prepare model data
message("Preparing model data...")
ModelData <- Analysis_ts_data %>%
  as.data.frame() %>%
  mutate(Watershed = Analysis_metadata$Watershed)

# Ensure column names are valid for modeling (no names starting with numbers)
if (any(grepl("^[0-9]", names(ModelData)))) {
  message("Renaming numeric columns for compatibility...")
  colnames(ModelData)[!colnames(ModelData) %in% c("Watershed")] <- 
    paste0("X", 1:(ncol(ModelData)-1))
}

# Ensure Watershed is a factor
ModelData$Watershed <- as.factor(ModelData$Watershed)

#############################################################
# Load Existing Train/Test Data or Create New Split
#############################################################

# Create model identifier
model_id <- paste(config$data_type, config$model_method, 
                  paste(config$landmark_filter, collapse="_"), sep="_")

# Check for the specific datasets from MultiModel_Comparison script
message("Checking for saved datasets from MultiModel comparison...")

# Try different potential locations for the saved datasets
possible_dataset_paths <- c(
  # Model-specific datasets
  here(paste0("Data/Train_Test_Sets/", model_id, "_datasets.rds")),
  
  # Generic datasets by data_type
  here(paste0("Data/Train_Test_Sets/", config$data_type, "_", 
              paste(config$landmark_filter, collapse="_"), "_datasets.rds")),
  
  # Check in Model Results folder
  here(paste0("Data/Model_Results/", model_id, "_datasets.rds")),
  
  # A common naming convention without model method
  here(paste0("Data/Train_Test_Sets/", config$data_type, "_", 
              config$model_method, "_datasets.rds")),
  
  # Try the Models folder
  here(paste0("Models/", config$model_method, "_models/", model_id, "_datasets.rds"))
)

datasets_found <- FALSE

if (config$use_saved_datasets) {
  # Try each potential path
  for (datasets_path in possible_dataset_paths) {
    if (file.exists(datasets_path)) {
      # Load existing split
      message("Loading existing train/test split from: ", datasets_path)
      train_test_data <- readRDS(datasets_path)
      
      # Extract train and test data - handle different possible structures
      if ("train" %in% names(train_test_data)) {
        train_data <- train_test_data$train
        datasets_found <- TRUE
      } else if ("traindata" %in% names(train_test_data)) {
        train_data <- train_test_data$traindata
        datasets_found <- TRUE
      }
      
      if ("test" %in% names(train_test_data)) {
        test_data <- train_test_data$test
      } else if ("testdata" %in% names(train_test_data)) {
        test_data <- train_test_data$testdata
      }
      
      if ("test_metadata" %in% names(train_test_data)) {
        test_metadata <- train_test_data$test_metadata
      } else if ("testmetadata" %in% names(train_test_data)) {
        test_metadata <- train_test_data$testmetadata
      } else {
        # If no test metadata found, try to extract it from the original Analysis_metadata
        message("Test metadata not found in saved datasets, attempting to reconstruct...")
        # This is a bit hacky but tries to match the original test data creation
        test_indices <- match(row.names(test_data), row.names(ModelData))
        if (length(test_indices) > 0 && !all(is.na(test_indices))) {
          test_metadata <- Analysis_metadata[test_indices, ]
        }
      }
      
      if (datasets_found) {
        message("Successfully loaded train/test split from previous analysis.")
        break
      }
    }
  }
}

# If no datasets found or not using saved datasets, create new split
if (!datasets_found || !config$use_saved_datasets) {
  message("Creating new train/test split...")
  set.seed(123)  # For reproducibility
  train_index <- createDataPartition(ModelData$Watershed, p = 0.8, list = FALSE)
  train_data <- ModelData[train_index, ]
  test_data <- ModelData[-train_index, ]
  test_metadata <- Analysis_metadata[-train_index, ]
  
  # Save the new split for future use
  train_test_data <- list(
    train = train_data, 
    test = test_data,
    test_metadata = test_metadata,
    train_idx = train_index
  )
  
  # Create directory if it doesn't exist
  if (!dir.exists(here("Data/Train_Test_Sets"))) {
    dir.create(here("Data/Train_Test_Sets"), recursive = TRUE)
  }
  
  saveRDS(
    train_test_data,
    here(paste0("Data/Train_Test_Sets/", model_id, "_datasets.rds"))
  )
  
  message("New train/test split created and saved for future use.")
}

# Print dataset summary
message("Dataset summary:")
message("Training set: ", nrow(train_data), " samples")
message("Testing set: ", nrow(test_data), " samples")
message("Class distribution in training set:")
print(table(train_data$Watershed))
message("Class distribution in testing set:")
print(table(test_data$Watershed))

# Set up cross-validation
message("Setting up cross-validation...")
cv_folds <- vfold_cv(train_data, v = 5, strata = Watershed)

# Create recipe
message("Creating preprocessing recipe...")
model_recipe <- recipe(Watershed ~ ., data = train_data) %>%
  step_normalize(all_numeric_predictors())

# Define model
message(paste("Setting up", config$model_method, "model..."))
if (config$model_method == "rf") {
  model_spec <- rand_forest(trees = 500) %>%
    set_engine("ranger", importance = "impurity") %>%
    set_mode("classification")
} else if (config$model_method == "svmRadial") {
  model_spec <- svm_rbf() %>%
    set_engine("kernlab") %>%
    set_mode("classification")
} else if (config$model_method == "knn") {
  model_spec <- nearest_neighbor(neighbors = 5) %>%
    set_engine("kknn") %>%
    set_mode("classification")
} else {
  stop("Unsupported model method specified")
}

# Create workflow
model_wf <- workflow() %>%
  add_recipe(model_recipe) %>%
  add_model(model_spec)

# Train model
message("Training model...")
final_model <- model_wf %>%
  fit(train_data)

# Save uncalibrated model
message("Saving uncalibrated model...")
saveRDS(final_model, here(paste0("Data/Calibrated_Models/", model_id, "_uncalibrated.rds")))

#############################################################
# Model Evaluation and Calibration
#############################################################

# Get predictions on test set
message("Generating predictions on test data...")
test_predictions <- predict(final_model, test_data, type = "class")
test_probs <- predict(final_model, test_data, type = "prob")

# Create results data frame
results_df <- bind_cols(
  test_metadata %>% select(Fish_id),
  tibble(
    Predicted = test_predictions$.pred_class,
    Actual = factor(test_data$Watershed)  # Ensure Actual is a factor
  ),
  test_probs
)

# Ensure Predicted has the same levels as Actual
results_df$Predicted <- factor(results_df$Predicted, levels = levels(results_df$Actual))

# Confusion matrix and metrics
message("Computing performance metrics...")
# Ensure both truth and estimate are factors with the same levels
results_df <- results_df %>%
  mutate(
    Actual = factor(Actual),
    Predicted = factor(Predicted, levels = levels(Actual))
  )

conf_matrix <- conf_mat(results_df, truth = Actual, estimate = Predicted)
accuracy <- accuracy_vec(results_df$Actual, results_df$Predicted)
message(paste("Uncalibrated model accuracy:", round(accuracy, 4)))

# Calculate class-specific metrics
class_metrics <- data.frame()
for (ws in levels(results_df$Actual)) {
  # Create binary classification: target class vs all others
  binary_data <- tibble(
    truth = factor(ifelse(results_df$Actual == ws, "Yes", "No"), levels = c("Yes", "No")),
    estimate = factor(ifelse(results_df$Predicted == ws, "Yes", "No"), levels = c("Yes", "No"))
  )
  
  # Compute binary classification metrics
  binary_conf <- conf_mat(binary_data, truth = truth, estimate = estimate)
  sens <- sens_vec(binary_data$truth, binary_data$estimate)
  spec <- spec_vec(binary_data$truth, binary_data$estimate)
  precision <- precision_vec(binary_data$truth, binary_data$estimate)
  f1 <- f_meas_vec(binary_data$truth, binary_data$estimate)
  
  # Add to metrics dataframe
  class_metrics <- rbind(class_metrics, data.frame(
    Watershed = ws,
    Sensitivity = sens,
    Specificity = spec,
    Precision = precision,
    F1_Score = f1
  ))
}

# Save uncalibrated metrics
write.csv(
  class_metrics,
  here(paste0("Data/Calibration_Results/", model_id, "_uncalibrated_metrics.csv")),
  row.names = FALSE
)

#############################################################
# Probability Calibration
#############################################################

if (config$calibration) {
  message("Performing probability calibration...")
  
  # Get the probability columns
  prob_cols <- results_df %>% select(starts_with(".pred_"))
  
  # Create a list to store all calibration plots
  cal_plots_before <- list()
  cal_plots_after <- list()
  cal_estimators <- list()
  
  # For each watershed class
  for (ws in levels(results_df$Actual)) {
    message(paste("Calibrating probabilities for", ws, "..."))
    # Extract column name for this watershed
    ws_col <- paste0(".pred_", ws)
    
    # Create binary dataset
    binary_df <- tibble(
      actual = results_df$Actual == ws,
      prob = results_df[[ws_col]]
    )
    
    # Create calibration plot before
    cal_plots_before[[ws]] <- cal_plot_windowed(
      binary_df, 
      truth = actual, 
      estimate = prob,
      window_size = 0.2,
      step_size = 0.05
    ) +
      ggtitle(paste("Before Calibration:", ws)) +
      theme_minimal() +
      coord_equal() +
      theme(plot.title = element_text(size = 10))
    
    # Calibrate probabilities
    cal_estimators[[ws]] <- cal_estimate_logistic(
      binary_df,
      truth = actual,
      estimate = prob
    )
    
    # Apply calibration
    cal_probs <- cal_apply(binary_df, cal_estimators[[ws]])
    
    # Create calibration plot after
    cal_plots_after[[ws]] <- cal_plot_windowed(
      cal_probs,
      truth = actual,
      estimate = cal_prob,
      window_size = 0.2,
      step_size = 0.05
    ) +
      ggtitle(paste("After Calibration:", ws)) +
      theme_minimal() +
      coord_equal() +
      theme(plot.title = element_text(size = 10))
  }
  
  # Create combined plots for before and after calibration
  before_grid <- grid.arrange(
    grobs = cal_plots_before,
    ncol = length(cal_plots_before),
    top = "Before Calibration"
  )
  
  after_grid <- grid.arrange(
    grobs = cal_plots_after,
    ncol = length(cal_plots_after),
    top = "After Calibration"
  )
  
  # Save the plots
  ggsave(
    here(paste0("Figures/Calibration/", model_id, "_before_calibration.png")),
    before_grid,
    width = min(10, 3 * length(cal_plots_before)),
    height = 4,
    dpi = 300
  )
  
  ggsave(
    here(paste0("Figures/Calibration/", model_id, "_after_calibration.png")),
    after_grid,
    width = min(10, 3 * length(cal_plots_after)),
    height = 4,
    dpi = 300
  )
  
  # Create a stacked comparison of before and after
  comparison_grid <- grid.arrange(before_grid, after_grid, nrow = 2)
  
  ggsave(
    here(paste0("Figures/Calibration/", model_id, "_calibration_comparison.png")),
    comparison_grid,
    width = min(12, 3 * length(cal_plots_before)),
    height = 8,
    dpi = 300
  )
  
  #############################################################
  # Apply Calibration to Test Set and Evaluate
  #############################################################
  
  message("Applying calibration to test set...")
  
  # Create a function to apply calibration to new data
  apply_calibration <- function(probs, estimators) {
    calibrated_probs <- probs
    
    # Apply calibration to each class
    for (ws in names(estimators)) {
      ws_col <- paste0(".pred_", ws)
      ws_data <- tibble(prob = probs[[ws_col]])
      cal_result <- cal_apply(ws_data, estimators[[ws]])
      calibrated_probs[[ws_col]] <- cal_result$cal_prob
    }
    
    # Normalize probabilities to sum to 1
    prob_cols <- names(probs)
    calibrated_probs <- calibrated_probs %>%
      mutate(total = rowSums(across(all_of(prob_cols)))) %>%
      mutate(across(all_of(prob_cols), ~ . / total)) %>%
      select(-total)
    
    return(calibrated_probs)
  }
  
  # Apply calibration
  calibrated_probs <- apply_calibration(prob_cols, cal_estimators)
  
  # Create new predictions based on calibrated probabilities
  cal_predictions <- apply(calibrated_probs, 1, function(row) {
    levels(results_df$Actual)[which.max(row)]
  })
  
  # Create calibrated results
  cal_results_df <- results_df %>%
    select(Fish_id, Actual) %>%
    mutate(
      Predicted = factor(cal_predictions, levels = levels(results_df$Actual)),
      Correct = Predicted == Actual
    ) %>%
    bind_cols(calibrated_probs)
  
  # Compute metrics for calibrated model
  cal_conf_matrix <- conf_mat(cal_results_df, truth = Actual, estimate = Predicted)
  cal_accuracy <- accuracy_vec(cal_results_df$Actual, cal_results_df$Predicted)
  message(paste("Calibrated model accuracy:", round(cal_accuracy, 4)))
  
  # Calculate class-specific metrics for calibrated model
  cal_class_metrics <- data.frame()
  for (ws in levels(cal_results_df$Actual)) {
    binary_actual <- ifelse(cal_results_df$Actual == ws, "Yes", "No")
    binary_pred <- ifelse(cal_results_df$Predicted == ws, "Yes", "No")
    
    binary_conf <- conf_mat(factor(binary_pred), factor(binary_actual))
    sens <- sens_vec(factor(binary_actual), factor(binary_pred))
    spec <- spec_vec(factor(binary_actual), factor(binary_pred))
    precision <- precision_vec(factor(binary_actual), factor(binary_pred))
    f1 <- f_meas_vec(factor(binary_actual), factor(binary_pred))
    
    cal_class_metrics <- rbind(cal_class_metrics, data.frame(
      Watershed = ws,
      Sensitivity = sens,
      Specificity = spec,
      Precision = precision,
      F1_Score = f1
    ))
  }
  
  # Save calibrated metrics
  write.csv(
    cal_class_metrics,
    here(paste0("Data/Calibration_Results/", model_id, "_calibrated_metrics.csv")),
    row.names = FALSE
  )
  
  # Compare metrics before and after calibration
  metrics_comparison <- class_metrics %>%
    left_join(cal_class_metrics, by = "Watershed", suffix = c("_Before", "_After")) %>%
    mutate(
      Sensitivity_Change = Sensitivity_After - Sensitivity_Before,
      Specificity_Change = Specificity_After - Specificity_Before,
      Precision_Change = Precision_After - Precision_Before,
      F1_Score_Change = F1_Score_After - F1_Score_Before
    )
  
  # Save comparison
  write.csv(
    metrics_comparison,
    here(paste0("Data/Calibration_Results/", model_id, "_metrics_comparison.csv")),
    row.names = FALSE
  )
  
  # Create performance comparison visualization
  perf_change <- metrics_comparison %>%
    select(Watershed, ends_with("Change")) %>%
    pivot_longer(
      cols = ends_with("Change"),
      names_to = "Metric",
      values_to = "Change"
    ) %>%
    mutate(
      Metric = gsub("_Change", "", Metric),
      Direction = ifelse(Change >= 0, "Improved", "Decreased")
    )
  
  perf_plot <- ggplot(perf_change, aes(x = Metric, y = Change, fill = Direction)) +
    geom_bar(stat = "identity", position = "identity") +
    facet_wrap(~Watershed) +
    scale_fill_manual(values = c("Improved" = "#4CAF50", "Decreased" = "#F44336")) +
    geom_text(aes(label = sprintf("%+.3f", Change)), vjust = -0.5) +
    labs(
      title = "Performance Changes After Calibration",
      subtitle = paste(config$data_type, config$model_method, paste(config$landmark_filter, collapse="_")),
      x = NULL,
      y = "Change in Performance Metric"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    )
  
  # Save performance plot
  ggsave(
    here(paste0("Figures/Calibration/", model_id, "_performance_changes.png")),
    perf_plot,
    width = 10,
    height = 6,
    dpi = 300
  )
  
  #############################################################
  # Save Calibrated Model
  #############################################################
  
  message("Creating and saving calibrated model object...")
  
  # Create a calibrated model object that includes the original model and calibration estimators
  calibrated_model <- list(
    original_model = final_model,
    cal_estimators = cal_estimators,
    config = config,
    uncalibrated_metrics = class_metrics,
    calibrated_metrics = cal_class_metrics,
    
    # Function to make predictions with the calibrated model
    predict = function(new_data, type = "class") {
      # Get raw probabilities from original model
      raw_probs <- predict(final_model, new_data, type = "prob")
      
      # Apply calibration
      cal_probs <- apply_calibration(raw_probs, cal_estimators)
      
      # If class predictions requested
      if (type == "class") {
        # Get class with highest probability
        predictions <- apply(cal_probs, 1, function(row) {
          levels(train_data$Watershed)[which.max(row)]
        })
        return(factor(predictions, levels = levels(train_data$Watershed)))
      } 
      # If probability predictions requested
      else if (type == "prob") {
        return(cal_probs)
      }
      else {
        stop("Invalid prediction type. Use 'class' or 'prob'.")
      }
    }
  )
  
  # Save calibrated model
  saveRDS(
    calibrated_model,
    here(paste0("Data/Calibrated_Models/", model_id, "_calibrated.rds"))
  )
  
  message("Calibrated model saved successfully.")
} else {
  message("Skipping calibration as specified in config.")
}

message("Model training and evaluation complete!")