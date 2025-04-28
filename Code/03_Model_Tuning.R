################################################################################
# 03_Model_Tuning.R
# 
# This script performs probability calibration and model tuning for the
# best-performing models identified in the model comparison step.
# 
# It:
# 1. Loads the best-performing model(s) based on balanced accuracy
# 2. Calibrates model probabilities to reflect true probabilities
# 3. Evaluates calibration performance
# 4. Saves calibrated models
#
# OUTPUTS:
# - Calibrated models saved to Models/Calibrated/[model_name]_calibrated.rds
# - Calibration performance plots saved to Figures/Calibration/
# - Calibration metrics saved to Model_Results/Calibration/
#
################################################################################

# Load necessary libraries
library(tidyverse)    # For data manipulation
library(here)         # For file path management
library(caret)        # For model training
library(probably)     # For probability calibration
library(glue)         # For string interpolation
library(logger)       # For logging
library(pROC)         # For ROC curves
library(ggplot2)      # For plotting
library(cowplot)      # For combining plots

# Create directories for outputs if they don't exist
dirs <- c(
  here("Models/Calibrated"),
  here("Figures/Calibration"),
  here("Model_Results/Calibration")
)

for(dir in dirs) {
  if(!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
    logger::log_info("Created directory: {dir}")
  }
}

################################################################################
# 1. Find the best-performing models from the comparison results
################################################################################

logger::log_info("Identifying best models from comparison results")

# Load the summary results from the comparison step
summary_file <- here("Model_Results/Model_Comparison_Summary.csv")

if(!file.exists(summary_file)) {
  stop("Model comparison summary file not found. Run 02_MultiModel_Comparison.R first.")
}

model_summary <- read.csv(summary_file)

# Get the top model configuration for each watershed
top_models <- model_summary %>%
  group_by(Data_Type, Model_Method) %>%
  top_n(1, Mean_Balanced_Accuracy) %>%
  arrange(desc(Mean_Balanced_Accuracy))

# Display the top models
print(top_models)

################################################################################
# Function to calibrate a single model
################################################################################

calibrate_model <- function(data_type, landmark_str, model_method) {
  
  logger::log_info("Calibrating {model_method} model using {data_type} data with landmarks: {landmark_str}")
  
  # Load the saved train/test data
  train_test_file <- glue("Data/Train_Test_Sets/{data_type}_{landmark_str}_train_test_data.rds")
  
  if(!file.exists(train_test_file)) {
    logger::log_error("Train/test data file not found: {train_test_file}")
    return(NULL)
  }
  
  train_test_data <- readRDS(train_test_file)
  
  traindata <- train_test_data$train
  testdata <- train_test_data$test
  test_metadata <- train_test_data$test_metadata
  
  # Load the trained model
  # Path differs based on model method
  model_dir <- switch(model_method,
                      "rf" = "rf_models",
                      "svmRadial" = "svm_models",
                      "knn" = "knn_models",
                      "rf_models") # Default to rf_models
  
  model_file <- glue("Models/{model_dir}/{model_method}_{data_type}_{landmark_str}.rds")
  
  if(!file.exists(model_file)) {
    logger::log_error("Model file not found: {model_file}")
    return(NULL)
  }
  
  model <- readRDS(model_file)
  
  # Get raw predictions on test data
  raw_pred <- predict(model, testdata, type = "prob")
  raw_pred <- as.data.frame(raw_pred)
  
  # Combine predictions with actual classes for calibration
  pred_df <- bind_cols(
    raw_pred, 
    actual = testdata$Watershed
  )
  
  # Create calibration results for each watershed
  watersheds <- unique(testdata$Watershed)
  
  # Lists to store calibration results
  cal_models <- list()
  cal_plots <- list()
  cal_metrics <- list()
  
  for(ws in watersheds) {
    logger::log_info("Calibrating probabilities for {ws}")
    
    # Create binary classification problem for this watershed
    binary_df <- pred_df %>%
      mutate(
        binary_actual = ifelse(actual == ws, "Yes", "No"),
        binary_actual = factor(binary_actual, levels = c("Yes", "No")),
        prob = raw_pred[[ws]]
      )
    
    # Plot original calibration curve
    before_cal <- binary_df %>%
      cal_plot_windowed(
        truth = binary_actual,
        estimate = prob,
        window_size = 0.1,
        step_size = 0.025
      ) +
      ggtitle(glue("Before Calibration: {ws}")) +
      theme_minimal()
    
    # Estimate calibration curve
    cal_model <- cal_estimate_logistic(
      binary_df,
      truth = binary_actual,
      estimate = prob
    )
    
    # Apply calibration
    calibrated_df <- cal_apply(binary_df, cal_model)
    
    # Plot calibrated curve
    after_cal <- calibrated_df %>%
      cal_plot_windowed(
        truth = binary_actual,
        estimate = .cal_estimate,
        window_size = 0.1,
        step_size = 0.025
      ) +
      ggtitle(glue("After Calibration: {ws}")) +
      theme_minimal()
    
    # Compare calibration metrics
    # Calculate AUC before calibration
    roc_before <- roc(binary_df$binary_actual == "Yes", binary_df$prob)
    auc_before <- auc(roc_before)
    
    # Calculate AUC after calibration
    roc_after <- roc(calibrated_df$binary_actual == "Yes", calibrated_df$.cal_estimate)
    auc_after <- auc(roc_after)
    
    # Calculate other metrics like Brier score
    brier_before <- mean((as.numeric(binary_df$binary_actual == "Yes") - binary_df$prob)^2)
    brier_after <- mean((as.numeric(calibrated_df$binary_actual == "Yes") - calibrated_df$.cal_estimate)^2)
    
    # Store results
    cal_models[[ws]] <- cal_model
    cal_plots[[ws]] <- plot_grid(before_cal, after_cal, ncol = 2)
    
    cal_metrics[[ws]] <- data.frame(
      Watershed = ws,
      AUC_Before = auc_before,
      AUC_After = auc_after,
      Brier_Before = brier_before,
      Brier_After = brier_after,
      Brier_Improvement = (brier_before - brier_after)/brier_before * 100
    )
  }
  
  # Combine all calibration metrics
  all_metrics <- bind_rows(cal_metrics)
  
  # Save the combined calibration plot
  combined_plot <- plot_grid(plotlist = cal_plots, ncol = 1)
  plot_file <- glue("Figures/Calibration/{model_method}_{data_type}_{landmark_str}_calibration.png")
  ggsave(plot_file, combined_plot, width = 12, height = 4 * length(watersheds), dpi = 300)
  logger::log_info("Saved calibration plots to {plot_file}")
  
  # Save the calibration metrics
  metrics_file <- glue("Model_Results/Calibration/{model_method}_{data_type}_{landmark_str}_cal_metrics.csv")
  write.csv(all_metrics, metrics_file, row.names = FALSE)
  logger::log_info("Saved calibration metrics to {metrics_file}")
  
  # Save the calibration models
  cal_models_file <- glue("Models/Calibrated/{model_method}_{data_type}_{landmark_str}_cal_models.rds")
  saveRDS(cal_models, cal_models_file)
  logger::log_info("Saved calibration models to {cal_models_file}")
  
  # Create a prediction function that applies the calibration
  predict_calibrated <- function(new_data, thresh = 0.5) {
    # Make raw predictions
    raw_probs <- predict(model, new_data, type = "prob") %>%
      as.data.frame()
    
    # Apply calibration to each watershed probability
    calibrated_probs <- raw_probs
    
    for(ws in names(cal_models)) {
      # Create temporary dataframe for calibration
      temp_df <- data.frame(
        prob = raw_probs[[ws]]
      )
      
      # Apply calibration
      calibrated <- cal_apply(temp_df, cal_models[[ws]])
      
      # Store calibrated probabilities
      calibrated_probs[[ws]] <- calibrated$.cal_estimate
    }
    
    # Determine class predictions based on highest calibrated probability
    class_idx <- apply(calibrated_probs, 1, which.max)
    classes <- colnames(calibrated_probs)[class_idx]
    
    # Confidence scores
    confidence <- apply(calibrated_probs, 1, max)
    
    # Return results
    list(
      class = classes,
      calibrated_probabilities = calibrated_probs,
      confidence = confidence
    )
  }
  
  # Create a calibrated model object
  calibrated_model <- list(
    base_model = model,
    cal_models = cal_models,
    predict = predict_calibrated,
    model_info = list(
      data_type = data_type,
      landmark_str = landmark_str,
      model_method = model_method
    )
  )
  
  # Save the calibrated model
  calibrated_model_file <- glue("Models/Calibrated/{model_method}_{data_type}_{landmark_str}_calibrated.rds")
  saveRDS(calibrated_model, calibrated_model_file)
  logger::log_info("Saved calibrated model to {calibrated_model_file}")
  
  # Test the calibrated model on the test data
  cal_predictions <- predict_calibrated(testdata)
  
  # Create results dataframe
  results_df <- tibble(
    Fish_id = test_metadata$Fish_id,
    Actual = testdata$Watershed,
    Predicted = cal_predictions$class,
    Confidence = cal_predictions$confidence
  ) %>%
    mutate(
      Correct = Predicted == Actual
    ) %>%
    bind_cols(as.data.frame(cal_predictions$calibrated_probabilities))
  
  # Calculate accuracy
  accuracy <- mean(results_df$Correct)
  logger::log_info("Calibrated model accuracy: {round(accuracy, 4)}")
  
  # Save prediction results
  results_file <- glue("Model_Results/Calibration/{model_method}_{data_type}_{landmark_str}_cal_predictions.csv")
  write.csv(results_df, results_file, row.names = FALSE)
  
  return(list(
    metrics = all_metrics,
    results = results_df,
    model = calibrated_model
  ))
}

################################################################################
# 2. Calibrate the top models
################################################################################

# Number of top models to calibrate
num_top_models <- min(3, nrow(top_models))

calibration_results <- list()

for(i in 1:num_top_models) {
  model_config <- top_models[i, ]
  
  data_type <- model_config$Data_Type
  landmark_str <- model_config$Model_Landmarks
  model_method <- model_config$Model_Method
  
  # Replace commas in landmark string with underscores for filename
  landmark_str <- gsub(",", "_", landmark_str)
  
  logger::log_info("Calibrating model #{i}: {model_method} using {data_type} data with landmarks {landmark_str}")
  
  result <- calibrate_model(data_type, landmark_str, model_method)
  
  if(!is.null(result)) {
    calibration_results[[i]] <- result
  }
}

################################################################################
# 3. Create summary of calibration results
################################################################################

if(length(calibration_results) > 0) {
  # Combine metrics from all models
  all_cal_metrics <- bind_rows(
    lapply(calibration_results, function(x) {
      x$metrics %>%
        mutate(
          Model = glue("{x$model$model_info$model_method} ({x$model$model_info$data_type}, {x$model$model_info$landmark_str})")
        )
    })
  )
  
  # Save combined metrics
  write.csv(all_cal_metrics, "Model_Results/Calibration/All_Calibration_Metrics.csv", row.names = FALSE)
  
  # Create summary visualization
  cal_summary_plot <- ggplot(all_cal_metrics, 
                             aes(x = Watershed, y = Brier_Improvement, fill = Model)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(
      title = "Probability Calibration Improvement",
      subtitle = "Percent reduction in Brier score (higher is better)",
      x = "Watershed",
      y = "Brier Score Improvement (%)"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    )
  
  ggsave("Figures/Calibration/Calibration_Improvement_Summary.png", 
         cal_summary_plot, width = 10, height = 6, dpi = 300)
  
  logger::log_info("Model calibration completed successfully")
  logger::log_info("Summary saved to Model_Results/Calibration/All_Calibration_Metrics.csv")
}

################################################################################
# 4. Example of how to use the calibrated model for prediction
################################################################################

# Example of how to load and use a calibrated model
example_usage <- function() {
  logger::log_info("Example: Loading and using a calibrated model")
  
  # Select the first calibrated model
  model_config <- top_models[1, ]
  data_type <- model_config$Data_Type
  landmark_str <- gsub(",", "_", model_config$Model_Landmarks)
  model_method <- model_config$Model_Method
  
  # Load the calibrated model
  cal_model_file <- glue("Models/Calibrated/{model_method}_{data_type}_{landmark_str}_calibrated.rds")
  
  if(file.exists(cal_model_file)) {
    cal_model <- readRDS(cal_model_file)
    logger::log_info("Loaded calibrated model from {cal_model_file}")
    
    # Load sample data for prediction (using test data as an example)
    train_test_file <- glue("Data/Train_Test_Sets/{data_type}_{landmark_str}_train_test_data.rds")
    train_test_data <- readRDS(train_test_file)
    
    # Take a small sample for demonstration
    sample_data <- train_test_data$test[1:5, ]
    
    # Make predictions using the calibrated model
    predictions <- cal_model$predict(sample_data)
    
    # Display the results
    results <- tibble(
      Actual = sample_data$Watershed,
      Predicted = predictions$class,
      Confidence = predictions$confidence
    ) %>%
      mutate(
        Correct = Predicted == Actual
      ) %>%
      bind_cols(as.data.frame(predictions$calibrated_probabilities))
    
    print(results)
    
    # Output to a markdown file for documentation
    sink("Model_Results/Calibration/Example_Usage.md")
    
    cat("# Example Usage of Calibrated Model\n\n")
    cat("## Model Information\n\n")
    cat("- Data Type:", cal_model$model_info$data_type, "\n")
    cat("- Landmarks:", cal_model$model_info$landmark_str, "\n")
    cat("- Model Method:", cal_model$model_info$model_method, "\n\n")
    
    cat("## Prediction Example\n\n")
    cat("Input data: First 5 samples from test set\n\n")
    
    cat("### Prediction Results\n\n")
    print(results)
    
    cat("\n\n## How to Use the Calibrated Model\n\n")
    cat("```r\n")
    cat("# Load the calibrated model\n")
    cat("cal_model <- readRDS(\"", cal_model_file, "\")\n\n")
    cat("# Make predictions on new data\n")
    cat("predictions <- cal_model$predict(new_data)\n\n")
    cat("# Results will include:\n")
    cat("# - class: Predicted class labels\n")
    cat("# - calibrated_probabilities: Calibrated probability for each class\n")
    cat("# - confidence: Highest calibrated probability (confidence score)\n")
    cat("```\n")
    
    sink()
    
    logger::log_info("Example usage documentation saved to Model_Results/Calibration/Example_Usage.md")
  } else {
    logger::log_error("Calibrated model file not found: {cal_model_file}")
  }
}

# Run the example usage function
example_usage()