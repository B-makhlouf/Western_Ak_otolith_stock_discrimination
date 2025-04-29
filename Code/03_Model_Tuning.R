######### Model_Probability_Calibration.R #########
# Calibrates model probabilities and evaluates performance on test data
# Creates pre/post-calibration visualizations with performance metrics

library(tidyverse)
library(caret)
library(ggplot2)
library(gridExtra)
library(here)
library(probably) # For probability calibration functions

# Create necessary directories
dirs <- c("Data/Calibrated_Models", "Figures/Calibration", "Data/Calibration_Results")
for (dir in dirs) {
  if (!dir.exists(here(dir))) {
    dir.create(here(dir), recursive = TRUE)
  }
}

# Load comprehensive results to identify models to calibrate
all_results <- read.csv(here("Data/Model_Results/ALL_Models_Results.csv"))

# Function to load model and its test data
load_model_and_test_data <- function(data_type, model_method, landmark_filter) {
  # Format identifier
  landmark_str <- paste(landmark_filter, collapse = "_")
  data_identifier <- paste(data_type, model_method, landmark_str, sep = "_")
  
  # Load model
  model_path <- here(paste0("Models/", model_method, "_models/", data_identifier, ".rds"))
  if (!file.exists(model_path)) {
    stop("Model file not found: ", model_path)
  }
  model <- readRDS(model_path)
  
  # Load test data
  data_path <- here(paste0("Data/Train_Test_Sets/", data_identifier, "_datasets.rds"))
  if (!file.exists(data_path)) {
    stop("Test data file not found: ", data_path)
  }
  train_test_data <- readRDS(data_path)
  
  return(list(model = model, test_data = train_test_data$test, 
              test_metadata = train_test_data$test_metadata))
}

# Function to evaluate model performance
evaluate_model <- function(predictions, true_values, probabilities = NULL) {
  # Ensure factors
  predictions <- as.factor(predictions)
  true_values <- as.factor(true_values)
  
  # Match factor levels
  if (!identical(levels(predictions), levels(true_values))) {
    predictions <- factor(predictions, levels = levels(true_values))
  }
  
  # Compute confusion matrix
  conf <- confusionMatrix(predictions, true_values)
  
  # Extract performance metrics
  metrics <- data.frame(
    Accuracy = conf$overall["Accuracy"],
    Kappa = conf$overall["Kappa"]
  )
  
  # Add class-specific metrics
  for (class in levels(true_values)) {
    if (class %in% rownames(conf$byClass)) {
      class_metrics <- conf$byClass[class, ]
      metrics[[paste0(class, "_Sensitivity")]] <- class_metrics["Sensitivity"]
      metrics[[paste0(class, "_Specificity")]] <- class_metrics["Specificity"]
      metrics[[paste0(class, "_F1")]] <- class_metrics["F1"]
      metrics[[paste0(class, "_BalancedAccuracy")]] <- class_metrics["Balanced Accuracy"]
    }
  }
  
  # If probabilities provided, add probability metrics
  if (!is.null(probabilities)) {
    # Log loss
    log_loss <- 0
    n <- length(true_values)
    
    for (i in 1:n) {
      class_idx <- which(levels(true_values) == true_values[i])
      prob <- probabilities[i, class_idx]
      # Avoid log(0)
      prob <- max(min(prob, 0.99999), 0.00001)
      log_loss <- log_loss - log(prob)/n
    }
    
    metrics$LogLoss <- log_loss
    
    # Brier score (mean squared error of probabilities)
    brier_score <- 0
    for (i in 1:n) {
      actual_probs <- rep(0, length(levels(true_values)))
      actual_probs[which(levels(true_values) == true_values[i])] <- 1
      brier_score <- brier_score + sum((probabilities[i,] - actual_probs)^2)/n
    }
    
    metrics$BrierScore <- brier_score
  }
  
  return(list(conf_matrix = conf, metrics = metrics))
}

# Function to calibrate a model and create visualization
calibrate_model <- function(data_type, model_method, landmark_filter) {
  # Load model and test data
  model_data <- load_model_and_test_data(data_type, model_method, landmark_filter)
  model <- model_data$model
  test_data <- model_data$test_data
  
  # Create model info
  model_info <- list(
    Data_Type = data_type,
    Model_Method = model_method,
    Landmark_Filter = paste(landmark_filter, collapse = "_")
  )
  
  # Get original predictions and probabilities
  predictions <- predict(model, test_data)
  probabilities <- predict(model, test_data, type = "prob")
  
  # Evaluate original model
  original_eval <- evaluate_model(predictions, test_data$Watershed, probabilities)
  
  # Get unique watersheds
  watersheds <- colnames(probabilities)
  
  # Create pre-calibration plots and calibrators
  pre_cal_plots <- list()
  cal_estimators <- list()
  
  for (ws in watersheds) {
    # Create binary target for this watershed
    binary_results <- data.frame(
      actual = test_data$Watershed == ws,
      prob = probabilities[, ws]
    )
    
    # Create pre-calibration plot
    pre_cal_plots[[ws]] <- cal_plot_windowed(
      binary_results, truth = actual, estimate = prob, 
      window_size = 0.2, step_size = 0.05
    ) +
      ggtitle(paste("Pre-Calibration:", ws)) +
      theme_minimal() +
      coord_equal() +
      theme(plot.title = element_text(size = 10))
    
    # Create calibration estimator
    cal_estimators[[ws]] <- cal_estimate_logistic(
      binary_results, truth = actual, estimate = prob
    )
  }
  
  # Create combined pre-calibration plot
  pre_cal_combined <- grid.arrange(
    grobs = pre_cal_plots,
    ncol = length(watersheds),
    top = paste("Pre-Calibration Curves -", 
                data_type, "-", model_method, "-", paste(landmark_filter, collapse = "_"))
  )
  
  # Save pre-calibration plot
  ggsave(
    here(paste0("Figures/Calibration/Pre_Calibration_", data_type, "_", 
                model_method, "_", paste(landmark_filter, collapse = "_"), ".png")),
    pre_cal_combined,
    width = 10, height = 4, dpi = 300
  )
  
  # Apply calibration to test data
  calibrated_probs <- probabilities
  
  for (ws in watersheds) {
    # Extract probabilities for this watershed
    ws_probs <- data.frame(prob = probabilities[, ws])
    
    # Apply calibration
    cal_probs <- cal_apply(ws_probs, cal_estimators[[ws]])
    
    # Update calibrated probabilities
    calibrated_probs[, ws] <- cal_probs$prob
  }
  
  # Normalize calibrated probabilities to sum to 1
  calibrated_probs <- calibrated_probs / rowSums(calibrated_probs)
  
  # Make predictions using calibrated probabilities
  calibrated_preds <- apply(calibrated_probs, 1, function(row) {
    levels(test_data$Watershed)[which.max(row)]
  })
  calibrated_preds <- factor(calibrated_preds, levels = levels(test_data$Watershed))
  
  # Evaluate calibrated model
  calibrated_eval <- evaluate_model(calibrated_preds, test_data$Watershed, calibrated_probs)
  
  # Create post-calibration plots
  post_cal_plots <- list()
  
  for (ws in watersheds) {
    # Create binary target for this watershed
    binary_results <- data.frame(
      actual = test_data$Watershed == ws,
      prob = calibrated_probs[, ws]
    )
    
    # Create post-calibration plot
    post_cal_plots[[ws]] <- cal_plot_windowed(
      binary_results, truth = actual, estimate = prob, 
      window_size = 0.2, step_size = 0.05
    ) +
      ggtitle(paste("Post-Calibration:", ws)) +
      theme_minimal() +
      coord_equal() +
      theme(plot.title = element_text(size = 10))
  }
  
  # Create combined post-calibration plot
  post_cal_combined <- grid.arrange(
    grobs = post_cal_plots,
    ncol = length(watersheds),
    top = paste("Post-Calibration Curves -",
                data_type, "-", model_method, "-", paste(landmark_filter, collapse = "_"))
  )
  
  # Save post-calibration plot
  ggsave(
    here(paste0("Figures/Calibration/Post_Calibration_", data_type, "_", 
                model_method, "_", paste(landmark_filter, collapse = "_"), ".png")),
    post_cal_combined,
    width = 10, height = 4, dpi = 300
  )
  
  # Create side-by-side comparison
  comparison <- grid.arrange(
    pre_cal_combined, post_cal_combined,
    ncol = 1,
    top = paste("Calibration Comparison -",
                data_type, "-", model_method, "-", paste(landmark_filter, collapse = "_"))
  )
  
  # Save comparison plot
  ggsave(
    here(paste0("Figures/Calibration/Comparison_", data_type, "_", 
                model_method, "_", paste(landmark_filter, collapse = "_"), ".png")),
    comparison,
    width = 12, height = 8, dpi = 300
  )
  
  # Calculate calibration metrics (Expected Calibration Error)
  calc_ece <- function(actual, prob, bins = 10) {
    # Create bins
    bin_width <- 1 / bins
    bin_indices <- floor(prob / bin_width) + 1
    bin_indices[bin_indices > bins] <- bins
    
    # Calculate ECE
    ece <- 0
    bin_counts <- numeric(bins)
    
    for (bin in 1:bins) {
      bin_mask <- bin_indices == bin
      if (sum(bin_mask) > 0) {
        bin_probs <- prob[bin_mask]
        bin_actual <- actual[bin_mask]
        bin_avg_prob <- mean(bin_probs)
        bin_avg_actual <- mean(bin_actual)
        bin_counts[bin] <- sum(bin_mask)
        ece <- ece + (sum(bin_mask) / length(prob)) * abs(bin_avg_prob - bin_avg_actual)
      }
    }
    
    return(list(ece = ece, bin_counts = bin_counts))
  }
  
  # Calculate ECE for pre and post calibration
  pre_cal_ece <- sapply(watersheds, function(ws) {
    calc_ece(test_data$Watershed == ws, probabilities[, ws])$ece
  })
  
  post_cal_ece <- sapply(watersheds, function(ws) {
    calc_ece(test_data$Watershed == ws, calibrated_probs[, ws])$ece
  })
  
  # Create calibration metrics table
  cal_metrics <- data.frame(
    Watershed = watersheds,
    Pre_Calibration_ECE = pre_cal_ece,
    Post_Calibration_ECE = post_cal_ece,
    Improvement = pre_cal_ece - post_cal_ece,
    Percent_Improvement = (pre_cal_ece - post_cal_ece) / pre_cal_ece * 100
  )
  
  # Add overall metrics
  cal_metrics <- rbind(
    cal_metrics,
    data.frame(
      Watershed = "Overall",
      Pre_Calibration_ECE = mean(pre_cal_ece),
      Post_Calibration_ECE = mean(post_cal_ece),
      Improvement = mean(pre_cal_ece) - mean(post_cal_ece),
      Percent_Improvement = (mean(pre_cal_ece) - mean(post_cal_ece)) / mean(pre_cal_ece) * 100
    )
  )
  
  # Compare performance metrics
  performance_comparison <- data.frame(
    Metric = names(original_eval$metrics),
    Original = unlist(original_eval$metrics),
    Calibrated = unlist(calibrated_eval$metrics),
    Difference = unlist(calibrated_eval$metrics) - unlist(original_eval$metrics),
    Percent_Change = (unlist(calibrated_eval$metrics) - unlist(original_eval$metrics)) / 
      unlist(original_eval$metrics) * 100
  )
  
  # Save metrics
  write.csv(
    cal_metrics,
    here(paste0("Data/Calibration_Results/ECE_", data_type, "_", 
                model_method, "_", paste(landmark_filter, collapse = "_"), ".csv")),
    row.names = FALSE
  )
  
  write.csv(
    performance_comparison,
    here(paste0("Data/Calibration_Results/Performance_", data_type, "_", 
                model_method, "_", paste(landmark_filter, collapse = "_"), ".csv")),
    row.names = FALSE
  )
  
  # Create confusion matrix comparison
  conf_matrix_comparison <- data.frame(
    Original = c(
      paste("Accuracy:", round(original_eval$conf_matrix$overall["Accuracy"], 4)),
      "Confusion Matrix:",
      capture.output(print(original_eval$conf_matrix$table))
    ),
    Calibrated = c(
      paste("Accuracy:", round(calibrated_eval$conf_matrix$overall["Accuracy"], 4)),
      "Confusion Matrix:",
      capture.output(print(calibrated_eval$conf_matrix$table))
    )
  )
  
  write.csv(
    conf_matrix_comparison,
    here(paste0("Data/Calibration_Results/ConfMatrix_", data_type, "_", 
                model_method, "_", paste(landmark_filter, collapse = "_"), ".csv")),
    row.names = FALSE
  )
  
  # Create calibrated model object
  calibrated_model <- list(
    original_model = model,
    cal_estimators = cal_estimators,
    model_info = model_info,
    cal_metrics = cal_metrics,
    performance_comparison = performance_comparison,
    
    predict = function(newdata, type = "raw") {
      # Get raw predictions from original model
      if (type == "raw") {
        # Get raw probabilities first
        raw_probs <- predict(model, newdata, type = "prob")
        
        # Apply calibration
        calibrated_probs <- raw_probs
        
        for (ws in names(cal_estimators)) {
          # Extract raw probabilities for this watershed
          ws_probs <- data.frame(prob = raw_probs[, ws])
          
          # Apply calibration
          cal_probs <- cal_apply(ws_probs, cal_estimators[[ws]])
          
          # Replace with calibrated probabilities
          calibrated_probs[, ws] <- cal_probs$prob
        }
        
        # Normalize probabilities to sum to 1
        calibrated_probs <- calibrated_probs / rowSums(calibrated_probs)
        
        # Return class with highest probability
        predictions <- apply(calibrated_probs, 1, function(row) {
          colnames(calibrated_probs)[which.max(row)]
        })
        
        return(factor(predictions, levels = colnames(calibrated_probs)))
      } else if (type == "prob") {
        # Get raw probabilities
        raw_probs <- predict(model, newdata, type = "prob")
        
        # Apply calibration to each watershed's probabilities
        calibrated_probs <- raw_probs
        
        for (ws in names(cal_estimators)) {
          # Extract raw probabilities for this watershed
          ws_probs <- data.frame(prob = raw_probs[, ws])
          
          # Apply calibration
          cal_probs <- cal_apply(ws_probs, cal_estimators[[ws]])
          
          # Replace with calibrated probabilities
          calibrated_probs[, ws] <- cal_probs$prob
        }
        
        # Normalize probabilities to sum to 1
        row_sums <- rowSums(calibrated_probs)
        calibrated_probs <- calibrated_probs / row_sums
        
        return(calibrated_probs)
      } else {
        stop("Invalid prediction type")
      }
    }
  )
  
  # Save calibrated model
  saveRDS(
    calibrated_model,
    here(paste0("Data/Calibrated_Models/Calibrated_", data_type, "_", 
                model_method, "_", paste(landmark_filter, collapse = "_"), ".rds"))
  )
  
  # Create performance visualization
  perf_metrics <- performance_comparison %>%
    filter(grepl("Accuracy|F1|Sensitivity|Specificity", Metric)) %>%
    mutate(
      MetricType = case_when(
        grepl("_", Metric) ~ strsplit(Metric, "_")[[1]][2],
        TRUE ~ Metric
      ),
      Watershed = case_when(
        grepl("_", Metric) ~ strsplit(Metric, "_")[[1]][1],
        TRUE ~ "Overall"
      )
    )
  
  perf_plot <- ggplot(perf_metrics, aes(x = Watershed, y = Percent_Change, fill = MetricType)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    theme_minimal() +
    labs(
      title = paste("Performance Change After Calibration -", 
                    data_type, "-", model_method, "-", paste(landmark_filter, collapse = "_")),
      x = "Watershed",
      y = "Percent Change (%)",
      fill = "Metric"
    )
  
  ggsave(
    here(paste0("Figures/Calibration/Performance_", data_type, "_", 
                model_method, "_", paste(landmark_filter, collapse = "_"), ".png")),
    perf_plot,
    width = 10, height = 6, dpi = 300
  )
  
  return(list(
    calibrated_model = calibrated_model,
    pre_cal_plots = pre_cal_plots,
    post_cal_plots = post_cal_plots,
    cal_metrics = cal_metrics,
    performance_comparison = performance_comparison,
    original_eval = original_eval,
    calibrated_eval = calibrated_eval
  ))
}

# Function to process models from results file
process_models_from_results <- function(results_file) {
  # Read results
  results <- read.csv(results_file)
  
  # Get unique model configurations
  model_configs <- results %>%
    select(Data_Type, Model_Method, Landmark_Filter) %>%
    distinct()
  
  # Create summary dataframe for all models
  all_perf_summary <- data.frame()
  
  # Process each model
  for (i in 1:nrow(model_configs)) {
    tryCatch({
      row <- model_configs[i, ]
      
      cat("Calibrating model:", row$Data_Type, "-", row$Model_Method, "-", row$Landmark_Filter, "\n")
      
      # Calibrate model
      cal_results <- calibrate_model(
        row$Data_Type, 
        row$Model_Method, 
        strsplit(row$Landmark_Filter, "_")[[1]]
      )
      
      # Add to summary
      model_summary <- data.frame(
        Data_Type = row$Data_Type,
        Model_Method = row$Model_Method,
        Landmark_Filter = row$Landmark_Filter,
        Original_Accuracy = cal_results$original_eval$metrics$Accuracy,
        Calibrated_Accuracy = cal_results$calibrated_eval$metrics$Accuracy,
        Original_LogLoss = cal_results$original_eval$metrics$LogLoss,
        Calibrated_LogLoss = cal_results$calibrated_eval$metrics$LogLoss,
        Original_BrierScore = cal_results$original_eval$metrics$BrierScore,
        Calibrated_BrierScore = cal_results$calibrated_eval$metrics$BrierScore,
        Mean_ECE_Before = mean(cal_results$cal_metrics$Pre_Calibration_ECE[
          cal_results$cal_metrics$Watershed != "Overall"]),
        Mean_ECE_After = mean(cal_results$cal_metrics$Post_Calibration_ECE[
          cal_results$cal_metrics$Watershed != "Overall"]),
        ECE_Improvement_Percent = mean(cal_results$cal_metrics$Percent_Improvement[
          cal_results$cal_metrics$Watershed != "Overall"])
      )
      
      all_perf_summary <- rbind(all_perf_summary, model_summary)
      
      cat("  Completed calibration\n")
    }, error = function(e) {
      cat("Error calibrating model:", row$Data_Type, "-", row$Model_Method, "-", row$Landmark_Filter, "\n")
      cat("  ", conditionMessage(e), "\n")
    })
  }
  
  # Save overall summary
  write.csv(
    all_perf_summary,
    here("Data/Calibration_Results/All_Models_Calibration_Summary.csv"),
    row.names = FALSE
  )
  
  # Create summary visualization
  if (nrow(all_perf_summary) > 0) {
    # ECE improvement plot
    ece_plot <- ggplot(all_perf_summary, 
                       aes(x = paste(Data_Type, Landmark_Filter, sep = "_"), 
                           y = ECE_Improvement_Percent, 
                           fill = Model_Method)) +
      geom_bar(stat = "identity", position = "dodge") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(
        title = "ECE Improvement After Calibration",
        x = "Model Configuration",
        y = "ECE Improvement (%)",
        fill = "Model Method"
      )
    
    # Brier score improvement plot
    brier_improvement <- (all_perf_summary$Original_BrierScore - 
                            all_perf_summary$Calibrated_BrierScore) / 
      all_perf_summary$Original_BrierScore * 100
    
    brier_plot <- ggplot(all_perf_summary, 
                         aes(x = paste(Data_Type, Landmark_Filter, sep = "_"), 
                             y = brier_improvement, 
                             fill = Model_Method)) +
      geom_bar(stat = "identity", position = "dodge") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(
        title = "Brier Score Improvement After Calibration",
        x = "Model Configuration",
        y = "Brier Score Improvement (%)",
        fill = "Model Method"
      )
    
    # Combine plots
    combined_summary <- grid.arrange(
      ece_plot, brier_plot,
      ncol = 1,
      top = "Calibration Effects Across All Models"
    )
    
    ggsave(
      here("Figures/Calibration/All_Models_Calibration_Summary.png"),
      combined_summary,
      width = 12, height = 8, dpi = 300
    )
  }
  
  return(all_perf_summary)
}

# Main execution
cat("Starting model probability calibration...\n")

# Process models from results file
all_perf_summary <- process_models_from_results(here("Data/Model_Results/ALL_Models_Results.csv"))

cat("Model probability calibration completed!\n")
cat("Calibrated models saved to: Data/Calibrated_Models/\n")
cat("Calibration plots saved to: Figures/Calibration/\n")
cat("Calibration and performance metrics saved to: Data/Calibration_Results/\n")
cat("Overall calibration summary saved to: Data/Calibration_Results/All_Models_Calibration_Summary.csv\n")

