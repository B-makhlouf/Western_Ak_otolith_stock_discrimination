######### MultiModel_Comprehensive_Comparison.R #########
# Performs classification across all combinations of data types, models, and landmarks
# Creates a SINGLE comprehensive visualization and saves all models

library(viridis)
library(tidyverse)
library(here)
library(caret)
library(ggplot2)
library(gridExtra)
library(grid)

# Load metadata
All_Metadata <- read.csv(here("Data/01_Metadata and QC/Metadata_and_QC.csv"))

# Define parameters to iterate over
data_types <- c("RAW", "MA", "GAM", "COMBINED", "Sr88")
model_methods <- c("knn", "rf", "svmRadial")
landmark_combinations <- list(
  "Core_Fw" = c("Core", "Fw")
)

# Function to load processed data with consistent file paths
load_processed_data <- function(data_type, landmark_filter) {
  # Construct filename
  landmark_str <- paste(landmark_filter, collapse = "_")
  
  # Primary path - from preprocessing script
  if (data_type == "COMBINED") {
    filename <- paste0("Processed_", landmark_str, "_Combined.csv")
  } else if (data_type == "Sr88") {
    filename <- paste0("Processed_", landmark_str, "_Sr88.csv")
  } else {
    filename <- paste0("Processed_", landmark_str, "_", data_type, ".csv")
  }
  
  # Try multiple potential locations
  possible_paths <- c(
    here("Data/02_Preprocessed_ts_matrices", filename),
    here("Data/Processed/Preprocessed_ts_matrices", filename),
    here("Data/Classification_ts_matrices/Sr8786", paste0("Processed_", landmark_str, "_", data_type, ".csv"))
  )
  
  # For COMBINED type, also try Sr88_Iso path
  if (data_type == "COMBINED") {
    possible_paths <- c(
      possible_paths,
      here("Data/Classification_ts_matrices/Sr88", paste0("Processed_", landmark_str, "_Sr88_Iso.csv"))
    )
  }
  
  # For Sr88 type
  if (data_type == "Sr88") {
    possible_paths <- c(
      possible_paths,
      here("Data/Classification_ts_matrices/Sr88", paste0("Processed_", landmark_str, "_Sr88.csv"))
    )
  }
  
  # Try each path
  for (path in possible_paths) {
    if (file.exists(path)) {
      cat("Loading data from:", path, "\n")
      return(read.csv(path))
    }
  }
  
  # If no files found
  stop(paste("File not found for", data_type, "data with", 
             paste(landmark_filter, collapse=","), "landmarks.",
             "Tried paths:", paste(possible_paths, collapse=", ")))
}

# Function to train and evaluate models
run_classification <- function(processed_data, data_type, model_method, landmark_filter) {
  # Format data identifier
  landmark_str <- paste(landmark_filter, collapse = "_")
  data_identifier <- paste(data_type, model_method, landmark_str, sep = "_")
  
  # Print progress
  cat("Processing:", data_identifier, "\n")
  
  # Merge with metadata and filter for QC
  AnalysisDataAll <- processed_data %>%
    left_join(All_Metadata %>% select(-Year), by = c("Fish_id" = "Fish_ID")) %>%
    select((ncol(.)-12):ncol(.), everything()) %>%
    filter(QC_Grade == "Yes")
  
  # Separate metadata and timeseries data
  Analysis_metadata <- AnalysisDataAll[, 1:17]
  Analysis_ts_data <- AnalysisDataAll[, 18:ncol(AnalysisDataAll)]
  
  # Prepare model data
  ModelData <- Analysis_ts_data %>%
    as.data.frame() %>%
    mutate(Watershed = Analysis_metadata$Watershed)
  
  # Ensure column names are valid for modeling
  if (any(grepl("^[0-9]", names(ModelData)))) {
    colnames(ModelData)[!colnames(ModelData) %in% c("Watershed")] <- 
      paste0("X", 1:(ncol(ModelData)-1))
  }
  
  # Split data into training (80%) and testing (20%)
  set.seed(123)
  trainIndex <- createDataPartition(ModelData$Watershed, p = 0.8, list = FALSE)
  traindata <- ModelData[trainIndex, ]
  testdata <- ModelData[-trainIndex, ]
  
  # Record training/testing sets
  train_test_data <- list(
    train = traindata,
    test = testdata,
    train_idx = trainIndex,
    test_metadata = Analysis_metadata[-trainIndex, ]
  )
  
  # Save training/testing sets
  dir.create(here("Data/Train_Test_Sets"), recursive = TRUE, showWarnings = FALSE)
  saveRDS(train_test_data, 
          here(paste0("Data/Train_Test_Sets/", data_identifier, "_datasets.rds")))
  
  # Set up cross-validation
  control <- trainControl(method = "cv", number = 5, classProbs = TRUE)
  
  # Train model
  model <- train(
    Watershed ~ ., 
    data = traindata, 
    method = model_method, 
    trControl = control
  )
  
  # Save model
  model_dir <- here(paste0("Models/", model_method, "_models"))
  dir.create(model_dir, recursive = TRUE, showWarnings = FALSE)
  saveRDS(model, here(paste0(model_dir, "/", data_identifier, ".rds")))
  
  # Make predictions
  predictions <- predict(model, testdata)
  probabilities <- predict(model, testdata, type = "prob")
  
  # Create results dataframe
  idScores <- Analysis_metadata[-trainIndex, ] %>%
    select(Fish_id) %>%
    mutate(
      Predicted = predictions,
      Actual = testdata$Watershed,
      Confidence = apply(probabilities, 1, max),
      Correct = Predicted == Actual
    ) %>%
    bind_cols(probabilities)
  
  # Ensure factors are consistent
  idScores <- idScores %>%
    mutate(Predicted = as.factor(Predicted), Actual = as.factor(Actual))
  
  # Compute confusion matrix
  conf_matrix <- confusionMatrix(idScores$Predicted, idScores$Actual)
  
  # Extract watershed-specific metrics
  watershed_results <- data.frame()
  for (watershed in unique(idScores$Actual)) {
    # Create binary classification for this watershed
    binary_actual <- ifelse(idScores$Actual == watershed, "Yes", "No")
    binary_predicted <- ifelse(idScores$Predicted == watershed, "Yes", "No")
    binary_conf <- confusionMatrix(factor(binary_predicted), factor(binary_actual))
    
    # Add metrics to results
    watershed_metrics <- data.frame(
      Data_Type = data_type,
      Model_Method = model_method,
      Landmark_Filter = landmark_str,
      Watershed = watershed,
      Accuracy = binary_conf$overall["Accuracy"],
      Sensitivity = binary_conf$byClass["Sensitivity"],
      Specificity = binary_conf$byClass["Specificity"],
      F1_Score = binary_conf$byClass["F1"],
      Balanced_Accuracy = binary_conf$byClass["Balanced Accuracy"],
      Precision = binary_conf$byClass["Pos Pred Value"],
      Sample_Size = sum(idScores$Actual == watershed)
    )
    
    watershed_results <- rbind(watershed_results, watershed_metrics)
  }
  
  # Add overall metrics
  overall_metrics <- data.frame(
    Data_Type = data_type,
    Model_Method = model_method,
    Landmark_Filter = landmark_str,
    Watershed = "Overall",
    Accuracy = conf_matrix$overall["Accuracy"],
    Balanced_Accuracy = NA,  # Will calculate weighted average later
    F1_Score = NA,           # Will calculate weighted average later
    Sensitivity = NA,        # Will calculate weighted average later
    Specificity = NA,        # Will calculate weighted average later
    Precision = NA,          # Will calculate weighted average later
    Sample_Size = nrow(testdata)
  )
  
  # Calculate weighted metrics for overall performance
  watershed_counts <- table(idScores$Actual)
  total_samples <- sum(watershed_counts)
  
  for (metric in c("Balanced_Accuracy", "F1_Score", "Sensitivity", "Specificity", "Precision")) {
    # Extract metrics for each watershed
    watershed_metrics <- watershed_results[[metric]]
    
    # Calculate weighted average based on sample sizes
    weighted_avg <- sum(watershed_metrics * (watershed_results$Sample_Size / total_samples))
    overall_metrics[[metric]] <- weighted_avg
  }
  
  # Combine watershed and overall results
  all_results <- rbind(watershed_results, overall_metrics)
  
  # Save confusion matrix and predictions
  conf_dir <- here("Data/Confusion_Matrices")
  dir.create(conf_dir, recursive = TRUE, showWarnings = FALSE)
  write.csv(as.data.frame(conf_matrix$table), 
            here(paste0(conf_dir, "/", data_identifier, "_confmatrix.csv")))
  write.csv(idScores, 
            here(paste0(conf_dir, "/", data_identifier, "_predictions.csv")))
  
  return(all_results)
}

# Create necessary directories
dirs <- c("Data/Model_Results", "Figures/ModelOutputs")
for (dir in dirs) {
  if (!dir.exists(here(dir))) {
    dir.create(here(dir), recursive = TRUE)
  }
}

# Initialize results storage
all_results <- data.frame()

# Run all model combinations
for (landmark_name in names(landmark_combinations)) {
  landmark_filter <- landmark_combinations[[landmark_name]]
  
  for (data_type in data_types) {
    # Try to load data
    tryCatch({
      processed_data <- load_processed_data(data_type, landmark_filter)
      
      for (model_method in model_methods) {
        # Run classification and collect results
        results <- run_classification(
          processed_data, data_type, model_method, landmark_filter
        )
        
        # Add to all results
        all_results <- rbind(all_results, results)
      }
    }, error = function(e) {
      message("Error processing ", data_type, " with ", landmark_name, ": ", e$message)
    })
  }
}

# Save the single comprehensive CSV
results_dir <- here("Data/Model_Results")
write.csv(all_results, file.path(results_dir, "ALL_Models_Results.csv"), row.names = FALSE)

# Reformat results for comprehensive visualization
viz_data <- all_results %>%
  # Create consistent naming for visualization
  mutate(
    ModelMethod = case_when(
      Model_Method == "knn" ~ "KNN",
      Model_Method == "rf" ~ "Random Forest",
      Model_Method == "svmRadial" ~ "SVM",
      TRUE ~ Model_Method
    ),
    DataType = case_when(
      Data_Type == "RAW" ~ "Sr8786_RAW",
      Data_Type == "MA" ~ "Sr8786_MA", 
      Data_Type == "GAM" ~ "Sr8786_GAM",
      Data_Type == "COMBINED" ~ "Combined",
      Data_Type == "Sr88" ~ "Sr88",
      TRUE ~ Data_Type
    )
  )

# Create single comprehensive visualization
create_comprehensive_viz <- function(data) {
  # Select metrics for visualization
  viz_metrics <- c("Accuracy", "F1_Score", "Specificity")
  
  # Create overall metrics section
  overall_data <- data %>%
    filter(Watershed == "Overall") %>%
    select(DataType, ModelMethod, all_of(viz_metrics)) %>%
    pivot_longer(cols = all_of(viz_metrics), names_to = "Metric", values_to = "Value") %>%
    mutate(
      Metric = case_when(
        Metric == "Accuracy" ~ "Overall Accuracy",
        Metric == "F1_Score" ~ "Overall F1 Score", 
        Metric == "Specificity" ~ "Overall Specificity",
        TRUE ~ Metric
      )
    )
  
  # Overall metrics heatmaps
  overall_plots <- lapply(unique(overall_data$Metric), function(m) {
    metric_data <- overall_data %>% filter(Metric == m)
    
    ggplot(metric_data, aes(x = ModelMethod, y = DataType, fill = Value)) +
      geom_tile(color = "white", size = 0.3) +
      scale_fill_viridis_c(option = "viridis", limits = c(0.45, 0.95)) +
      geom_text(aes(label = sprintf("%.2f", Value)), color = "white", size = 3.5) +
      labs(title = m, x = "", y = "") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.text.y = element_text(size = 8),
        plot.title = element_text(hjust = 0.5, size = 10),
        legend.position = "none"
      )
  })
  
  # Watershed-specific accuracy section
  watershed_data <- data %>%
    filter(Watershed != "Overall") %>%
    select(DataType, ModelMethod, Watershed, Accuracy)
  
  # Watershed-specific heatmaps
  watershed_plots <- lapply(unique(watershed_data$Watershed), function(w) {
    ws_data <- watershed_data %>% filter(Watershed == w)
    
    ggplot(ws_data, aes(x = ModelMethod, y = DataType, fill = Accuracy)) +
      geom_tile(color = "white", size = 0.3) +
      scale_fill_viridis_c(option = "viridis", limits = c(0.45, 0.95)) +
      geom_text(aes(label = sprintf("%.2f", Accuracy)), color = "white", size = 3.5) +
      labs(title = paste(w, "Accuracy"), x = "", y = "") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.text.y = element_text(size = 8),
        plot.title = element_text(hjust = 0.5, size = 10),
        legend.position = "none"
      )
  })
  
  # Combine into a single comprehensive visualization
  main_grid <- grid.arrange(
    # First row: Overall metrics
    grid.arrange(grobs = overall_plots, ncol = 3),
    # Second row: Watershed-specific metrics
    grid.arrange(grobs = watershed_plots, ncol = 3),
    # Layout
    heights = c(1, 1),
    top = textGrob(
      "Model Performance Comparison\nEvaluation across different metrics and watershed classes",
      gp = gpar(fontsize = 14, fontface = "bold")
    )
  )
  
  return(main_grid)
}

# Create the comprehensive visualization
comprehensive_viz <- create_comprehensive_viz(viz_data)

# Save the comprehensive visualization
ggsave(
  file.path(here("Figures/ModelOutputs"), "Comprehensive_Model_Comparison.png"),
  comprehensive_viz,
  width = 14,
  height = 10,
  dpi = 300
)

# Create combined landmark model visualizations
for (landmark_name in names(landmark_combinations)) {
  landmark_data <- viz_data %>%
    filter(Landmark_Filter == landmark_name)
  
  if (nrow(landmark_data) > 0) {
    landmark_viz <- create_comprehensive_viz(landmark_data)
    
    ggsave(
      file.path(here("Figures/ModelOutputs"), paste0("Model_Comparison_", landmark_name, ".png")),
      landmark_viz,
      width = 14,
      height = 10,
      dpi = 300
    )
  }
}

# Print completion message
cat("\nComprehensive multimodel comparison completed!\n")
cat("All results saved to: Data/Model_Results/ALL_Models_Results.csv\n")
cat("Comprehensive visualization saved to: Figures/ModelOutputs/Comprehensive_Model_Comparison.png\n")
cat("Individual landmark visualizations saved to: Figures/ModelOutputs/Model_Comparison_*.png\n")