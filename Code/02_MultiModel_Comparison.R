################################################################################
# 02_MultiModel_Comparison.R
# 
# This script performs multi-model comparison for classifying otolith timeseries data.
# It trains and evaluates Random Forest, SVM, and KNN models on different data 
# preprocessing methods (RAW, GAM, MA) and different landmark selections.
# 
# OUTPUTS:
# - Trained models saved to Models/[model_type]/[model_name].rds
# - Comprehensive results summary saved to Model_Results/Comprehensive_Model_Results.csv
# - A comprehensive heatmap visualization saved to Figures/ModelOutputs/Model_Performance_Heatmap.png
# - Training and testing datasets saved for model tuning
#
################################################################################

# Load necessary libraries
library(viridis)      # For color palettes
library(patchwork)    # For combining plots
library(tidyverse)    # For data manipulation
library(here)         # For file path management
library(caret)        # For model training
library(glue)         # For string interpolation
library(logger)       # For logging

# Source helper functions
source(here("Code/Helper_Code/PCA_functions.R"))
source(here("Code/Helper_Code/Raw_Data_Preprocessing.R"))

# Load metadata
logger::log_info("Loading metadata")
All_Metadata <- read.csv(here("/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/Data/01_Metadata and QC/Metadata_and_QC.csv"))

################################################################################
# Configuration parameters
################################################################################

# Define data types to iterate over
data_types <- c("GAM", "MA", "RAW")

# Define landmark combinations to evaluate
landmark_configs <- list(
  c("Core", "Fw")
)

# Define model types to test
model_types <- c("rf", "svmRadial", "knn")

# Cross-validation settings
cv_folds <- 5

# Train/test split proportion
train_proportion <- 0.8

# Random seed for reproducibility
random_seed <- 123

# Create directories for outputs if they don't exist
dirs <- c(
  here("Models/rf_models"), 
  here("Models/svmRadial_models"), 
  here("Models/knn_models"),
  here("Data/03_Train_Test_Sets"),
  here("Model_Results"),
  here("Figures/ModelOutputs")
)

for(dir in dirs) {
  if(!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
    logger::log_info("Created directory: {dir}")
  }
}

################################################################################
# Function to process a single model configuration
################################################################################

run_model_comparison <- function(data_type, landmark_filter) {
  
  landmark_str <- paste(landmark_filter, collapse = "_")
  logger::log_info("Processing {data_type} data with landmarks: {landmark_str}")
  
  # Process each combination of data type and landmark configuration
  # Load processed data for the current data_type and landmark config
  processed_data <- tryCatch({
    # Specifically look in the Preprocessed_ts_matrices directory
    filename <- glue("Data/02_Preprocessed_ts_matrices/Processed_{landmark_str}_{data_type}.csv")
    if(file.exists(here(filename))) {
      read.csv(here(filename))
    } else {
      stop(glue("File not found: {filename}"))
    }
  }, error = function(e) {
    logger::log_error("Error loading data: {e$message}")
    return(NULL)
  })
  
  if(is.null(processed_data)) {
    logger::log_error("Skipping this configuration due to data loading failure")
    return(NULL)
  }
  
  # Merge with metadata
  AnalysisDataAll <- processed_data %>%
    left_join(All_Metadata %>% select(-Year), by = c("Fish_id" = "Fish_ID")) %>%
    select((ncol(.)-12):ncol(.), everything()) %>%
    # Select only those with QC = "Yes"
    filter(QC_Grade == "Yes")
  
  # Separate metadata and isotope time series data
  Analysis_metadata <- AnalysisDataAll[,1:17]  
  Analysis_ts_data <- AnalysisDataAll[,18:ncol(AnalysisDataAll)]  
  
  # Ensure selected_data is a dataframe and add Watershed
  ModelData <- Analysis_ts_data %>% 
    as.data.frame() %>% 
    mutate(Watershed = Analysis_metadata$Watershed)
  
  # Split data into training (80%) and testing (20%)
  set.seed(random_seed)
  trainIndex <- createDataPartition(ModelData$Watershed, p = train_proportion, list = FALSE)
  traindata <- ModelData[trainIndex, ]
  testdata <- ModelData[-trainIndex, ]
  
  # Save train and test datasets for later tuning
  train_test_filename <- glue("Data/03_Train_Test_Sets/{data_type}_{landmark_str}_train_test_data.rds")
  saveRDS(list(train = traindata, test = testdata, 
               train_metadata = Analysis_metadata[trainIndex, ],
               test_metadata = Analysis_metadata[-trainIndex, ]),
          train_test_filename)
  logger::log_info("Saved train/test data to {train_test_filename}")
  
  # Count number of samples for each Watershed in training and testing sets
  train_counts <- as.data.frame(table(traindata$Watershed))
  colnames(train_counts) <- c("Watershed", "Train_Count")
  
  test_counts <- as.data.frame(table(testdata$Watershed))
  colnames(test_counts) <- c("Watershed", "Test_Count")
  
  # Set up cross-validation
  control <- trainControl(method = "cv", number = cv_folds, classProbs = TRUE)  
  
  # Initialize results dataframe for this configuration
  all_results_df <- data.frame()
  
  for (model_type in model_types) {
    logger::log_info("Training {model_type} model")
    
    # Train the model
    model <- train(Watershed ~ ., data = traindata, method = model_type, trControl = control)
    
    # Save the trained model
    model_filename <- glue("Models/{model_type}_models/{model_type}_{data_type}_{landmark_str}.rds")
    saveRDS(model, model_filename)
    logger::log_info("Saved model to {model_filename}")
    
    # Make predictions (both class labels and probabilities)
    predictions <- predict(model, testdata)
    probabilities <- predict(model, testdata, type = "prob")
    
    # Extract IDs for test samples and performance metrics
    idScores <- Analysis_metadata[-trainIndex, ] %>%
      select(Fish_id) %>%
      mutate(
        Predicted = predictions,
        Actual = testdata$Watershed,
        Confidence = apply(probabilities, 1, max),
        Correct = Predicted == Actual
      ) %>%
      bind_cols(probabilities)  # Add probability columns
    
    # Convert factors
    idScores <- idScores %>%
      mutate(Predicted = as.factor(Predicted), Actual = as.factor(Actual))
    
    # Compute confusion matrix
    conf_matrix <- confusionMatrix(idScores$Predicted, idScores$Actual)
    
    # Extract overall classification accuracy
    overall_accuracy <- conf_matrix$overall["Accuracy"]
    
    # Extract sensitivity & specificity for each class
    class_metrics <- as.data.frame(conf_matrix$byClass)
    class_metrics$Watershed <- rownames(class_metrics)
    
    # Delete the row names 
    rownames(class_metrics) <- NULL
    
    # Remove "Class: " from class names using gsub
    class_metrics$Watershed <- gsub("Class: ", "", class_metrics$Watershed)
    
    # Calculate F1 score for each class
    class_metrics$F1 <- with(class_metrics, 2 * Sensitivity * `Pos Pred Value` / (Sensitivity + `Pos Pred Value`))
    
    # Combine training, testing, accuracy, and class metrics into a single dataframe
    results_df <- train_counts %>%
      full_join(test_counts, by = "Watershed") %>%
      full_join(class_metrics, by = "Watershed") %>%
      mutate(
        Overall_Accuracy = overall_accuracy,
        # Add configuration identifiers for the summary CSV
        Data_Type = data_type,
        Model_Method = model_type,
        Landmark_Config = paste(landmark_filter, collapse = "_"),
        Config_ID = paste(data_type, paste(landmark_filter, collapse = "_"), model_type, sep = "_")
      )
    
    # Append to the main results dataframe
    all_results_df <- bind_rows(all_results_df, results_df)
  }
  
  return(all_results_df)
}

################################################################################
# Function to create a comprehensive performance heatmap
################################################################################

create_performance_heatmap <- function(results_df) {
  # Clean up data for visualization
  heatmap_data <- results_df %>%
    mutate(
      # Create a nice label for the data type + landmarks
      DataConfig = case_when(
        Data_Type == "RAW" & Landmark_Config == "Core_Fw" ~ "Sr88",
        Data_Type == "MA" & Landmark_Config == "Core_Fw" ~ "Sr8786_MA",
        Data_Type == "GAM" & Landmark_Config == "Core_Fw" ~ "Sr8786_GAM",
        TRUE ~ paste(Data_Type, Landmark_Config, sep="_")
      )
    )
  
  # Format labels properly - make sure to vectorize this function
  label_formatter <- function(value) {
    sapply(value, function(x) {
      if(is.na(x)) return("")
      return(sprintf("%.2f", x))
    })
  }
  
  # Prepare the overall metrics section
  overall_metrics <- heatmap_data %>%
    group_by(DataConfig, Model_Method) %>%
    summarize(
      `Overall Accuracy` = first(Overall_Accuracy),
      `Overall F1 Score` = mean(F1, na.rm = TRUE),
      `Overall Specificity` = mean(Specificity, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Create data frames for each metric to plot as separate heatmaps
  accuracy_data <- overall_metrics %>% 
    select(DataConfig, Model_Method, `Overall Accuracy`) %>%
    rename(Value = `Overall Accuracy`)
  
  f1_data <- overall_metrics %>% 
    select(DataConfig, Model_Method, `Overall F1 Score`) %>%
    rename(Value = `Overall F1 Score`)
  
  specificity_data <- overall_metrics %>% 
    select(DataConfig, Model_Method, `Overall Specificity`) %>%
    rename(Value = `Overall Specificity`)
  
  # Create dataframes for watershed-specific metrics
  nushagak_data <- heatmap_data %>%
    filter(Watershed == "Nush") %>%
    select(DataConfig, Model_Method, Sensitivity) %>%
    rename(Value = Sensitivity)
  
  kuskokwim_data <- heatmap_data %>%
    filter(Watershed == "Kusko") %>%
    select(DataConfig, Model_Method, Sensitivity) %>%
    rename(Value = Sensitivity)
  
  yukon_data <- heatmap_data %>%
    filter(Watershed == "Yukon") %>%
    select(DataConfig, Model_Method, Sensitivity) %>%
    rename(Value = Sensitivity)
  
  # Create the heatmap plots
  p_accuracy <- ggplot(accuracy_data, aes(x = Model_Method, y = DataConfig, fill = Value)) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_text(aes(label = label_formatter(Value)), color = "black", size = 3.5) +
    scale_fill_viridis_c(option = "viridis", limits = c(0, 1)) +
    labs(title = "Overall Accuracy") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.title = element_blank(),
      legend.position = "none",
      panel.grid = element_blank(),
      plot.title = element_text(size = 10, face = "bold", hjust = 0.5)
    )
  
  p_f1 <- ggplot(f1_data, aes(x = Model_Method, y = DataConfig, fill = Value)) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_text(aes(label = label_formatter(Value)), color = "black", size = 3.5) +
    scale_fill_viridis_c(option = "viridis", limits = c(0, 1)) +
    labs(title = "Overall F1 Score") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.title = element_blank(),
      legend.position = "none",
      panel.grid = element_blank(),
      plot.title = element_text(size = 10, face = "bold", hjust = 0.5)
    )
  
  p_specificity <- ggplot(specificity_data, aes(x = Model_Method, y = DataConfig, fill = Value)) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_text(aes(label = label_formatter(Value)), color = "black", size = 3.5) +
    scale_fill_viridis_c(option = "viridis", limits = c(0, 1)) +
    labs(title = "Overall Specificity") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.title = element_blank(),
      legend.position = "none",
      panel.grid = element_blank(),
      plot.title = element_text(size = 10, face = "bold", hjust = 0.5)
    )
  
  p_nushagak <- ggplot(nushagak_data, aes(x = Model_Method, y = DataConfig, fill = Value)) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_text(aes(label = label_formatter(Value)), color = "black", size = 3.5) +
    scale_fill_viridis_c(option = "viridis", limits = c(0, 1)) +
    labs(title = "Nushagak Accuracy") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.title = element_blank(),
      legend.position = "none",
      panel.grid = element_blank(),
      plot.title = element_text(size = 10, face = "bold", hjust = 0.5)
    )
  
  p_kuskokwim <- ggplot(kuskokwim_data, aes(x = Model_Method, y = DataConfig, fill = Value)) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_text(aes(label = label_formatter(Value)), color = "black", size = 3.5) +
    scale_fill_viridis_c(option = "viridis", limits = c(0, 1)) +
    labs(title = "Kuskokwim Accuracy") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.title = element_blank(),
      legend.position = "none",
      panel.grid = element_blank(),
      plot.title = element_text(size = 10, face = "bold", hjust = 0.5)
    )
  
  p_yukon <- ggplot(yukon_data, aes(x = Model_Method, y = DataConfig, fill = Value)) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_text(aes(label = label_formatter(Value)), color = "black", size = 3.5) +
    scale_fill_viridis_c(option = "viridis", limits = c(0, 1)) +
    labs(title = "Yukon Accuracy") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.title = element_blank(),
      legend.position = "none",
      panel.grid = element_blank(),
      plot.title = element_text(size = 10, face = "bold", hjust = 0.5)
    )
  
  # Combine plots into a grid
  top_row <- p_accuracy + p_f1 + p_specificity + plot_layout(ncol = 3)
  bottom_row <- p_nushagak + p_kuskokwim + p_yukon + plot_layout(ncol = 3)
  
  combined_plot <- top_row / bottom_row +
    plot_annotation(
      title = "Model Performance Comparison",
      subtitle = "Evaluation across different metrics and watershed classes",
      theme = theme(
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 10, hjust = 0.5)
      )
    )
  
  return(combined_plot)
}

################################################################################
# Main execution loop
################################################################################

# Initialize a dataframe to store all results across configurations
all_configurations_results <- data.frame()

# Process each combination of data type and landmark configuration
for (data_type in data_types) {
  for (landmark_filter in landmark_configs) {
    
    landmark_str <- paste(landmark_filter, collapse = "_")
    logger::log_info("Processing configuration: {data_type}, landmarks: {landmark_str}")
    
    # Run model comparison for this configuration
    results <- run_model_comparison(data_type, landmark_filter)
    
    if (!is.null(results)) {
      # Append results to the complete results dataframe
      all_configurations_results <- bind_rows(all_configurations_results, results)
    }
  }
}

# Save complete results to a single comprehensive CSV
if (nrow(all_configurations_results) > 0) {
  # Save the comprehensive results file with all configurations
  write.csv(all_configurations_results, "Model_Results/Comprehensive_Model_Results.csv", row.names = FALSE)
  logger::log_info("Saved comprehensive results to Model_Results/Comprehensive_Model_Results.csv")
  
  # Create two versions of the results table for easier analysis
  
  # 1. Overall metrics by configuration
  overall_metrics <- all_configurations_results %>%
    group_by(Data_Type, Landmark_Config, Model_Method) %>%
    summarize(
      Overall_Accuracy = first(Overall_Accuracy),
      Mean_Balanced_Accuracy = mean(`Balanced Accuracy`, na.rm = TRUE),
      Mean_F1 = mean(F1, na.rm = TRUE),
      Mean_Specificity = mean(Specificity, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(Overall_Accuracy))
  
  write.csv(overall_metrics, "Model_Results/Overall_Metrics_Summary.csv", row.names = FALSE)
  
  # 2. Class-specific metrics by configuration
  class_metrics <- all_configurations_results %>%
    select(Data_Type, Landmark_Config, Model_Method, Watershed, 
           Sensitivity, Specificity, `Pos Pred Value`, F1) %>%
    arrange(Data_Type, Landmark_Config, Model_Method, Watershed)
  
  write.csv(class_metrics, "Model_Results/Class_Metrics_Summary.csv", row.names = FALSE)
  
  # Create and save the comprehensive performance heatmap
  perf_heatmap <- create_performance_heatmap(all_configurations_results)
  
  ggsave("Figures/ModelOutputs/Model_Performance_Heatmap.png", 
         perf_heatmap, width = 12, height = 8, dpi = 300)
  logger::log_info("Saved performance heatmap to Figures/ModelOutputs/Model_Performance_Heatmap.png")
}

logger::log_info("Model comparison completed successfully")