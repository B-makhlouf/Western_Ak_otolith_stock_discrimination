################################################################################
# 02_MultiModel_Comparison.R
# 
# This script performs multi-model comparison for classifying otolith timeseries data.
# It trains and evaluates Random Forest, SVM, and KNN models on different data 
# preprocessing methods (RAW, GAM, MA) and different landmark selections.
# 
# OUTPUTS:
# - Trained models saved to Models/[model_type]/[model_name].rds
# - Model evaluation metrics saved to Model_Results/[data_type]_[landmarks]_MultiModel_Results.csv
# - Performance visualization plots saved to Figures/ModelOutputs/
# - Training and testing datasets saved for model tuning
#
################################################################################

# Load necessary libraries
library(viridis)      # For color palettes
library(patchwork)    # For combining plots
library(plotly)       # For interactive plots
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
All_Metadata <- read.csv(here("/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/Data/Metadata and QC/Metadata_and_QC.csv"))

################################################################################
# Configuration parameters
################################################################################

# Define data types to iterate over
data_types <- c("GAM", "MA")

# Define landmark combinations to evaluate
landmark_configs <- list(
  c("Core"),
  c("Fw"),
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
  here("Data/Train_Test_Sets"),
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
    filename <- glue("Data/Preprocessed_ts_matrices/Processed_{landmark_str}_{data_type}.csv")
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
  train_test_filename <- glue("Data/Train_Test_Sets/{data_type}_{landmark_str}_train_test_data.rds")
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
    
    # Save detailed prediction results
    pred_filename <- glue("Model_Results/Predictions/{data_type}_{landmark_str}_{model_type}_predictions.csv")
    dir.create(dirname(pred_filename), showWarnings = FALSE, recursive = TRUE)
    write.csv(idScores, pred_filename, row.names = FALSE)
    
    # Compute confusion matrix
    conf_matrix <- confusionMatrix(idScores$Predicted, idScores$Actual)
    
    # Extract overall classification accuracy
    overall_accuracy <- conf_matrix$overall["Accuracy"]
    
    # Extract sensitivity & specificity for each class
    class_metrics <- as.data.frame(conf_matrix$byClass)
    class_metrics$Watershed <- rownames(class_metrics)
    
    # Delete the row names 
    rownames(class_metrics) <- NULL
    
    # Remove "Class:_" from class names using gsub
    class_metrics$Watershed <- gsub("Class: ", "", class_metrics$Watershed)
    
    # Combine training, testing, accuracy, and class metrics into a single dataframe
    results_df <- train_counts %>%
      full_join(test_counts, by = "Watershed") %>%
      full_join(class_metrics, by = "Watershed") %>%
      mutate(Overall_Accuracy = overall_accuracy)
    
    # Add model specifications to the results dataframe
    results_df <- results_df %>%
      mutate(
        Model_Landmarks = paste(landmark_filter, collapse = ","),
        Data_Type = data_type,
        Model_Method = model_type
      )
    
    # Append to the main results dataframe
    all_results_df <- bind_rows(all_results_df, results_df)
  }
  
  return(all_results_df)
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
      
      # Save individual configuration results
      # Initialize directories to save results
      result_filename <- glue("Model_Results/{data_type}_{landmark_str}_MultiModel_Results.csv")
      write.csv(results, result_filename, row.names = FALSE)
      logger::log_info("Saved results to {result_filename}")
      
      # Generate and save visualization plots for this configuration
      watershed_colors <- c(
        "Yukon" = "#1f77b4",    # Blue 
        "Nush" = "#ff7f0e",     # Orange
        "Kusko" = "#2ca02c"     # Green
      )
      
      # Create plot title
      landmark_title <- paste(landmark_str, data_type, sep = "_")
      
      # Sensitivity plot (recall)
      sensitivityplot <- ggplot(results, aes(x = Watershed, y = Sensitivity, fill = Watershed)) +
        geom_bar(stat = "identity", position = "dodge", alpha = .7) +
        facet_grid(. ~ Model_Method, scales = "free_y") +
        labs(
          title = glue("{landmark_title} - Sensitivity (Recall)"),
          x = "Watershed",
          y = "Sensitivity"
        ) +
        theme_grey() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          strip.text = element_text(size = 12),
          strip.background = element_blank()
        ) +
        scale_y_continuous(limits = c(0, 1)) +
        scale_fill_manual(values = watershed_colors) +
        geom_text(
          aes(label = round(Sensitivity, 2)),
          position = position_dodge(width = 0.8),
          vjust = -0.5,
          color = "black",
          size = 3
        )
      
      # Specificity plot (precision)  
      specificityplot <- ggplot(results, aes(x = Watershed, y = Specificity, fill = Watershed)) +
        geom_bar(stat = "identity", position = "dodge", alpha = .7) +
        facet_grid(. ~ Model_Method, scales = "free_y") +
        labs(
          title = glue("{landmark_title} - Specificity (Precision)"),
          x = "Watershed",
          y = "Specificity"
        ) +
        theme_grey() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          strip.text = element_text(size = 12),
          strip.background = element_blank()
        ) +
        scale_y_continuous(limits = c(0, 1)) +
        scale_fill_manual(values = watershed_colors) +
        geom_text(
          aes(label = round(Specificity, 2)),
          position = position_dodge(width = 0.8),
          vjust = -0.5,
          color = "black",
          size = 3
        )
      
      # Balanced Accuracy plot
      balanced_accuracy_plot <- ggplot(results, aes(x = Watershed, y = `Balanced Accuracy`, fill = Watershed)) +
        geom_bar(stat = "identity", position = "dodge", alpha = .7) +
        facet_grid(. ~ Model_Method, scales = "free_y") +
        labs(
          title = glue("{landmark_title} - Balanced Accuracy"),
          x = "Watershed",
          y = "Balanced Accuracy"
        ) +
        theme_grey() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          strip.text = element_text(size = 12),
          strip.background = element_blank()
        ) +
        scale_y_continuous(limits = c(0, 1)) +
        scale_fill_manual(values = watershed_colors) +
        geom_text(
          aes(label = round(`Balanced Accuracy`, 2)),
          position = position_dodge(width = 0.8),
          vjust = -0.5,
          color = "black",
          size = 3
        )
      
      # Save plots
      ggsave(glue("Figures/ModelOutputs/{landmark_title}_Sensitivity.png"), 
             sensitivityplot, width = 12, height = 6, dpi = 300)
      ggsave(glue("Figures/ModelOutputs/{landmark_title}_Specificity.png"), 
             specificityplot, width = 12, height = 6, dpi = 300)
      ggsave(glue("Figures/ModelOutputs/{landmark_title}_Balanced_Accuracy.png"), 
             balanced_accuracy_plot, width = 12, height = 6, dpi = 300)
      
      logger::log_info("Saved visualization plots for {landmark_title}")
    }
  }
}

# Save complete results from all configurations
if (nrow(all_configurations_results) > 0) {
  write.csv(all_configurations_results, "Model_Results/All_Configurations_Results.csv", row.names = FALSE)
  logger::log_info("Saved complete results to Model_Results/All_Configurations_Results.csv")
  
  # Create a summary table of best models by Balanced Accuracy
  summary_table <- all_configurations_results %>%
    group_by(Data_Type, Model_Landmarks, Model_Method) %>%
    summarize(
      Mean_Balanced_Accuracy = mean(`Balanced Accuracy`, na.rm = TRUE),
      Overall_Accuracy = first(Overall_Accuracy),
      .groups = "drop"
    ) %>%
    arrange(desc(Mean_Balanced_Accuracy))
  
  write.csv(summary_table, "Model_Results/Model_Comparison_Summary.csv", row.names = FALSE)
  logger::log_info("Saved model comparison summary to Model_Results/Model_Comparison_Summary.csv")
  
  # Create a combined visualization of the top performing models
  top_models <- summary_table %>%
    top_n(5, Mean_Balanced_Accuracy)
  
  top_config_ids <- paste(top_models$Data_Type, top_models$Model_Landmarks, top_models$Model_Method, sep = "_")
  
  top_results <- all_configurations_results %>%
    mutate(ConfigID = paste(Data_Type, Model_Landmarks, Model_Method, sep = "_")) %>%
    filter(ConfigID %in% top_config_ids)
  
  top_models_plot <- ggplot(top_results, aes(x = Watershed, y = `Balanced Accuracy`, fill = ConfigID)) +
    geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
    labs(
      title = "Top 5 Models by Mean Balanced Accuracy",
      x = "Watershed",
      y = "Balanced Accuracy",
      fill = "Model Configuration"
    ) +
    theme_grey() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    ) +
    scale_y_continuous(limits = c(0, 1))
  
  ggsave("Figures/ModelOutputs/Top_Models_Comparison.png", 
         top_models_plot, width = 12, height = 8, dpi = 300)
  logger::log_info("Saved top models comparison plot")
}

logger::log_info("Model comparison completed successfully")