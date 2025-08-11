# Updated Modeling Script with Feature Importance Collection
# This script modifies your existing modeling pipeline to collect feature importance

library(here)
library(dplyr)
library(tidymodels)
library(ranger)
library(kernlab)
library(kknn)

################################################################################
#### CONFIGURATION
################################################################################

# Filter threshold
NATAL_ISO_THRESHOLD <- 0.713

# Set seed for reproducibility
set.seed(123)

# Define data types
data_types <- c("RAW", "GAM", "MA", "Sr88", "Combined")

# Define paths
base_data_path <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/data/LA_Data/Preprocessed_ts_matrices"

# Output directories
train_test_dir_total <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/data/LA_Data/TrainingTesting"
train_test_dir_overlap <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/data/LA_Data/TrainingTesting/Filtered"
models_dir_total <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/Output/Models/Total"
models_dir_overlap <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/Output/Models/Filtered"

# NEW: Feature importance output directory
importance_dir <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/Output/FeatureImportance"

# Create directories if they don't exist
dir.create(train_test_dir_total, recursive = TRUE, showWarnings = FALSE)
dir.create(train_test_dir_overlap, recursive = TRUE, showWarnings = FALSE)
dir.create(models_dir_total, recursive = TRUE, showWarnings = FALSE)
dir.create(models_dir_overlap, recursive = TRUE, showWarnings = FALSE)
dir.create(importance_dir, recursive = TRUE, showWarnings = FALSE)

# Define metadata columns to exclude from modeling
metadata_columns <- c("Fish_id", "Year", "Natal_Start", "Marine_Start", 
                      "Marine_End", "Interpolated_Points", "Micron_Size", "Natal_Iso", "Original_Data_Points")

################################################################################
#### STEP 1: Create train/test splits and datasets (SAME AS BEFORE)
################################################################################

# Load all datasets WITHOUT filtering to determine splits
all_data_unfiltered <- list()
for (data_type in data_types) {
  file_path <- file.path(base_data_path, paste0("NatalToMarine_Processed_", data_type, ".csv"))
  
  if (file.exists(file_path)) {
    data <- read.csv(file_path) %>%
      mutate(Watershed = as.factor(Watershed))
    
    all_data_unfiltered[[data_type]] <- data
  }
}

# Get common fish IDs across all datasets (unfiltered)
fish_ids_unfiltered <- lapply(all_data_unfiltered, function(x) x$Fish_id)
common_fish_ids <- Reduce(intersect, fish_ids_unfiltered)

# Create train/test split based on Fish_id
unique_fish_ids <- unique(common_fish_ids)
train_fish_ids <- sample(unique_fish_ids, size = 0.8 * length(unique_fish_ids))
test_fish_ids <- setdiff(unique_fish_ids, train_fish_ids)

cat("Created train/test splits:\n")
cat("Train Fish IDs:", length(train_fish_ids), "\n")
cat("Test Fish IDs:", length(test_fish_ids), "\n")

# Process each dataset to create train/test files (SAME AS BEFORE)
for (data_type in data_types) {
  file_path <- file.path(base_data_path, paste0("NatalToMarine_Processed_", data_type, ".csv"))
  
  if (file.exists(file_path)) {
    # Load data (unfiltered)
    data <- read.csv(file_path) %>%
      mutate(Watershed = as.factor(Watershed)) %>%
      filter(Fish_id %in% common_fish_ids)
    
    # Split data using Fish_id splits
    train_data <- data[data$Fish_id %in% train_fish_ids, ]
    test_data_full <- data[data$Fish_id %in% test_fish_ids, ]
    
    # Create filtered test data (same fish IDs, but filtered values)
    test_data_filtered <- test_data_full %>%
      filter(Natal_Iso < NATAL_ISO_THRESHOLD)
    
    # Remove metadata columns - keep ONLY Watershed (target) and predictors
    train_clean <- train_data %>%
      select(-all_of(metadata_columns))
    
    test_full_clean <- test_data_full %>%
      select(-all_of(metadata_columns))
    
    test_filtered_clean <- test_data_filtered %>%
      select(-all_of(metadata_columns))
    
    # Save TOTAL analysis files (same training, full test set)
    write.csv(train_clean, file.path(train_test_dir_total, paste0("Train_", data_type, ".csv")), row.names = FALSE)
    write.csv(test_full_clean, file.path(train_test_dir_total, paste0("Test_", data_type, ".csv")), row.names = FALSE)
    
    # Save OVERLAP analysis files (same training, filtered test set)
    write.csv(train_clean, file.path(train_test_dir_overlap, paste0("Train_", data_type, ".csv")), row.names = FALSE)
    write.csv(test_filtered_clean, file.path(train_test_dir_overlap, paste0("Test_", data_type, ".csv")), row.names = FALSE)
  }
}

# Save Fish_id splits for reference
fish_id_splits <- data.frame(
  Fish_id = c(train_fish_ids, test_fish_ids),
  Split = c(rep("Train", length(train_fish_ids)), rep("Test", length(test_fish_ids)))
)

write.csv(fish_id_splits, file.path(train_test_dir_total, "Fish_ID_Splits.csv"), row.names = FALSE)
write.csv(fish_id_splits, file.path(train_test_dir_overlap, "Fish_ID_Splits.csv"), row.names = FALSE)

################################################################################
#### MODIFIED FUNCTION TO RUN ANALYSIS WITH FEATURE IMPORTANCE
################################################################################

run_analysis_with_importance <- function(train_test_dir, models_dir, analysis_name, results_dir) {
  
  # Create results data frame
  results <- data.frame()
  
  # Create results directory if it doesn't exist
  dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Loop through each dataset and model
  for (data_type in data_types) {
    
    # Load data
    train_data <- read.csv(file.path(train_test_dir, paste0("Train_", data_type, ".csv"))) %>%
      mutate(Watershed = as.factor(Watershed))
    test_data <- read.csv(file.path(train_test_dir, paste0("Test_", data_type, ".csv"))) %>%
      mutate(Watershed = as.factor(Watershed))
    
    # Skip if no test data after filtering
    if (nrow(test_data) == 0) {
      cat("Skipping", data_type, "- no test data after filtering\n")
      next
    }
    
    cat("Processing", data_type, "- Train:", nrow(train_data), "Test:", nrow(test_data), "\n")
    
    base_recipe <- recipe(Watershed ~ ., data = train_data)
    
    # Define models with proper mtry for classification
    n_predictors <- ncol(train_data) - 1
    
    # MODIFIED: Define Random Forest with importance enabled
    models <- list(
      RF = rand_forest(trees = 500, mtry = floor(sqrt(n_predictors))) %>% 
        set_engine("ranger", importance = "impurity") %>%  # ENABLE IMPORTANCE
        set_mode("classification"),
      SVM = svm_rbf() %>% set_engine("kernlab") %>% set_mode("classification"),
      KNN = nearest_neighbor(neighbors = 5) %>% set_engine("kknn") %>% set_mode("classification")
    )
    
    # Train and evaluate each model
    for (model_name in names(models)) {
      
      # Set seed before each model for reproducibility
      set.seed(123)
      
      # Fit model on training data
      workflow_obj <- workflow() %>%
        add_recipe(base_recipe) %>%
        add_model(models[[model_name]]) %>%
        fit(train_data)
      
      # Save the trained model as RDS
      model_filename <- paste0(data_type, "_", model_name, "_model.rds")
      model_filepath <- file.path(models_dir, model_filename)
      saveRDS(workflow_obj, model_filepath)
      
      # NEW: Extract and save feature importance for Random Forest models
      if (model_name == "RF") {
        tryCatch({
          # Extract the ranger model from the workflow
          ranger_model <- workflow_obj$fit$fit$fit
          
          # Get variable importance
          if (!is.null(ranger_model$variable.importance)) {
            variable_importance <- ranger_model$variable.importance
            
            # Convert to data frame
            importance_df <- data.frame(
              Feature = names(variable_importance),
              Importance = as.numeric(variable_importance),
              Dataset = data_type,
              Analysis = analysis_name
            ) %>%
              arrange(desc(Importance)) %>%
              mutate(
                # Extract time point index from feature name
                Time_Point = as.numeric(gsub("X", "", Feature)),
                # Normalize importance to 0-1 scale
                Importance_Normalized = (Importance - min(Importance)) / (max(Importance) - min(Importance)),
                # Calculate rank
                Importance_Rank = rank(-Importance)
              )
            
            # Save feature importance
            importance_filename <- paste0(data_type, "_RF_", analysis_name, "_feature_importance.csv")
            importance_filepath <- file.path(importance_dir, importance_filename)
            write.csv(importance_df, importance_filepath, row.names = FALSE)
            
            cat("✓ Feature importance saved for", data_type, "RF model\n")
            
            # Print top 5 most important features
            cat("  Top 5 features:", paste(head(importance_df$Feature, 5), collapse = ", "), "\n")
          } else {
            cat("⚠ Warning: No feature importance available for", data_type, "RF model\n")
          }
        }, error = function(e) {
          cat("✗ Error extracting feature importance for", data_type, "RF:", e$message, "\n")
        })
      }
      
      # Make predictions on test data
      predictions <- workflow_obj %>%
        predict(test_data) %>%
        bind_cols(test_data %>% select(Watershed))
      
      # Get prediction probabilities
      pred_probs <- workflow_obj %>%
        predict(test_data, type = "prob")
      
      # Combine predictions and probabilities with metadata
      predictions_with_probs <- predictions %>%
        bind_cols(pred_probs) %>%
        mutate(
          Dataset = data_type,
          Model = model_name,
          Correct = Watershed == .pred_class
        )
      
      # Save predictions and probabilities
      pred_filename <- paste0(data_type, "_", model_name, "_", analysis_name, "_predictions.csv")
      pred_filepath <- file.path(results_dir, pred_filename)
      write.csv(predictions_with_probs, pred_filepath, row.names = FALSE)
      
      # Calculate metrics
      accuracy <- mean(predictions$Watershed == predictions$.pred_class)
      f1_score <- predictions %>%
        f_meas(truth = Watershed, estimate = .pred_class) %>%
        pull(.estimate)
      
      # Store results
      results <- rbind(results, data.frame(
        Dataset = data_type,
        Model = model_name,
        Accuracy = round(accuracy, 3),
        F1_Score = round(f1_score, 3)
      ))
    }
  }
  
  # Display results sorted by accuracy
  results <- results[order(-results$Accuracy), ]
  cat("\n=== Results for", analysis_name, "analysis ===\n")
  print(results)
  
  return(results)
}

################################################################################
#### RUN BOTH ANALYSES WITH FEATURE IMPORTANCE
################################################################################

# Define results directories
results_dir_total <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/Output/ModelResultsPreCal/Total"
results_dir_overlap <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/Output/ModelResultsPreCal/Filtered"

cat("=== Running TOTAL analysis with feature importance ===\n")
results_total <- run_analysis_with_importance(train_test_dir_total, models_dir_total, "TOTAL", results_dir_total)

cat("\n=== Running OVERLAP analysis with feature importance ===\n")
results_overlap <- run_analysis_with_importance(train_test_dir_overlap, models_dir_overlap, "OVERLAP", results_dir_overlap)

################################################################################
#### CREATE SUMMARY OF FEATURE IMPORTANCE RESULTS
################################################################################

cat("\n=== Creating feature importance summary ===\n")

# List all feature importance files
importance_files <- list.files(importance_dir, pattern = "feature_importance.csv", full.names = TRUE)

if (length(importance_files) > 0) {
  # Load and combine all feature importance results
  all_importance <- data.frame()
  
  for (file in importance_files) {
    importance_data <- read.csv(file)
    all_importance <- rbind(all_importance, importance_data)
  }
  
  # Save combined feature importance
  write.csv(all_importance, file.path(importance_dir, "All_RF_Feature_Importance.csv"), row.names = FALSE)
  
  # Create summary by dataset
  importance_summary <- all_importance %>%
    group_by(Dataset, Analysis) %>%
    summarise(
      Total_Features = n(),
      Mean_Importance = mean(Importance),
      Max_Importance = max(Importance),
      Min_Importance = min(Importance),
      Most_Important_Feature = Feature[which.max(Importance)],
      Most_Important_Time_Point = Time_Point[which.max(Importance)],
      .groups = "drop"
    )
  
  write.csv(importance_summary, file.path(importance_dir, "Feature_Importance_Summary.csv"), row.names = FALSE)
  
  cat("✓ Feature importance summary created\n")
  print(importance_summary)
} else {
  cat("⚠ No feature importance files found\n")
}

################################################################################
#### CREATE HEATMAPS (SAME AS BEFORE)
################################################################################

library(ggplot2)
library(viridis)
library(scales)

# Create output directory for figures
figures_dir <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/Figures"
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)

# Function to create heatmap (SAME AS BEFORE)
create_heatmap <- function(results, title, filename) {
  
  # Prepare data for heatmap
  results_formatted <- results %>%
    mutate(
      Dataset = factor(Dataset, levels = c("RAW", "GAM", "MA", "Sr88", "Combined")),
      Model = factor(Model, levels = c("RF", "SVM", "KNN"),
                     labels = c("Random Forest", "SVM", "KNN"))
    )
  
  # Create heatmap
  p <- ggplot(results_formatted, aes(x = Model, y = Dataset, fill = Accuracy)) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_text(aes(label = sprintf("%.3f", Accuracy)), 
              color = "black", size = 4, fontface = "bold") +
    scale_fill_gradientn(
      colors = c("dodgerblue4", "dodgerblue", "yellow", "orange", "firebrick"),
      values = scales::rescale(c(min(results_formatted$Accuracy), 
                                 min(results_formatted$Accuracy) + 0.25 * (max(results_formatted$Accuracy) - min(results_formatted$Accuracy)), 
                                 mean(range(results_formatted$Accuracy)), 
                                 max(results_formatted$Accuracy) - 0.25 * (max(results_formatted$Accuracy) - min(results_formatted$Accuracy)), 
                                 max(results_formatted$Accuracy))),
      limits = c(min(results_formatted$Accuracy) * 0.99, max(results_formatted$Accuracy) * 1.01),
      labels = scales::percent_format(accuracy = 0.1)
    ) +
    labs(
      title = title,
      x = "Model Type",
      y = "Data Source",
      fill = "Accuracy"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
      axis.title = element_text(face = "bold", size = 14),
      axis.text = element_text(size = 12),
      panel.grid = element_blank(),
      legend.position = "right",
      legend.key.height = unit(1.5, "cm"),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 10)
    )
  
  # Save heatmap
  ggsave(file.path(figures_dir, filename), p, width = 10, height = 8, dpi = 300)
  
  return(p)
}

# Create heatmaps for both analyses
if (nrow(results_total) > 0) {
  create_heatmap(results_total, "Classification Accuracy - TOTAL Analysis (All Data Types)", "heatmap_total_all.png")
}

if (nrow(results_overlap) > 0) {
  create_heatmap(results_overlap, "Classification Accuracy - OVERLAP Analysis (All Data Types)", "heatmap_overlap_all.png")
}

cat("\n=== Analysis Complete ===\n")
cat("Models saved to:", models_dir_total, "and", models_dir_overlap, "\n")
cat("Feature importance saved to:", importance_dir, "\n")
cat("Results saved to:", results_dir_total, "and", results_dir_overlap, "\n")
cat("Figures saved to:", figures_dir, "\n")