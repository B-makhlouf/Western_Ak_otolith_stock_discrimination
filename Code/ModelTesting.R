# Load required libraries
library(here)
library(dplyr)
library(tidymodels)
library(ranger)
library(kernlab)
library(kknn)
library(ggplot2)
library(viridis)
library(patchwork)

################################################################################
#### CONFIGURATION - Set toggles here
################################################################################

# Set this to TRUE to filter for natal_iso < 0.713, FALSE for original analysis
FILTER_NATAL_ISO <- TRUE  # Change to TRUE to apply natal_iso filter

# Filter threshold (only used when FILTER_NATAL_ISO = TRUE)
NATAL_ISO_THRESHOLD <- 0.713

# Set this to TRUE to normalize predictors, FALSE to use raw values
NORMALIZE_PREDICTORS <- FALSE  # Change to TRUE to apply normalization

# Cross-validation settings
CV_FOLDS <- 5  # Number of cross-validation folds
CV_REPEATS <- 3  # Number of repeated CV rounds

################################################################################
########## STEP 1 
################################################################################
#### Create test/train splits from each of the datasets, keeping the same fishIDs

# Set seed for reproducibility
set.seed(123)

# Define data types
data_types <- c("RAW", "GAM", "MA", "Sr88", "Combined")

# Define paths
base_data_path <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/data/LA_Data/Preprocessed_ts_matrices"

# FIXED: Figures directory is now always the same regardless of filtering
figures_dir <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/Figures"

# Create different output directories based on filter setting (but NOT figures)
if (FILTER_NATAL_ISO) {
  train_test_dir <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/data/LA_Data/Train_Test_Filtered"
  models_dir <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/data/Models/Filtered"
  results_dir <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/data/Results/Filtered"
  cat("RUNNING WITH NATAL_ISO FILTER: natal_iso <", NATAL_ISO_THRESHOLD, "\n")
  cat("Normalization:", ifelse(NORMALIZE_PREDICTORS, "ENABLED", "DISABLED"), "\n")
} else {
  train_test_dir <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/data/LA_Data/Train_Test"
  models_dir <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/data/Models"
  results_dir <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/data/Results"
  cat("RUNNING WITHOUT NATAL_ISO FILTER (original analysis)\n")
  cat("Normalization:", ifelse(NORMALIZE_PREDICTORS, "ENABLED", "DISABLED"), "\n")
}

# Create output directories if they don't exist
for (dir_path in c(train_test_dir, models_dir, results_dir, figures_dir)) {
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
    cat("Created directory:", dir_path, "\n")
  }
}

# Define metadata columns to exclude from modeling
metadata_columns <- c("Fish_id", "Year", "Natal_Iso", "Natal_Start", "Marine_Start", 
                      "Marine_End", "Original_Data_Points", "Interpolated_Points")

# Load all datasets and get common fish IDs
all_data <- list()
for (data_type in data_types) {
  file_path <- file.path(base_data_path, paste0("NatalToMarine_Processed_", data_type, ".csv"))
  
  if (file.exists(file_path)) {
    data <- read.csv(file_path)
    data$Watershed <- as.factor(data$Watershed)
    
    # Apply natal_iso filter if enabled
    if (FILTER_NATAL_ISO) {
      original_count <- nrow(data)
      data <- data[data$Natal_Iso < NATAL_ISO_THRESHOLD, ]
      filtered_count <- nrow(data)
      cat(paste("Loaded", data_type, ":", original_count, "samples ->", filtered_count, "after natal_iso filter\n"))
    } else {
      cat(paste("Loaded", data_type, ":", nrow(data), "samples\n"))
    }
    
    all_data[[data_type]] <- data
  } else {
    cat(paste("File not found:", file_path, "\n"))
  }
}

# Get common fish IDs across all datasets
fish_ids <- lapply(all_data, function(x) x$Fish_id)
common_fish_ids <- Reduce(intersect, fish_ids)

# Create train/test split based on Fish_id
unique_fish_ids <- unique(common_fish_ids)
train_fish_ids <- sample(unique_fish_ids, size = 0.8 * length(unique_fish_ids))
test_fish_ids <- setdiff(unique_fish_ids, train_fish_ids)

cat(paste("Train/test split:", length(train_fish_ids), "training fish,", length(test_fish_ids), "testing fish\n"))

# Loop through each dataset and save train/test splits
for (data_type in names(all_data)) {
  cat(paste("Processing", data_type, "...\n"))
  
  # Filter to common fish IDs
  data <- all_data[[data_type]]
  data <- data[data$Fish_id %in% common_fish_ids, ]
  
  # Split data by Fish_id
  train_data <- data[data$Fish_id %in% train_fish_ids, ]
  test_data <- data[data$Fish_id %in% test_fish_ids, ]
  
  # Remove metadata columns - keep ONLY Watershed (target) and predictors
  train_clean <- train_data[, !names(train_data) %in% metadata_columns]
  test_clean <- test_data[, !names(test_data) %in% metadata_columns]
  
  # Save files
  train_filename <- file.path(train_test_dir, paste0("Train_", data_type, ".csv"))
  test_filename <- file.path(train_test_dir, paste0("Test_", data_type, ".csv"))
  
  write.csv(train_clean, train_filename, row.names = FALSE)
  write.csv(test_clean, test_filename, row.names = FALSE)
  
  cat(paste("Saved:", basename(train_filename), "(", nrow(train_clean), "samples,", ncol(train_clean)-1, "predictors )\n"))
  cat(paste("Saved:", basename(test_filename), "(", nrow(test_clean), "samples,", ncol(test_clean)-1, "predictors )\n"))
}

# Save Fish_id splits for reference
fish_id_splits <- data.frame(
  Fish_id = c(train_fish_ids, test_fish_ids),
  Split = c(rep("Train", length(train_fish_ids)), rep("Test", length(test_fish_ids)))
)

write.csv(fish_id_splits, file.path(train_test_dir, "Fish_ID_Splits.csv"), row.names = FALSE)
cat(paste("\nAll train/test sets saved to:", train_test_dir, "\n"))

################################################################################
########## STEP 2: Cross-Validation and Model Training
################################################################################

# Set seed for reproducibility
set.seed(123)

# Create results data frame for cross-validation
cv_results <- data.frame()
final_test_results <- data.frame()

# Loop through each dataset
for (data_type in data_types) {
  
  cat(paste("\n", rep("=", 50), "\n"))
  cat(paste("Processing dataset:", data_type, "\n"))
  cat(rep("=", 50), "\n")
  
  # Load data
  train_data <- read.csv(file.path(train_test_dir, paste0("Train_", data_type, ".csv")))
  train_data$Watershed <- as.factor(train_data$Watershed)
  test_data <- read.csv(file.path(train_test_dir, paste0("Test_", data_type, ".csv")))
  test_data$Watershed <- as.factor(test_data$Watershed)
  
  # Calculate number of predictors (excluding target variable)
  n_predictors <- ncol(train_data) - 1
  
  # Create recipe with optional normalization
  if (NORMALIZE_PREDICTORS) {
    base_recipe <- recipe(Watershed ~ ., data = train_data) %>%
      step_normalize(all_predictors())
    cat(paste("Using normalization for", data_type, "\n"))
  } else {
    base_recipe <- recipe(Watershed ~ ., data = train_data)
    cat(paste("No normalization for", data_type, "\n"))
  }
  
  # Define models
  models <- list(
    RF = rand_forest(trees = 500, mtry = floor(sqrt(n_predictors))) %>% 
      set_engine("ranger") %>% set_mode("classification"),
    SVM = svm_rbf() %>% set_engine("kernlab") %>% set_mode("classification"),
    KNN = nearest_neighbor(neighbors = 5) %>% set_engine("kknn") %>% set_mode("classification")
  )
  
  # Perform repeated cross-validation for each model
  for (model_name in names(models)) {
    
    cat(paste("Running cross-validation for", model_name, "...\n"))
    
    # Perform repeated cross-validation
    for (repeat_idx in 1:CV_REPEATS) {
      
      # Create cross-validation folds (stratified by Watershed)
      cv_folds <- vfold_cv(train_data, v = CV_FOLDS, strata = Watershed)
      
      # Create workflow
      workflow_obj <- workflow() %>%
        add_recipe(base_recipe) %>%
        add_model(models[[model_name]])
      
      # Perform cross-validation
      cv_res <- workflow_obj %>%
        fit_resamples(resamples = cv_folds)
      
      # Extract metrics
      cv_metrics <- collect_metrics(cv_res, summarize = FALSE)
      
      # Store detailed CV results
      cv_accuracy <- cv_metrics[cv_metrics$.metric == "accuracy", ]
      
      for (i in 1:nrow(cv_accuracy)) {
        cv_results <- rbind(cv_results, data.frame(
          Dataset = data_type,
          Model = model_name,
          Repeat = repeat_idx,
          Fold = i,
          Accuracy = cv_accuracy$.estimate[i]
        ))
      }
    }
    
    # Train final model on full training set
    final_workflow <- workflow() %>%
      add_recipe(base_recipe) %>%
      add_model(models[[model_name]]) %>%
      fit(train_data)
    
    # Save the trained model
    model_filename <- paste0(data_type, "_", model_name, "_model.rds")
    model_filepath <- file.path(models_dir, model_filename)
    saveRDS(final_workflow, model_filepath)
    
    # Evaluate on test set
    test_predictions <- final_workflow %>%
      predict(test_data) %>%
      bind_cols(test_data[, "Watershed", drop = FALSE])
    
    # Calculate test metrics
    test_accuracy <- mean(test_predictions$Watershed == test_predictions$.pred_class)
    
    # Store test results
    final_test_results <- rbind(final_test_results, data.frame(
      Dataset = data_type,
      Model = model_name,
      Test_Accuracy = round(test_accuracy, 3)
    ))
    
    cat(paste("Completed", model_name, "- Test Accuracy:", round(test_accuracy, 3), "\n"))
  }
}

################################################################################
########## STEP 3: Results Analysis and Visualization
################################################################################

# Calculate CV summary statistics
cv_summary <- aggregate(Accuracy ~ Dataset + Model, data = cv_results, 
                        FUN = function(x) c(Mean = round(mean(x), 3), SD = round(sd(x), 3)))
cv_summary <- do.call(data.frame, cv_summary)

# Display results
cat("\n", rep("=", 60), "\n")
cat("CROSS-VALIDATION SUMMARY RESULTS\n")
cat(rep("=", 60), "\n")
print(cv_summary)

cat("\n", rep("=", 60), "\n")
cat("FINAL TEST SET RESULTS\n")
cat(rep("=", 60), "\n")
final_test_results_sorted <- final_test_results[order(-final_test_results$Test_Accuracy), ]
print(final_test_results_sorted)

# Save results
write.csv(cv_results, file.path(results_dir, "CV_Detailed_Results.csv"), row.names = FALSE)
write.csv(cv_summary, file.path(results_dir, "CV_Summary_Results.csv"), row.names = FALSE)
write.csv(final_test_results, file.path(results_dir, "Final_Test_Results.csv"), row.names = FALSE)

################################################################################
########## STEP 4: Visualization
################################################################################

# Clean dataset labels
cv_results_clean <- cv_results
cv_results_clean$Dataset_Label <- ifelse(cv_results_clean$Dataset == "RAW", "Sr87/86 Raw",
                                         ifelse(cv_results_clean$Dataset == "GAM", "Sr87/86 GAM",
                                                ifelse(cv_results_clean$Dataset == "MA", "Sr87/86 Moving Average",
                                                       ifelse(cv_results_clean$Dataset == "Sr88", "Sr88",
                                                              ifelse(cv_results_clean$Dataset == "Combined", "Combined Sr88 + Sr87/86", cv_results_clean$Dataset)))))

final_test_clean <- final_test_results
final_test_clean$Dataset_Label <- ifelse(final_test_clean$Dataset == "RAW", "Sr87/86 Raw",
                                         ifelse(final_test_clean$Dataset == "GAM", "Sr87/86 GAM",
                                                ifelse(final_test_clean$Dataset == "MA", "Sr87/86 Moving Average",
                                                       ifelse(final_test_clean$Dataset == "Sr88", "Sr88",
                                                              ifelse(final_test_clean$Dataset == "Combined", "Combined Sr88 + Sr87/86", final_test_clean$Dataset)))))

# Professional theme
theme_clean <- theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 11),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_text(size = 11, face = "bold"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA),
    strip.text = element_text(size = 10, face = "bold")
  )

# Add title suffixes
filter_suffix <- if (FILTER_NATAL_ISO) paste0(" (Natal_Iso < ", NATAL_ISO_THRESHOLD, ")") else ""
norm_suffix <- if (NORMALIZE_PREDICTORS) " [Normalized]" else " [Raw]"
title_suffix <- paste0(filter_suffix, norm_suffix)

# Create boxplots for cross-validation results
accuracy_boxplot <- ggplot(cv_results_clean, aes(x = Model, y = Accuracy, fill = Model)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 16, outlier.size = 1) +
  geom_jitter(width = 0.2, alpha = 0.5, size = 0.8) +
  facet_wrap(~Dataset_Label, scales = "free_y", ncol = 3) +
  scale_fill_viridis_d(name = "Model") +
  labs(
    title = paste0("Cross-Validation Accuracy Distribution", title_suffix),
    x = "Model",
    y = "Accuracy",
    subtitle = paste0(CV_FOLDS, "-Fold CV × ", CV_REPEATS, " Repeats")
  ) +
  theme_clean +
  theme(legend.position = "bottom")

# Create heatmaps for final test results
final_test_clean$Accuracy_Rank <- rank(-final_test_clean$Test_Accuracy, ties.method = "min")
final_test_clean$Top3_Accuracy <- ifelse(final_test_clean$Accuracy_Rank <= 3, "Top 3", "Other")

test_accuracy_heatmap <- ggplot(final_test_clean, aes(x = Model, y = Dataset_Label, fill = Top3_Accuracy)) +
  geom_tile(color = "black", size = 1) +
  geom_text(aes(label = sprintf("%.3f", Test_Accuracy)), color = "black", size = 4, fontface = "bold") +
  scale_fill_manual(name = "Performance", values = c("Top 3" = "lightgreen", "Other" = "white")) +
  labs(title = paste0("Final Test Set Accuracy", title_suffix), x = "Model", y = "Dataset") +
  theme_clean +
  theme(axis.text.x = element_text(angle = 0))

# Save all plots
ggsave(file.path(figures_dir, "CV_Accuracy_Boxplots.png"), accuracy_boxplot, 
       width = 12, height = 8, dpi = 300, bg = "white")
ggsave(file.path(figures_dir, "Test_Accuracy_Heatmap.png"), test_accuracy_heatmap, 
       width = 8, height = 5, dpi = 300, bg = "white")

# Create a combined plot showing both CV and test results
combined_plot <- accuracy_boxplot | test_accuracy_heatmap
ggsave(file.path(figures_dir, "Combined_CV_and_Test_Results.png"), combined_plot, 
       width = 16, height = 8, dpi = 300, bg = "white")

cat("\n", rep("=", 60), "\n")
cat("ANALYSIS COMPLETE\n")
cat(rep("=", 60), "\n")
if (FILTER_NATAL_ISO) {
  cat("Filter applied: natal_iso <", NATAL_ISO_THRESHOLD, "\n")
} else {
  cat("No filter applied (original analysis)\n")
}
cat("Normalization:", ifelse(NORMALIZE_PREDICTORS, "ENABLED", "DISABLED"), "\n")
cat("Cross-validation:", CV_FOLDS, "folds ×", CV_REPEATS, "repeats\n")
cat("Results saved to:", results_dir, "\n")
cat("Models saved to:", models_dir, "\n")
cat("Figures saved to:", figures_dir, "\n")
cat("Key outputs:\n")
cat("- CV_Detailed_Results.csv: All fold-by-fold results\n")
cat("- CV_Summary_Results.csv: Mean and SD statistics\n")
cat("- Final_Test_Results.csv: Hold-out test performance\n")
cat("- Boxplots: CV performance distributions\n")
cat("- Heatmaps: Final test performance\n")
cat(rep("=", 60), "\n")