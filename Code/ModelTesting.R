# Load required libraries
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

# Output directories - updated path
train_test_dir_total <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/data/LA_Data/TrainingTesting"
train_test_dir_overlap <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/data/LA_Data/TrainingTesting/Filtered"
models_dir_total <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/data/Models"
models_dir_overlap <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/data/Models/Filtered"

# Create directories if they don't exist
dir.create(train_test_dir_total, recursive = TRUE, showWarnings = FALSE)
dir.create(train_test_dir_overlap, recursive = TRUE, showWarnings = FALSE)
dir.create(models_dir_total, recursive = TRUE, showWarnings = FALSE)
dir.create(models_dir_overlap, recursive = TRUE, showWarnings = FALSE)

# Define metadata columns to exclude from modeling
metadata_columns <- c("Fish_id", "Year", "Natal_Start", "Marine_Start", 
                      "Marine_End", "Interpolated_Points", "Micron_Size", "Natal_Iso","Original_Data_Points")

################################################################################
#### STEP 1: Create train/test splits and datasets
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

# Process each dataset to create train/test files
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
#### FUNCTION TO RUN ANALYSIS
################################################################################

run_analysis <- function(train_test_dir, models_dir, analysis_name) {
  
  # Create results data frame
  results <- data.frame()
  
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
    
    models <- list(
      RF = rand_forest(trees = 500, mtry = floor(sqrt(n_predictors))) %>% 
        set_engine("ranger") %>% set_mode("classification"),
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
      
      # Make predictions on test data
      predictions <- workflow_obj %>%
        predict(test_data) %>%
        bind_cols(test_data %>% select(Watershed))
      
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
#### RUN BOTH ANALYSES
################################################################################

cat("=== Running TOTAL analysis (same training, full test set) ===\n")
results_total <- run_analysis(train_test_dir_total, models_dir_total, "TOTAL")

cat("\n=== Running OVERLAP analysis (same training, filtered test set) ===\n")
results_overlap <- run_analysis(train_test_dir_overlap, models_dir_overlap, "OVERLAP")

cat("\n=== Analysis Complete ===\n")
cat("Both analyses used identical training sets\n")
cat("TOTAL tested on full test set, OVERLAP tested on filtered test set\n")
