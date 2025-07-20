# Load required libraries
library(here)
library(dplyr)
library(tidymodels)
library(ranger)
library(kernlab)
library(kknn)
library(ggplot2)
library(viridis)

################################################################################
#### CONFIGURATION
################################################################################

# Set this to TRUE to filter for natal_iso < 0.713, FALSE for original analysis
FILTER_NATAL_ISO <- TRUE

# Filter threshold (only used when FILTER_NATAL_ISO = TRUE)
NATAL_ISO_THRESHOLD <- 0.713

################################################################################
#### STEP 1: Create test/train splits
################################################################################

# Set seed for reproducibility
set.seed(123)

# Define data types
data_types <- c("RAW", "GAM", "MA", "Sr88", "Combined")

# Define paths
base_data_path <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/data/LA_Data/Preprocessed_ts_matrices"
figures_dir <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/Figures"

# Create different output directories based on filter setting
if (FILTER_NATAL_ISO) {
  train_test_dir <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/data/LA_Data/Train_Test_Filtered"
  models_dir <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/data/Models/Filtered"
} else {
  train_test_dir <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/data/LA_Data/Train_Test"
  models_dir <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/data/Models"
}


# Define metadata columns to exclude from modeling
metadata_columns <- c("Fish_id", "Year", "Natal_Iso", "Natal_Start", "Marine_Start", 
                      "Marine_End", "Original_Data_Points", "Interpolated_Points")

# Load all datasets and get common fish IDs
all_data <- list()
for (data_type in data_types) {
  file_path <- file.path(base_data_path, paste0("NatalToMarine_Processed_", data_type, ".csv"))
  
  if (file.exists(file_path)) {
    data <- read.csv(file_path) %>%
      mutate(Watershed = as.factor(Watershed))
    
    # Apply natal_iso filter if enabled
    if (FILTER_NATAL_ISO) {
      data <- data %>% filter(Natal_Iso < NATAL_ISO_THRESHOLD)
    }
    
    all_data[[data_type]] <- data
  }
}

# Get common fish IDs across all datasets
fish_ids <- lapply(all_data, function(x) x$Fish_id)
common_fish_ids <- Reduce(intersect, fish_ids)

# Create train/test split based on Fish_id
unique_fish_ids <- unique(common_fish_ids)
train_fish_ids <- sample(unique_fish_ids, size = 0.8 * length(unique_fish_ids))
test_fish_ids <- setdiff(unique_fish_ids, train_fish_ids)

# Loop through each dataset and save train/test splits
for (data_type in names(all_data)) {
  # Filter to common fish IDs
  data <- all_data[[data_type]] %>%
    filter(Fish_id %in% common_fish_ids)
  
  # Split data by Fish_id
  train_data <- data[data$Fish_id %in% train_fish_ids, ]
  test_data <- data[data$Fish_id %in% test_fish_ids, ]
  
  # Remove metadata columns - keep ONLY Watershed (target) and predictors
  train_clean <- train_data %>%
    select(-all_of(metadata_columns))
  
  test_clean <- test_data %>%
    select(-all_of(metadata_columns))
  
  # Save files
  train_filename <- file.path(train_test_dir, paste0("Train_", data_type, ".csv"))
  test_filename <- file.path(train_test_dir, paste0("Test_", data_type, ".csv"))
  
  write.csv(train_clean, train_filename, row.names = FALSE)
  write.csv(test_clean, test_filename, row.names = FALSE)
}

# Save Fish_id splits for reference
fish_id_splits <- data.frame(
  Fish_id = c(train_fish_ids, test_fish_ids),
  Split = c(rep("Train", length(train_fish_ids)), rep("Test", length(test_fish_ids)))
)

write.csv(fish_id_splits, file.path(train_test_dir, "Fish_ID_Splits.csv"), row.names = FALSE)

################################################################################
#### STEP 2: Run the models
################################################################################

# Set seed for reproducibility
set.seed(123)

# Create results data frame
results <- data.frame()

# Loop through each dataset and model
for (data_type in data_types) {
  
  # Load data
  train_data <- read.csv(file.path(train_test_dir, paste0("Train_", data_type, ".csv"))) %>%
    mutate(Watershed = as.factor(Watershed))
  test_data <- read.csv(file.path(train_test_dir, paste0("Test_", data_type, ".csv"))) %>%
    mutate(Watershed = as.factor(Watershed))
  
  
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
    
    # Fit model
    workflow_obj <- workflow() %>%
      add_recipe(base_recipe) %>%
      add_model(models[[model_name]]) %>%
      fit(train_data)
    
    # Save the trained model as RDS
    model_filename <- paste0(data_type, "_", model_name, "_model.rds")
    model_filepath <- file.path(models_dir, model_filename)
    saveRDS(workflow_obj, model_filepath)
    
    # Make predictions
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
print(results)

################################################################################
#### STEP 3: Visualize
################################################################################


# Clean dataset labels
results_clean <- results %>%
  mutate(
    Dataset_Label = case_when(
      Dataset == "RAW" ~ "Sr87/86 Raw",
      Dataset == "GAM" ~ "Sr87/86 GAM", 
      Dataset == "MA" ~ "Sr87/86 Moving Average",
      Dataset == "Sr88" ~ "Sr88",
      Dataset == "Combined" ~ "Combined Sr88 + Sr87/86"
    )
  )

# Professional theme
theme_clean <- theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 11),
    legend.title = element_text(size = 11, face = "bold"),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA)
  )

# Create ranking for highlighting top 3
results_clean <- results_clean %>%
  mutate(
    Accuracy_Rank = rank(-Accuracy, ties.method = "min"),
    F1_Rank = rank(-F1_Score, ties.method = "min"),
    Top3_Accuracy = ifelse(Accuracy_Rank <= 3, "Top 3", "Other"),
    Top3_F1 = ifelse(F1_Rank <= 3, "Top 3", "Other")
  )

# Add normalization status to plot titles
filter_suffix <- if (FILTER_NATAL_ISO) paste0(" (Natal_Iso < ", NATAL_ISO_THRESHOLD, ")") else ""
norm_suffix <- if (NORMALIZE_PREDICTORS) " [Normalized]" else " [Raw]"
title_suffix <- paste0(filter_suffix, norm_suffix)

# Accuracy heatmap
accuracy_plot <- ggplot(results_clean, aes(x = Model, y = Dataset_Label, fill = Top3_Accuracy)) +
  geom_tile(color = "black", size = 1) +
  geom_text(aes(label = sprintf("%.3f", Accuracy)), color = "black", size = 4, fontface = "bold") +
  scale_fill_manual(name = "Performance", values = c("Top 3" = "lightgreen", "Other" = "white")) +
  labs(title = paste0("Model Accuracy", title_suffix), x = "Model", y = "Dataset") +
  theme_clean

# F1-Score heatmap  
f1_plot <- ggplot(results_clean, aes(x = Model, y = Dataset_Label, fill = Top3_F1)) +
  geom_tile(color = "black", size = 1) +
  geom_text(aes(label = sprintf("%.3f", F1_Score)), color = "black", size = 4, fontface = "bold") +
  scale_fill_manual(name = "Performance", values = c("Top 3" = "lightgreen", "Other" = "white")) +
  labs(title = paste0("Model F1-Score", title_suffix), x = "Model", y = "Dataset") +
  theme_clean

# Save plots
ggsave(file.path(figures_dir, "Model_Accuracy_Heatmap.png"), accuracy_plot, 
       width = 8, height = 5, dpi = 300, bg = "white")
ggsave(file.path(figures_dir, "Model_F1Score_Heatmap.png"), f1_plot, 
       width = 8, height = 5, dpi = 300, bg = "white")