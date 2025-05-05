# 03a_timeseries_modeling.R
# Unified training of time series models using tidymodels framework
# Simplified to focus only on accuracy

library(tidyverse)
library(tidymodels)
library(here)

# Create all necessary directories at the start
create_directories <- function() {
  dirs <- c(
    "data/models",
    "data/results",
    "figures/models"
  )
  
  for (dir in dirs) {
    dir.create(here(dir), recursive = TRUE, showWarnings = FALSE)
  }
}

# Configuration
config <- list(
  data_sources = c("GAM", "MA", "RAW", "Sr88", "Combined"), # Time series data types
  model_types = c("rf", "svm", "knn"),
  test_prop = 0.2,
  cv_folds = 5,
  random_seed = 123
)

# Load all preprocessed time series data
load_ts_data <- function() {
  # Time series data
  data_files <- list.files(here("data/preprocessed_matrices"), 
                           pattern = "^preprocessed_.+\\.csv$", 
                           full.names = TRUE)
  
  # Read all data files
  all_data <- list()
  
  for (file in data_files) {
    file_name <- basename(file)
    data_type <- gsub("preprocessed_(.+)\\.csv", "\\1", file_name)
    
    message(paste("Loading", data_type, "data..."))
    all_data[[data_type]] <- read_csv(file)
  }
  
  return(all_data)
}

# Create train/test split for time series data
create_ts_split <- function(all_data, config) {
  # Set seed for reproducibility
  set.seed(config$random_seed)
  
  # Get Fish_id from each dataset
  fish_ids <- lapply(all_data, function(data) data$Fish_id)
  
  # Find common Fish_ids across all datasets
  common_fish_ids <- Reduce(intersect, fish_ids)
  
  message(paste("Found", length(common_fish_ids), "common Fish_ids across datasets"))
  
  # Sample test set indices
  test_indices <- sample(length(common_fish_ids), size = floor(length(common_fish_ids) * config$test_prop))
  test_fish_ids <- common_fish_ids[test_indices]
  
  # Split each dataset using the same Fish_ids
  split_data <- list()
  
  for (data_type in names(all_data)) {
    data <- all_data[[data_type]]
    
    # Split data
    train_data <- data %>% filter(!Fish_id %in% test_fish_ids)
    test_data <- data %>% filter(Fish_id %in% test_fish_ids)
    
    split_data[[data_type]] <- list(
      train = train_data,
      test = test_data
    )
    
    message(paste("  ", data_type, "split:", nrow(train_data), "training samples,", 
                  nrow(test_data), "testing samples"))
  }
  
  # Save split
  saveRDS(split_data, here("data/ts_split_data.rds"))
  
  # Also save a list of train/test Fish_ids for consistency with shape analysis
  saveRDS(
    list(train_ids = setdiff(common_fish_ids, test_fish_ids),
         test_ids = test_fish_ids),
    here("data/fish_id_split.rds")
  )
  
  return(split_data)
}

# Train models for each time series data source using tidymodels
train_ts_models <- function(split_data, config) {
  # Initialize results storage
  all_models <- list()
  all_metrics <- list()
  
  # Train models for each data source
  for (data_source in names(split_data)) {
    # Get train and test data
    train_data <- split_data[[data_source]]$train
    test_data <- split_data[[data_source]]$test
    
    # Ensure Watershed is a factor
    train_data$Watershed <- as.factor(train_data$Watershed)
    test_data$Watershed <- as.factor(test_data$Watershed)
    
    # Define the recipe for preprocessing
    recipe_spec <- recipe(Watershed ~ ., data = train_data) %>%
      update_role(Fish_id, new_role = "ID") %>%
      update_role(Year, Natal_Iso, new_role = "ID") %>%
      step_normalize(all_predictors(), -all_nominal(), -has_role("ID"))
    
    # Create cross validation folds from training data
    set.seed(config$random_seed)
    cv_folds <- vfold_cv(
      train_data,
      v = config$cv_folds,
      strata = Watershed
    )
    
    # Train models for each model type
    for (model_type in config$model_types) {
      # Create model ID
      model_id <- paste(data_source, model_type, sep = "_")
      message(paste("Training model:", model_id))
      
      # Define model specification based on model_type
      if (model_type == "rf") {
        model_spec <- rand_forest() %>%
          set_engine("ranger") %>%
          set_mode("classification")
      } else if (model_type == "svm") {
        model_spec <- svm_rbf() %>%
          set_engine("kernlab") %>%
          set_mode("classification")
      } else if (model_type == "knn") {
        model_spec <- nearest_neighbor() %>%
          set_engine("kknn") %>%
          set_mode("classification")
      } else {
        message(paste("Unknown model type:", model_type))
        next
      }
      
      # Create the workflow
      workflow_spec <- workflow() %>%
        add_recipe(recipe_spec) %>%
        add_model(model_spec)
      
      # Fit the model on training data
      final_fit <- workflow_spec %>%
        fit(data = train_data)
      
      # Make predictions on test data
      predictions <- predict(final_fit, test_data)
      
      # Combine with actual values
      results_df <- test_data %>%
        select(Fish_id, Watershed) %>%
        bind_cols(predictions) %>%
        mutate(correct = Watershed == .pred_class)
      
      # Calculate accuracy
      acc <- mean(results_df$correct)
      
      # Store results
      all_models[[model_id]] <- final_fit
      all_metrics[[model_id]] <- list(
        accuracy = acc,
        predictions = results_df,
        model_info = list(
          data_source = data_source,
          model_type = model_type
        )
      )
      
      message(paste("  Accuracy:", round(acc, 4)))
    }
  }
  
  # Save models and metrics
  saveRDS(all_models, here("data/models/ts_models.rds"))
  saveRDS(all_metrics, here("data/results/ts_metrics.rds"))
  
  # Create summary metrics data frame
  summary_metrics <- map_dfr(names(all_metrics), function(model_id) {
    metrics <- all_metrics[[model_id]]
    tibble(
      Model_ID = model_id,
      Data_Source = metrics$model_info$data_source,
      Model_Type = metrics$model_info$model_type,
      Accuracy = metrics$accuracy
    )
  })
  
  # Save summary metrics
  write_csv(summary_metrics, here("data/results/ts_summary_metrics.csv"))
  
  # Create model visualizations
  create_model_visualizations(summary_metrics)
  
  return(list(
    models = all_models,
    metrics = all_metrics
  ))
}

# Create simplified model performance visualizations focusing only on accuracy
create_model_visualizations <- function(summary_metrics) {
  # 1. Overall accuracy comparison
  p1 <- ggplot(summary_metrics, aes(x = reorder(Model_ID, Accuracy), y = Accuracy, fill = Data_Source)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = sprintf("%.3f", Accuracy)), hjust = -0.1, size = 3) +
    coord_flip() +
    labs(
      title = "Model Accuracy Comparison",
      x = NULL,
      y = "Accuracy",
      fill = "Data Source"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5)
    )
  
  ggsave(
    here("figures/models/ts_accuracy_comparison.png"),
    p1,
    width = 10,
    height = 8,
    dpi = 300
  )
  
  # 2. Accuracy by data source and model type
  p2 <- ggplot(summary_metrics, aes(x = Data_Source, y = Accuracy, fill = Model_Type)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(label = sprintf("%.3f", Accuracy)), 
              position = position_dodge(width = 0.9), 
              vjust = -0.5, size = 3) +
    labs(
      title = "Model Accuracy by Data Source and Model Type",
      x = "Data Source",
      y = "Accuracy",
      fill = "Model Type"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5)
    )
  
  ggsave(
    here("figures/models/ts_accuracy_by_source_and_type.png"),
    p2,
    width = 10,
    height = 6,
    dpi = 300
  )
}

# Main execution
main <- function() {
  # Create all necessary directories first
  create_directories()
  
  # Load all time series data
  message("Loading time series data...")
  ts_data <- load_ts_data()
  
  # Check for existing train/test split
  split_file <- here("data/ts_split_data.rds")
  if (file.exists(split_file)) {
    message("Loading existing train/test split...")
    split_data <- readRDS(split_file)
  } else {
    message("Creating new train/test split...")
    split_data <- create_ts_split(ts_data, config)
  }
  
  # Train models
  message("Training time series models...")
  model_results <- train_ts_models(split_data, config)
  
  message("Time series modeling completed successfully!")
  return(model_results)
}

# Run the main function
ts_model_results <- main()
