# Modified version of 03_modeling.R to create ONLY the accuracy heatmap
# Saves to specific location with specific filename

library(tidyverse)
library(tidymodels)
library(here)
library(viridis)
library(scales)

# Create the specific output directory
output_dir <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/Figures/ts_Classification"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Configuration
config <- list(
  data_sources = c("GAM", "MA", "RAW", "Sr88", "Combined"),
  model_types = c("rf", "svm", "knn"),
  test_prop = 0.2,
  cv_folds = 5,
  random_seed = 123
)

# Load all preprocessed time series data
load_ts_data <- function() {
  data_files <- list.files(here("data/preprocessed_matrices"), 
                           pattern = "^preprocessed_.+\\.csv$", 
                           full.names = TRUE)
  
  all_data <- list()
  for (file in data_files) {
    file_name <- basename(file)
    data_type <- gsub("preprocessed_(.+)\\.csv", "\\1", file_name)
    message(paste("Loading", data_type, "data..."))
    all_data[[data_type]] <- read_csv(file)
  }
  return(all_data)
}

# Create train/test split
create_ts_split <- function(all_data, config) {
  set.seed(config$random_seed)
  
  fish_ids <- lapply(all_data, function(data) data$Fish_id)
  common_fish_ids <- Reduce(intersect, fish_ids)
  message(paste("Found", length(common_fish_ids), "common Fish_ids across datasets"))
  
  test_indices <- sample(length(common_fish_ids), size = floor(length(common_fish_ids) * config$test_prop))
  test_fish_ids <- common_fish_ids[test_indices]
  
  split_data <- list()
  for (data_type in names(all_data)) {
    data <- all_data[[data_type]]
    train_data <- data %>% filter(!Fish_id %in% test_fish_ids)
    test_data <- data %>% filter(Fish_id %in% test_fish_ids)
    
    split_data[[data_type]] <- list(
      train = train_data,
      test = test_data
    )
    message(paste("  ", data_type, "split:", nrow(train_data), "training samples,", 
                  nrow(test_data), "testing samples"))
  }
  return(split_data)
}

# Train models (simplified to only collect metrics needed for heatmap)
train_ts_models_for_heatmap <- function(split_data, config) {
  all_metrics <- list()
  
  for (data_source in names(split_data)) {
    train_data <- split_data[[data_source]]$train
    test_data <- split_data[[data_source]]$test
    
    train_data$Watershed <- as.factor(train_data$Watershed)
    test_data$Watershed <- as.factor(test_data$Watershed)
    
    recipe_spec <- recipe(Watershed ~ ., data = train_data) %>%
      update_role(Fish_id, new_role = "ID") %>%
      update_role(Year, Natal_Iso, new_role = "ID") %>%
      step_normalize(all_predictors(), -all_nominal(), -has_role("ID"))
    
    for (model_type in config$model_types) {
      model_id <- paste(data_source, model_type, sep = "_")
      message(paste("Training model:", model_id))
      
      # Define model specification
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
      }
      
      workflow_spec <- workflow() %>%
        add_recipe(recipe_spec) %>%
        add_model(model_spec)
      
      final_fit <- workflow_spec %>% fit(data = train_data)
      predictions <- predict(final_fit, test_data) %>%
        bind_cols(test_data %>% select(Watershed)) %>%
        mutate(correct = Watershed == .pred_class)
      
      acc <- mean(predictions$correct)
      
      all_metrics[[model_id]] <- list(
        accuracy = acc,
        model_info = list(
          data_source = data_source,
          model_type = model_type
        )
      )
      message(paste("  Accuracy:", round(acc, 4)))
    }
  }
  
  # Create summary metrics
  summary_metrics <- map_dfr(names(all_metrics), function(model_id) {
    metrics <- all_metrics[[model_id]]
    tibble(
      Model_ID = model_id,
      Data_Source = metrics$model_info$data_source,
      Model_Type = metrics$model_info$model_type,
      Accuracy = metrics$accuracy
    )
  })
  
  return(summary_metrics)
}

# Create ONLY the accuracy heatmap
create_multimodel_heatmap <- function(summary_metrics, save_path) {
  # Prepare data for visualization
  accuracy_summary <- summary_metrics %>%
    mutate(
      Data_Source = factor(Data_Source, 
                           levels = c("GAM", "MA", "RAW", "Sr88", "Combined")),
      Model_Type = factor(Model_Type, 
                          levels = c("rf", "svm", "knn"),
                          labels = c("Random Forest", "SVM", "KNN"))
    )
  
  # Find min and max accuracy for better scaling
  min_acc <- min(accuracy_summary$Accuracy)
  max_acc <- max(accuracy_summary$Accuracy)
  mid_point <- (min_acc + max_acc) / 2
  
  # Set custom breaks
  custom_breaks <- seq(
    from = floor(min_acc * 100) / 100,
    to = ceiling(max_acc * 100) / 100,
    length.out = 5
  )
  
  # Create the heatmap with blue-yellow-red color scale
  heatmap_plot <- ggplot(accuracy_summary, aes(x = Model_Type, y = Data_Source, fill = Accuracy)) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_text(aes(label = sprintf("%.3f", Accuracy)), 
              color = "black", size = 4, fontface = "bold") +
    # Blue-yellow-red scale
    scale_fill_gradientn(
      colors = c("dodgerblue4", "dodgerblue", "yellow", "orange", "firebrick"),
      values = scales::rescale(c(min_acc, min_acc + (max_acc - min_acc) * 0.25, 
                                 mid_point, mid_point + (max_acc - mid_point) * 0.5, max_acc)),
      limits = c(min_acc * 0.99, max_acc * 1.01),
      breaks = custom_breaks,
      labels = scales::percent_format(accuracy = 0.1)
    ) +
    labs(
      title = "Classification Accuracy by Model and Data Type",
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
  
  # Save the plot
  ggsave(
    save_path,
    heatmap_plot,
    width = 10,
    height = 8,
    dpi = 300
  )
  
  message(paste("✓ Heatmap saved to:", save_path))
  return(heatmap_plot)
}

# Main execution
main <- function() {
  message("Loading time series data...")
  ts_data <- load_ts_data()
  
  # Check for existing split or create new one
  split_file <- here("data/ts_split_data.rds")
  if (file.exists(split_file)) {
    message("Loading existing train/test split...")
    split_data <- readRDS(split_file)
  } else {
    message("Creating new train/test split...")
    split_data <- create_ts_split(ts_data, config)
    saveRDS(split_data, split_file)
  }
  
  message("Training models for heatmap...")
  summary_metrics <- train_ts_models_for_heatmap(split_data, config)
  
  # Create and save ONLY the heatmap
  output_file <- file.path(output_dir, "multimodel_comp.png")
  heatmap_plot <- create_multimodel_heatmap(summary_metrics, output_file)
  
  message("✓ Heatmap creation completed!")
  message(paste("File saved to:", output_file))
  
  return(summary_metrics)
}

# Run the main function
results <- main()
