# 03a_timeseries_modeling.R
# Unified training of time series models using tidymodels framework

library(tidyverse)
library(tidymodels)
library(here)

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
  all_workflows <- list()
  
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
          set_engine("ranger", importance = "impurity") %>%
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
      
      # Train the model with cross-validation
      cv_results <- workflow_spec %>%
        fit_resamples(
          resamples = cv_folds,
          metrics = metric_set(accuracy, roc_auc, sensitivity, specificity),
          control = control_resamples(save_pred = TRUE)
        )
      
      # Collect CV metrics
      cv_metrics <- collect_metrics(cv_results)
      
      # Fit the final model on all training data
      final_fit <- workflow_spec %>%
        fit(data = train_data)
      
      # Make predictions on test data
      predictions <- predict(final_fit, test_data)
      class_preds <- predictions$.pred_class
      
      # Get probability predictions
      prob_predictions <- predict(final_fit, test_data, type = "prob")
      
      # Combine results
      results_df <- test_data %>%
        select(Fish_id, Watershed) %>%
        bind_cols(
          predict(final_fit, test_data),
          predict(final_fit, test_data, type = "prob")
        ) %>%
        mutate(
          correct = Watershed == .pred_class
        )
      
      # Calculate confusion matrix
      conf_mat_obj <- conf_mat(results_df, truth = Watershed, estimate = .pred_class)
      
      # Calculate accuracy
      acc <- accuracy_vec(truth = results_df$Watershed, estimate = results_df$.pred_class)
      
      # Calculate metrics for each class
      class_metrics <- data.frame()
      
      for (cls in levels(results_df$Watershed)) {
        # Create binary version
        binary_results <- results_df %>%
          mutate(
            binary_truth = factor(ifelse(Watershed == cls, "yes", "no"), levels = c("yes", "no")),
            binary_pred = factor(ifelse(.pred_class == cls, "yes", "no"), levels = c("yes", "no"))
          )
        
        # Calculate metrics
        sens <- sensitivity(binary_results, truth = binary_truth, estimate = binary_pred)
        spec <- specificity(binary_results, truth = binary_truth, estimate = binary_pred)
        prec <- precision(binary_results, truth = binary_truth, estimate = binary_pred)
        
        # F1 score
        f1 <- (2 * sens * prec) / (sens + prec)
        
        # Add to dataframe
        class_metrics <- rbind(class_metrics, data.frame(
          Class = cls,
          Sensitivity = sens$.estimate,
          Specificity = spec$.estimate,
          Precision = prec$.estimate,
          F1 = f1
        ))
      }
      
      # Store results
      all_models[[model_id]] <- final_fit
      all_workflows[[model_id]] <- workflow_spec
      all_metrics[[model_id]] <- list(
        cv_results = cv_results,
        cv_metrics = cv_metrics,
        final_accuracy = acc,
        conf_mat = conf_mat_obj,
        class_metrics = class_metrics,
        predictions = results_df,
        model_info = list(
          data_source = data_source,
          model_type = model_type
        )
      )
      
      message(paste("  Accuracy:", round(acc, 4)))
    }
  }
  
  # Save models, workflows, and metrics
  saveRDS(all_models, here("data/models/ts_models.rds"))
  saveRDS(all_workflows, here("data/models/ts_workflows.rds"))
  saveRDS(all_metrics, here("data/results/ts_metrics.rds"))
  
  # Create summary metrics data frame
  summary_metrics <- map_dfr(names(all_metrics), function(model_id) {
    metrics <- all_metrics[[model_id]]
    tibble(
      Model_ID = model_id,
      Data_Source = metrics$model_info$data_source,
      Model_Type = metrics$model_info$model_type,
      Accuracy = metrics$final_accuracy
    )
  })
  
  # Save summary metrics
  write_csv(summary_metrics, here("data/results/ts_summary_metrics.csv"))
  
  # Create detailed metrics with class-specific performance
  detailed_metrics <- map_dfr(names(all_metrics), function(model_id) {
    metrics <- all_metrics[[model_id]]
    
    metrics$class_metrics %>%
      mutate(
        Model_ID = model_id,
        Data_Source = metrics$model_info$data_source,
        Model_Type = metrics$model_info$model_type
      )
  })
  
  # Save detailed metrics
  write_csv(detailed_metrics, here("data/results/ts_detailed_metrics.csv"))
  
  # Create model visualizations
  create_model_visualizations(all_metrics)
  
  return(list(
    models = all_models,
    workflows = all_workflows,
    metrics = all_metrics
  ))
}

# Create model performance visualizations
create_model_visualizations <- function(all_metrics) {
  # Create figures directory if it doesn't exist
  dir.create(here("figures/models"), recursive = TRUE, showWarnings = FALSE)
  
  # Extract summary metrics
  summary_metrics <- map_dfr(names(all_metrics), function(model_id) {
    metrics <- all_metrics[[model_id]]
    tibble(
      Model_ID = model_id,
      Data_Source = metrics$model_info$data_source,
      Model_Type = metrics$model_info$model_type,
      Accuracy = metrics$final_accuracy
    )
  })
  
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
  
  # 3. Extract class-specific metrics
  class_metrics <- map_dfr(names(all_metrics), function(model_id) {
    # Extract metrics
    metrics <- all_metrics[[model_id]]
    
    # Add model info
    metrics$class_metrics %>%
      mutate(
        Model_ID = model_id,
        Data_Source = metrics$model_info$data_source,
        Model_Type = metrics$model_info$model_type
      )
  })
  
  # Create F1 score heatmap
  p3 <- class_metrics %>%
    ggplot(aes(x = Class, y = Model_ID, fill = F1)) +
    geom_tile(color = "white", size = 0.2) +
    geom_text(aes(label = sprintf("%.2f", F1)), color = "white", size = 3) +
    scale_fill_viridis_c(option = "plasma", limits = c(0, 1)) +
    labs(
      title = "F1 Scores by Model and Watershed Class",
      x = "Watershed",
      y = "Model",
      fill = "F1 Score"
    ) +
    theme_minimal() +
    theme(
      axis.text.y = element_text(hjust = 1),
      legend.position = "right",
      plot.title = element_text(hjust = 0.5),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  ggsave(
    here("figures/models/ts_f1_heatmap.png"),
    p3,
    width = 12,
    height = 10,
    dpi = 300
  )
  
  # 4. Create individual confusion matrices
  for (model_id in names(all_metrics)) {
    # Get confusion matrix
    conf_mat_obj <- all_metrics[[model_id]]$conf_mat
    
    # Create plot using autoplot
    p <- autoplot(conf_mat_obj, type = "heatmap") +
      labs(title = paste("Confusion Matrix -", model_id)) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5)
      )
    
    ggsave(
      here(paste0("figures/models/ts_confmat_", model_id, ".png")),
      p,
      width = 8,
      height = 6,
      dpi = 300
    )
  }
}

# Main execution
main <- function() {
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
