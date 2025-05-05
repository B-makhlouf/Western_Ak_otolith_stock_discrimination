# 03_modeling.R
# Unified training of all models using the same train/test split
# Implements all models using tidymodels framework

library(tidyverse)
library(tidymodels)
library(here)

# Configuration
config <- list(
  data_sources = c("GAM", "Sr88", "Combined", "Outline"),
  model_types = c("rf", "svm", "knn"),
  test_prop = 0.2,
  cv_folds = 5,
  random_seed = 123
)

# Load all preprocessed data
load_all_data <- function() {
  # Time series data
  gam_data <- read_csv(here("data/preprocessed_matrices/preprocessed_GAM.csv"))
  sr88_data <- read_csv(here("data/preprocessed_matrices/preprocessed_Sr88.csv"))
  combined_data <- read_csv(here("data/preprocessed_matrices/preprocessed_Combined.csv"))
  
  # Outline data
  outline_data <- read_csv(here("data/processed/outlines/fourier_coefficients.csv"))
  
  # Return as list
  list(
    GAM = gam_data,
    Sr88 = sr88_data,
    Combined = combined_data,
    Outline = outline_data
  )
}

# Create unified train/test split
create_unified_split <- function(all_data, config) {
  # Extract fish IDs from each dataset
  fish_ids <- list(
    GAM = all_data$GAM$Fish_id,
    Sr88 = all_data$Sr88$Fish_id,
    Combined = all_data$Combined$Fish_id,
    Outline = extract_fish_ids(all_data$Outline$picname)
  )
  
  # Find common fish IDs across datasets
  common_fish_ids <- Reduce(intersect, fish_ids)
  
  if (length(common_fish_ids) < 10) {
    # If few common IDs, create separate splits
    message("Few common fish IDs found. Creating separate splits for each dataset.")
    
    split_data <- list()
    
    for (data_source in names(all_data)) {
      df <- all_data[[data_source]]
      
      # Create ID column
      if (data_source == "Outline") {
        df$Fish_id <- extract_fish_ids(df$picname)
      }
      
      # Set seed for reproducibility
      set.seed(config$random_seed)
      
      # Create split
      if (data_source == "Outline") {
        split <- initial_split(df, prop = 1 - config$test_prop, strata = watershed)
      } else {
        split <- initial_split(df, prop = 1 - config$test_prop, strata = Watershed)
      }
      
      train_data <- training(split)
      test_data <- testing(split)
      
      split_data[[data_source]] <- list(
        train = train_data,
        test = test_data,
        split = split
      )
    }
  } else {
    # Create unified split using common fish IDs
    message(paste(length(common_fish_ids), "common fish IDs found. Creating unified split."))
    
    # Set seed for reproducibility
    set.seed(config$random_seed)
    
    # Create indices for train/test split
    all_indices <- 1:length(common_fish_ids)
    test_indices <- sample(all_indices, size = floor(length(all_indices) * config$test_prop))
    train_indices <- setdiff(all_indices, test_indices)
    
    train_ids <- common_fish_ids[train_indices]
    test_ids <- common_fish_ids[test_indices]
    
    # Apply split to each dataset
    split_data <- list()
    
    for (data_source in names(all_data)) {
      df <- all_data[[data_source]]
      
      # Create ID column for outline data
      if (data_source == "Outline") {
        df$Fish_id <- extract_fish_ids(df$picname)
        
        train_data <- df %>% filter(Fish_id %in% train_ids)
        test_data <- df %>% filter(Fish_id %in% test_ids)
      } else {
        train_data <- df %>% filter(Fish_id %in% train_ids)
        test_data <- df %>% filter(Fish_id %in% test_ids)
      }
      
      split_data[[data_source]] <- list(
        train = train_data,
        test = test_data
      )
    }
  }
  
  # Save the splits
  dir.create(here("data/processed/splits"), recursive = TRUE, showWarnings = FALSE)
  saveRDS(split_data, here("data/processed/splits/unified_split.rds"))
  
  message("Train/test splits created and saved.")
  return(split_data)
}

# Extract fish ID from picname
extract_fish_ids <- function(picnames) {
  # Extract year, watershed, and ID number from picnames
  # Format expected: YYYY_ww_NNN
  gsub("^(\\d{4}_[a-z]{2}_\\d+).*$", "\\1", picnames)
}

# Train models for each data source and model type
train_all_models <- function(split_data, config) {
  # Initialize results storage
  all_models <- list()
  all_metrics <- list()
  
  # Set up cross-validation
  cv_settings <- vfold_cv(
    data = NULL,  # Will be set for each dataset
    v = config$cv_folds
  )
  
  # Train models for each data source
  for (data_source in names(split_data)) {
    train_data <- split_data[[data_source]]$train
    test_data <- split_data[[data_source]]$test
    
    # Adjust column names for consistency
    target_var <- ifelse(data_source == "Outline", "watershed", "Watershed")
    id_var <- "Fish_id"
    
    # Ensure target is a factor
    train_data[[target_var]] <- as.factor(train_data[[target_var]])
    test_data[[target_var]] <- as.factor(test_data[[target_var]])
    
    # Create recipe
    model_recipe <- recipe(formula(paste(target_var, "~ .")), data = train_data) %>%
      step_rm(all_of(id_var)) %>%
      step_rm(matches("picname|Year|Natal_Iso")) %>%
      step_normalize(all_numeric_predictors())
    
    # Update cross-validation folds
    cv_settings$splits <- vfold_cv(
      data = train_data,
      v = config$cv_folds,
      strata = !!sym(target_var)
    )$splits
    
    # Train models for each model type
    for (model_type in config$model_types) {
      # Create model specification
      if (model_type == "rf") {
        model_spec <- rand_forest(trees = 500) %>%
          set_engine("ranger", importance = "impurity") %>%
          set_mode("classification")
      } else if (model_type == "svm") {
        model_spec <- svm_rbf() %>%
          set_engine("kernlab") %>%
          set_mode("classification")
      } else if (model_type == "knn") {
        model_spec <- nearest_neighbor(neighbors = 5) %>%
          set_engine("kknn") %>%
          set_mode("classification")
      } else {
        next
      }
      
      # Create workflow
      wf <- workflow() %>%
        add_recipe(model_recipe) %>%
        add_model(model_spec)
      
      # Fit model with cross-validation
      cv_results <- fit_resamples(
        wf,
        resamples = cv_settings,
        metrics = metric_set(accuracy, kap, roc_auc)
      )
      
      # Fit final model
      final_model <- fit(wf, train_data)
      
      # Make predictions on test data
      predictions <- predict(final_model, test_data)
      probabilities <- predict(final_model, test_data, type = "prob")
      
      # Combine predictions with actual values
      results <- bind_cols(
        test_data %>% select(all_of(id_var), all_of(target_var)),
        predictions,
        probabilities
      ) %>%
        rename(
          Actual = !!sym(target_var),
          Predicted = .pred_class
        )
      
      # Calculate metrics
      metrics <- list(
        Accuracy = accuracy_vec(results$Actual, results$Predicted),
        Kappa = kap_vec(results$Actual, results$Predicted),
        CV_Metrics = collect_metrics(cv_results)
      )
      
      # Save model and results
      model_id <- paste(data_source, model_type, sep = "_")
      all_models[[model_id]] <- final_model
      all_metrics[[model_id]] <- list(
        metrics = metrics,
        predictions = results
      )
      
      message(paste("Trained model:", model_id, "| Accuracy:", round(metrics$Accuracy, 4)))
    }
  }
  
  # Create directories for saving
  dir.create(here("data/models"), recursive = TRUE, showWarnings = FALSE)
  dir.create(here("data/results"), recursive = TRUE, showWarnings = FALSE)
  
  # Save models and results
  saveRDS(all_models, here("data/models/all_models.rds"))
  saveRDS(all_metrics, here("data/results/all_model_metrics.rds"))
  
  message("All models trained and saved.")
  return(list(models = all_models, metrics = all_metrics))
}

# Main execution
all_data <- load_all_data()
split_data <- create_unified_split(all_data, config)
model_results <- train_all_models(split_data, config)