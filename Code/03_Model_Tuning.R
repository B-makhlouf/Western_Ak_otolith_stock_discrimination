# 03_model_tuning.R
# Calibrates model probabilities to ensure reliable probability estimates
# Provides pre/post calibration visualization and performance metrics
# Saves calibrated models for deployment

library(tidyverse)
library(tidymodels)
library(probably)
library(here)
library(ggplot2)
library(gridExtra)
library(viridis)

# Create necessary directories
dirs <- c("data/models/calibrated", "figures/calibration", "data/results/calibration")
for (dir in dirs) {
  if (!dir.exists(here(dir))) {
    dir.create(here(dir), recursive = TRUE)
  }
}

# Configuration
config <- list(
  data_types = c("GAM", "Sr88", "Combined", "Outline"),
  model_methods = c("rf", "svm", "knn"),
  calibration_method = "isotonic",  # "isotonic" or "logistic"
  random_seed = 123
)

# Load all data
load_all_data <- function() {
  # Load preprocessed data
  data_files <- list.files(here("data/preprocessed_matrices"), 
                           pattern = "^preprocessed_.+\\.csv$", 
                           full.names = TRUE)
  
  # Load outline data if available
  outline_file <- here("data/processed/outlines/fourier_coefficients.csv")
  if (file.exists(outline_file)) {
    data_files <- c(data_files, outline_file)
  }
  
  # Read all data files
  all_data <- list()
  
  for (file in data_files) {
    file_name <- basename(file)
    data_type <- gsub("preprocessed_(.+)\\.csv", "\\1", file_name)
    
    if (data_type == "fourier_coefficients") {
      data_type <- "Outline"
    }
    
    all_data[[data_type]] <- read_csv(file)
  }
  
  return(all_data)
}

# Load trained models
load_models <- function() {
  # Look for models in standard locations
  model_dirs <- c(
    here("data/models"),
    here("models")
  )
  
  models <- list()
  
  for (dir in model_dirs) {
    if (dir.exists(dir)) {
      model_files <- list.files(dir, pattern = "\\.rds$", full.names = TRUE, recursive = TRUE)
      
      for (file in model_files) {
        # Extract model ID from filename
        model_id <- gsub(".*/(.*)\\.rds", "\\1", file)
        
        # Load model
        tryCatch({
          models[[model_id]] <- readRDS(file)
          message("Loaded model: ", model_id)
        }, error = function(e) {
          message("Error loading model: ", file, " - ", e$message)
        })
      }
    }
  }
  
  # If no models found, return NULL
  if (length(models) == 0) {
    message("No trained models found.")
    return(NULL)
  }
  
  return(models)
}

# Create a unified train/test split
create_split <- function(all_data, test_prop = 0.2) {
  # Set seed for reproducibility
  set.seed(config$random_seed)
  
  # Create a consistent split across all data types
  split_data <- list()
  
  # Get Fish_id from each dataset
  fish_ids <- list()
  
  for (data_type in names(all_data)) {
    if (data_type == "Outline") {
      # Extract Fish_id from picname
      fish_ids[[data_type]] <- gsub("^(\\d{4}_[a-z]{2}_\\d+).*$", "\\1", all_data[[data_type]]$picname)
    } else {
      fish_ids[[data_type]] <- all_data[[data_type]]$Fish_id
    }
  }
  
  # Find common Fish_ids across all datasets
  common_fish_ids <- Reduce(intersect, fish_ids)
  
  if (length(common_fish_ids) > 20) {
    message(paste("Found", length(common_fish_ids), "common Fish_ids across datasets"))
    
    # Sample test set indices
    test_indices <- sample(length(common_fish_ids), size = floor(length(common_fish_ids) * test_prop))
    test_fish_ids <- common_fish_ids[test_indices]
    
    # Split each dataset using the same Fish_ids
    for (data_type in names(all_data)) {
      data <- all_data[[data_type]]
      
      if (data_type == "Outline") {
        # Create Fish_id column for matching
        data$Fish_id <- gsub("^(\\d{4}_[a-z]{2}_\\d+).*$", "\\1", data$picname)
        
        # Split data
        train_data <- data %>% filter(!Fish_id %in% test_fish_ids)
        test_data <- data %>% filter(Fish_id %in% test_fish_ids)
        
        # Remove temporary Fish_id column
        train_data$Fish_id <- NULL
        test_data$Fish_id <- NULL
      } else {
        # Split data
        train_data <- data %>% filter(!Fish_id %in% test_fish_ids)
        test_data <- data %>% filter(Fish_id %in% test_fish_ids)
      }
      
      split_data[[data_type]] <- list(
        train = train_data,
        test = test_data
      )
    }
  } else {
    # If few common fish IDs, create separate splits
    message("Insufficient common Fish_ids found. Creating separate splits for each dataset.")
    
    for (data_type in names(all_data)) {
      data <- all_data[[data_type]]
      
      # Get stratification variable
      strat_var <- if (data_type == "Outline") "watershed" else "Watershed"
      
      # Create split
      split <- initial_split(data, prop = 1 - test_prop, strata = !!sym(strat_var))
      
      train_data <- training(split)
      test_data <- testing(split)
      
      split_data[[data_type]] <- list(
        train = train_data,
        test = test_data
      )
    }
  }
  
  # Save split
  saveRDS(split_data, here("data/split_data.rds"))
  
  return(split_data)
}

# Train models for each data type using tidymodels
train_models <- function(split_data) {
  models <- list()
  
  for (data_type in names(split_data)) {
    train_data <- split_data[[data_type]]$train
    
    # Determine target variable name
    target_var <- if (data_type == "Outline") "watershed" else "Watershed"
    
    # Ensure target is a factor
    train_data[[target_var]] <- as.factor(train_data[[target_var]])
    
    # Create recipe
    model_recipe <- recipe(as.formula(paste(target_var, "~ .")), data = train_data) %>%
      step_rm(matches("Fish_id|picname|Natal_Iso|Year")) %>%
      step_normalize(all_numeric_predictors())
    
    # Set up cross-validation
    cv_folds <- vfold_cv(train_data, v = 5, strata = !!sym(target_var))
    
    for (model_method in config$model_methods) {
      # Create model specification
      if (model_method == "rf") {
        model_spec <- rand_forest(trees = 500) %>%
          set_engine("ranger", importance = "impurity") %>%
          set_mode("classification")
      } else if (model_method == "svm") {
        model_spec <- svm_rbf() %>%
          set_engine("kernlab") %>%
          set_mode("classification")
      } else if (model_method == "knn") {
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
      
      # Fit model
      model_id <- paste(data_type, model_method, sep = "_")
      
      message("Training model: ", model_id)
      
      # Fit final model
      fitted_model <- fit(wf, train_data)
      
      # Store model
      models[[model_id]] <- fitted_model
    }
  }
  
  # Save models
  saveRDS(models, here("data/models/all_models.rds"))
  
  return(models)
}

# Evaluate and calibrate models
calibrate_models <- function(models, split_data) {
  # Results storage
  calibration_results <- list()
  
  for (model_id in names(models)) {
    message("Calibrating model: ", model_id)
    
    # Parse model id to get data type
    parts <- strsplit(model_id, "_")[[1]]
    data_type <- parts[1]
    model_method <- parts[2]
    
    # Get model and test data
    model <- models[[model_id]]
    test_data <- split_data[[data_type]]$test
    
    # Determine target variable
    target_var <- if (data_type == "Outline") "watershed" else "Watershed"
    test_data[[target_var]] <- as.factor(test_data[[target_var]])
    
    # Generate predictions
    class_preds <- predict(model, test_data, type = "class")
    prob_preds <- predict(model, test_data, type = "prob")
    
    # Create results dataframe
    results <- tibble(
      Actual = test_data[[target_var]],
      Predicted = class_preds$.pred_class
    ) %>%
      bind_cols(prob_preds)
    
    # Calculate metrics
    acc <- accuracy_vec(results$Actual, results$Predicted)
    kap <- kap_vec(results$Actual, results$Predicted)
    
    message("  Uncalibrated accuracy: ", round(acc, 4))
    
    # Prepare for calibration
    cal_estimators <- list()
    cal_plots_before <- list()
    cal_plots_after <- list()
    
    # For each class
    for (cls in levels(results$Actual)) {
      # Get probability column
      prob_col <- paste0(".pred_", cls)
      
      # Create binary dataset
      binary_df <- tibble(
        actual = results$Actual == cls,
        prob = results[[prob_col]]
      )
      
      # Create calibration plot before
      cal_plots_before[[cls]] <- cal_plot_windowed(
        binary_df, 
        truth = actual, 
        estimate = prob,
        window_size = 0.2,
        step_size = 0.05
      ) +
        ggtitle(paste("Before Calibration:", cls)) +
        theme_minimal() +
        coord_equal() +
        theme(plot.title = element_text(size = 10))
      
      # Calibrate probabilities
      if (config$calibration_method == "isotonic") {
        cal_estimators[[cls]] <- cal_estimate_isotonic(
          binary_df,
          truth = actual,
          estimate = prob
        )
      } else {
        cal_estimators[[cls]] <- cal_estimate_logistic(
          binary_df,
          truth = actual,
          estimate = prob
        )
      }
      
      # Apply calibration
      cal_probs <- cal_apply(binary_df, cal_estimators[[cls]])
      
      # Create calibration plot after
      cal_plots_after[[cls]] <- cal_plot_windowed(
        cal_probs,
        truth = actual,
        estimate = cal_prob,
        window_size = 0.2,
        step_size = 0.05
      ) +
        ggtitle(paste("After Calibration:", cls)) +
        theme_minimal() +
        coord_equal() +
        theme(plot.title = element_text(size = 10))
    }
    
    # Save calibration plots
    before_grid <- grid.arrange(
      grobs = cal_plots_before,
      ncol = length(cal_plots_before),
      top = "Before Calibration"
    )
    
    after_grid <- grid.arrange(
      grobs = cal_plots_after,
      ncol = length(cal_plots_after),
      top = "After Calibration"
    )
    
    comparison_grid <- grid.arrange(before_grid, after_grid, nrow = 2)
    
    ggsave(
      here(paste0("figures/calibration/", model_id, "_calibration.png")),
      comparison_grid,
      width = min(12, 3 * length(cal_plots_before)),
      height = 8,
      dpi = 300
    )
    
    # Apply calibration to all test data
    cal_results <- results
    
    for (cls in levels(results$Actual)) {
      # Get probability column
      prob_col <- paste0(".pred_", cls)
      cal_col <- paste0(".cal_", cls)
      
      # Create binary dataset for this class
      binary_df <- tibble(
        prob = results[[prob_col]]
      )
      
      # Apply calibration
      cal_probs <- cal_apply(binary_df, cal_estimators[[cls]])
      
      # Add calibrated probabilities to results
      cal_results[[cal_col]] <- cal_probs$cal_prob
    }
    
    # Determine new predictions based on calibrated probabilities
    cal_probs_only <- cal_results %>%
      select(starts_with(".cal_"))
    
    cal_pred_class <- apply(cal_probs_only, 1, function(row) {
      levels(results$Actual)[which.max(row)]
    })
    
    cal_results$Calibrated <- factor(cal_pred_class, levels = levels(results$Actual))
    
    # Calculate metrics for calibrated predictions
    cal_acc <- accuracy_vec(cal_results$Actual, cal_results$Calibrated)
    cal_kap <- kap_vec(cal_results$Actual, cal_results$Calibrated)
    
    message("  Calibrated accuracy: ", round(cal_acc, 4))
    
    # Create confusion matrix
    conf_before <- conf_mat(results, truth = Actual, estimate = Predicted)
    conf_after <- conf_mat(cal_results, truth = Actual, estimate = Calibrated)
    
    # Calculate class-specific metrics
    class_metrics_before <- map_dfr(levels(results$Actual), function(cls) {
      binary_actual <- factor(ifelse(results$Actual == cls, "Yes", "No"), levels = c("Yes", "No"))
      binary_pred <- factor(ifelse(results$Predicted == cls, "Yes", "No"), levels = c("Yes", "No"))
      
      tibble(
        Class = cls,
        Sensitivity = sens_vec(binary_actual, binary_pred),
        Specificity = spec_vec(binary_actual, binary_pred),
        Precision = precision_vec(binary_actual, binary_pred),
        F1 = f_meas_vec(binary_actual, binary_pred),
        Type = "Before"
      )
    })
    
    class_metrics_after <- map_dfr(levels(cal_results$Actual), function(cls) {
      binary_actual <- factor(ifelse(cal_results$Actual == cls, "Yes", "No"), levels = c("Yes", "No"))
      binary_pred <- factor(ifelse(cal_results$Calibrated == cls, "Yes", "No"), levels = c("Yes", "No"))
      
      tibble(
        Class = cls,
        Sensitivity = sens_vec(binary_actual, binary_pred),
        Specificity = spec_vec(binary_actual, binary_pred),
        Precision = precision_vec(binary_actual, binary_pred),
        F1 = f_meas_vec(binary_actual, binary_pred),
        Type = "After"
      )
    })
    
    class_metrics <- bind_rows(class_metrics_before, class_metrics_after)
    
    # Create metrics visualization
    metrics_plot <- class_metrics %>%
      pivot_longer(
        cols = c(Sensitivity, Specificity, Precision, F1),
        names_to = "Metric",
        values_to = "Value"
      ) %>%
      ggplot(aes(x = Class, y = Value, fill = Type)) +
      geom_bar(stat = "identity", position = "dodge") +
      facet_wrap(~Metric) +
      labs(
        title = paste("Performance Metrics:", model_id),
        x = "Class",
        y = "Value",
        fill = "Calibration"
      ) +
      scale_fill_manual(values = c("Before" = "#3498db", "After" = "#e74c3c")) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)
      )
    
    ggsave(
      here(paste0("figures/calibration/", model_id, "_metrics.png")),
      metrics_plot,
      width = 10,
      height = 6,
      dpi = 300
    )
    
    # Create calibrated model object
    calibrated_model <- list(
      original_model = model,
      cal_estimators = cal_estimators,
      metrics = list(
        before = list(
          accuracy = acc,
          kappa = kap,
          class_metrics = class_metrics_before
        ),
        after = list(
          accuracy = cal_acc,
          kappa = cal_kap,
          class_metrics = class_metrics_after
        )
      ),
      
      # Function to make predictions with calibrated model
      predict = function(new_data, type = "class") {
        # Get raw probabilities from original model
        raw_probs <- predict(model, new_data, type = "prob")
        
        # Apply calibration to each class
        cal_probs <- raw_probs
        
        for (cls in names(cal_estimators)) {
          # Get probability column
          prob_col <- paste0(".pred_", cls)
          cal_col <- paste0(".cal_", cls)
          
          # Create binary dataset for this class
          binary_df <- tibble(
            prob = raw_probs[[prob_col]]
          )
          
          # Apply calibration
          cal_result <- cal_apply(binary_df, cal_estimators[[cls]])
          
          # Replace with calibrated probabilities
          cal_probs[[prob_col]] <- cal_result$cal_prob
        }
        
        # If class predictions requested
        if (type == "class") {
          # Get class with highest probability
          pred_class <- apply(cal_probs, 1, function(row) {
            levels(results$Actual)[which.max(row)]
          })
          
          return(factor(pred_class, levels = levels(results$Actual)))
        } 
        # If probability predictions requested
        else if (type == "prob") {
          return(cal_probs)
        }
        else {
          stop("Invalid prediction type. Use 'class' or 'prob'.")
        }
      }
    )
    
    # Save calibrated model
    saveRDS(
      calibrated_model,
      here(paste0("data/models/calibrated/", model_id, "_calibrated.rds"))
    )
    
    # Store results
    calibration_results[[model_id]] <- list(
      test_results = cal_results,
      class_metrics = class_metrics,
      calibrated_model = calibrated_model
    )
  }
  
  # Save overall results
  saveRDS(
    calibration_results,
    here("data/results/calibration/all_calibration_results.rds")
  )
  
  # Create comprehensive comparison visualization
  create_comparison_visualization(calibration_results)
  
  return(calibration_results)
}

# Create comprehensive comparison visualization (continued)
create_comparison_visualization <- function(calibration_results) {
  # Extract metrics for all models
  all_metrics <- map_dfr(names(calibration_results), function(model_id) {
    # Get class metrics
    class_metrics <- calibration_results[[model_id]]$class_metrics
    
    # Add model identifier
    class_metrics %>%
      mutate(Model_ID = model_id)
  })
  
  # Create performance change visualization
  perf_change <- all_metrics %>%
    select(Model_ID, Class, Type, F1) %>%
    pivot_wider(
      names_from = Type,
      values_from = F1
    ) %>%
    mutate(
      Change = After - Before,
      Direction = ifelse(Change >= 0, "Improved", "Decreased")
    )
  
  # Parse model components
  model_components <- perf_change %>%
    mutate(
      Data_Source = gsub("_.*$", "", Model_ID),
      Model_Type = gsub("^.*_", "", Model_ID)
    )
  
  # F1 score heatmap
  f1_heatmap <- all_metrics %>%
    filter(Type == "After") %>%
    ggplot(aes(x = Class, y = Model_ID, fill = F1)) +
    geom_tile(color = "white", size = 0.2) +
    geom_text(aes(label = sprintf("%.2f", F1)), color = "white", size = 3) +
    scale_fill_viridis(option = "plasma", limits = c(0, 1)) +
    labs(
      title = "F1 Scores After Calibration",
      x = "Watershed",
      y = "Model",
      fill = "F1 Score"
    ) +
    theme_minimal() +
    theme(
      axis.text.y = element_text(hjust = 1),
      legend.position = "right",
      plot.title = element_text(hjust = 0.5)
    )
  
  # Performance change by model
  perf_change_plot <- model_components %>%
    group_by(Model_ID) %>%
    summarize(
      Mean_Change = mean(Change),
      Data_Source = first(Data_Source),
      Model_Type = first(Model_Type)
    ) %>%
    ggplot(aes(x = reorder(Model_ID, Mean_Change), y = Mean_Change, fill = Data_Source)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = sprintf("%+.3f", Mean_Change)), vjust = ifelse(.$Mean_Change > 0, -0.5, 1.5)) +
    labs(
      title = "Mean F1 Score Change After Calibration",
      x = NULL,
      y = "Mean Change",
      fill = "Data Source"
    ) +
    scale_fill_brewer(palette = "Set2") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(hjust = 0.5)
    )
  
  # Performance change by watershed
  watershed_change_plot <- model_components %>%
    group_by(Class) %>%
    summarize(
      Mean_Change = mean(Change),
      SD_Change = sd(Change)
    ) %>%
    ggplot(aes(x = reorder(Class, Mean_Change), y = Mean_Change)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    geom_errorbar(aes(ymin = Mean_Change - SD_Change, ymax = Mean_Change + SD_Change), width = 0.2) +
    geom_text(aes(label = sprintf("%+.3f", Mean_Change)), vjust = ifelse(.$Mean_Change > 0, -0.5, 1.5)) +
    labs(
      title = "Mean F1 Score Change by Watershed",
      x = "Watershed",
      y = "Mean Change"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(hjust = 0.5)
    )
  
  # Combine plots
  combined_plot <- grid.arrange(
    f1_heatmap,
    grid.arrange(perf_change_plot, watershed_change_plot, ncol = 2),
    nrow = 2,
    heights = c(1.5, 1),
    top = "Model Calibration Performance Summary"
  )
  
  # Save the combined visualization
  ggsave(
    here("figures/calibration/calibration_performance_summary.png"),
    combined_plot,
    width = 12,
    height = 10,
    dpi = 300
  )
  
  # Save metrics to CSV
  write_csv(
    all_metrics,
    here("data/results/calibration/all_calibration_metrics.csv")
  )
  
  return(combined_plot)
}

# Main execution
main <- function() {
  # Load or create all necessary data
  message("Loading data...")
  all_data <- load_all_data()
  
  # Check for existing trained models
  message("Checking for existing models...")
  existing_models <- load_models()
  
  # Check for existing train/test split
  split_file <- here("data/split_data.rds")
  if (file.exists(split_file)) {
    message("Loading existing train/test split...")
    split_data <- readRDS(split_file)
  } else {
    message("Creating new train/test split...")
    split_data <- create_split(all_data)
  }
  
  # Use existing models or train new ones
  if (!is.null(existing_models) && length(existing_models) > 0) {
    message("Using existing trained models...")
    models <- existing_models
  } else {
    message("Training new models...")
    models <- train_models(split_data)
  }
  
  # Calibrate models
  message("Calibrating models...")
  calibration_results <- calibrate_models(models, split_data)
  
  message("Model tuning completed successfully!")
  return(calibration_results)
}

# Run the main function
calibration_results <- main()