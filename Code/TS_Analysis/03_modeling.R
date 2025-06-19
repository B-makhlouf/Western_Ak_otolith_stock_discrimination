# 03_modeling.R
# Unified training of time series models using tidymodels framework
# Focused only on accuracy metrics

library(tidyverse)
library(tidymodels)
library(here)
library(viridis)
library(patchwork)
library(scales)

# Create all necessary directories at the start
create_directories <- function() {
  dirs <- c(
    "data/models",
    "data/results",
    "figures/models",
    "figures/models/by_model"
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

# Function to create model performance visualizations
create_model_visualizations <- function(summary_metrics) {
  # Create 1. Overall accuracy comparison
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
    scale_fill_manual(values = c("#DB3A34", "#FF784F", "#508484", "#79C99E", "#DB9D47")) +
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
    scale_fill_manual(values = c("#DB3A34", "#FF784F", "#508484")) +
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
  
  return(list(p1 = p1, p2 = p2))
}

# Function to create model accuracy heatmap with blue-yellow-red scale
create_accuracy_heatmap <- function(summary_metrics, save_path = "figures/models/model_accuracy_heatmap.png") {
  # Prepare data for visualization
  accuracy_summary <- summary_metrics %>%
    mutate(
      Data_Source = factor(Data_Source, 
                           levels = c("GAM", "MA", "RAW", "Sr88", "Combined", "Outline")),
      Model_Type = factor(Model_Type, 
                          levels = c("rf", "svm", "knn"),
                          labels = c("Random Forest", "SVM", "KNN"))
    )
  
  # Find min and max accuracy for better scaling
  min_acc <- min(accuracy_summary$Accuracy)
  max_acc <- max(accuracy_summary$Accuracy)
  mid_point <- (min_acc + max_acc) / 2
  
  # Set custom breaks to emphasize differences at the high end
  custom_breaks <- seq(
    from = floor(min_acc * 100) / 100,  # Round down to nearest 0.01
    to = ceiling(max_acc * 100) / 100,  # Round up to nearest 0.01
    length.out = 5                       # Use 5 distinct color levels
  )
  
  # Create the heatmap with blue-yellow-red color scale
  heatmap_plot <- ggplot(accuracy_summary, aes(x = Model_Type, y = Data_Source, fill = Accuracy)) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_text(aes(label = sprintf("%.3f", Accuracy)), 
              color = "black", size = 3.5, fontface = "bold") +
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
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      axis.title = element_text(face = "bold"),
      axis.text = element_text(size = 10),
      panel.grid = element_blank(),
      legend.position = "right",
      legend.key.height = unit(1.5, "cm")
    )
  
  # Save the plot
  ggsave(
    save_path,
    heatmap_plot,
    width = 10,
    height = 8,
    dpi = 300
  )
  
  return(heatmap_plot)
}

# Function to create confusion matrices with blue-yellow-red color scale
create_confusion_matrices <- function(all_metrics, save_path = "figures/models/confusion_matrices.png") {
  # For each data source, use the Random Forest model (typically best performing)
  rf_models <- grep("_rf$", names(all_metrics), value = TRUE)
  
  # Create confusion matrix for each model
  conf_plots <- map(rf_models, function(model_id) {
    # Get predictions
    predictions <- all_metrics[[model_id]]$predictions
    
    # Calculate confusion matrix
    conf_mat <- predictions %>%
      count(Watershed, .pred_class) %>%
      group_by(Watershed) %>%
      mutate(Percent = n / sum(n)) %>%
      ungroup()
    
    # Get accuracy
    acc <- mean(predictions$correct)
    
    # Get data source for labeling
    data_source <- strsplit(model_id, "_")[[1]][1]
    
    # Create the plot with blue-yellow-red color scale
    ggplot(conf_mat, aes(x = .pred_class, y = Watershed, fill = Percent)) +
      geom_tile() +
      geom_text(aes(label = sprintf("%d\n(%.1f%%)", n, Percent * 100)), 
                color = "black", size = 3.5) +
      # Blue-yellow-red scale
      scale_fill_gradientn(
        colors = c("dodgerblue4", "dodgerblue", "yellow", "orange", "firebrick"),
        limits = c(0, 1)
      ) +
      labs(
        title = paste0(data_source, "-RF (Acc: ", scales::percent(acc, accuracy = 0.1), ")"),
        x = "Predicted",
        y = "Actual",
        fill = "Percent"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5),
        axis.text = element_text(size = 10)
      )
  })
  
  # Determine layout
  n_plots <- length(conf_plots)
  n_cols <- min(3, n_plots)
  n_rows <- ceiling(n_plots / n_cols)
  
  # Combine plots
  combined_plot <- wrap_plots(conf_plots, ncol = n_cols) +
    plot_annotation(
      title = "Confusion Matrices for Random Forest Models",
      theme = theme(plot.title = element_text(hjust = 0.5, size = 16))
    )
  
  # Save the plot
  ggsave(
    save_path,
    combined_plot,
    width = min(16, 5 * n_cols),
    height = min(16, 4 * n_rows),
    dpi = 300
  )
  
  return(combined_plot)
}

# Function to create class-specific accuracy heatmap with blue-yellow-red scale
create_class_accuracy_heatmap <- function(all_metrics, save_path = "figures/models/class_accuracy_heatmap.png") {
  # Calculate class-specific accuracy for each model
  class_accuracy <- map_dfr(names(all_metrics), function(model_id) {
    # Parse model components
    parts <- strsplit(model_id, "_")[[1]]
    data_source <- parts[1]
    model_type <- parts[2]
    
    # Get predictions
    preds <- all_metrics[[model_id]]$predictions
    
    # Calculate accuracy for each class
    preds %>%
      group_by(Watershed) %>%
      summarize(
        Accuracy = mean(correct),
        .groups = "drop"
      ) %>%
      mutate(
        Model_ID = model_id,
        Data_Source = data_source,
        Model_Type = model_type
      )
  })
  
  # Create a more readable model label
  class_accuracy <- class_accuracy %>%
    mutate(
      Model_Label = paste0(
        ifelse(Data_Source == "Combined", "Combined", 
               ifelse(Data_Source == "Outline", "Shape", Data_Source)),
        "-",
        ifelse(Model_Type == "rf", "RF", 
               ifelse(Model_Type == "svm", "SVM", 
                      ifelse(Model_Type == "knn", "KNN", Model_Type)))
      ),
      Model_Label = factor(Model_Label)
    )
  
  # Calculate min, max, and quartiles for more informative color scaling
  min_acc <- min(class_accuracy$Accuracy)
  max_acc <- max(class_accuracy$Accuracy)
  q1 <- quantile(class_accuracy$Accuracy, 0.25)
  median_acc <- median(class_accuracy$Accuracy)
  q3 <- quantile(class_accuracy$Accuracy, 0.75)
  
  # Create custom breaks centered more on the higher end
  custom_breaks <- c(
    floor(min_acc * 100) / 100,  # Round down to nearest 0.01
    floor(q1 * 100) / 100,       # First quartile
    floor(median_acc * 100) / 100, # Median
    floor(q3 * 100) / 100,       # Third quartile
    ceiling(max_acc * 100) / 100  # Round up to nearest 0.01
  )
  
  # Create the heatmap with blue-yellow-red color scale
  heatmap_plot <- ggplot(class_accuracy, aes(x = Watershed, y = Model_Label, fill = Accuracy)) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_text(aes(label = sprintf("%.2f", Accuracy)), 
              color = "black", size = 3, fontface = "bold") +
    # Blue-yellow-red scale
    scale_fill_gradientn(
      colors = c("dodgerblue4", "dodgerblue", "yellow", "orange", "firebrick"),
      values = scales::rescale(c(min_acc, q1, median_acc, q3, max_acc)),
      limits = c(min_acc * 0.99, max_acc * 1.01),
      breaks = custom_breaks,
      labels = scales::percent_format(accuracy = 0.1)
    ) +
    labs(
      title = "Watershed-Specific Classification Accuracy",
      x = "Watershed",
      y = "Model",
      fill = "Accuracy"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      axis.title = element_text(face = "bold"),
      axis.text.y = element_text(size = 8),
      panel.grid = element_blank(),
      legend.position = "right",
      legend.key.height = unit(1.5, "cm")
    )
  
  # Save the plot
  ggsave(
    save_path,
    heatmap_plot,
    width = 12,
    height = 10,
    dpi = 300
  )
  
  return(heatmap_plot)
}

# Function to create model-specific visualizations
create_model_specific_visualizations <- function(all_metrics, output_dir = "figures/models/by_model") {
  # Create output directory if it doesn't exist
  dir.create(here(output_dir), recursive = TRUE, showWarnings = FALSE)
  
  # Process each model
  for (model_id in names(all_metrics)) {
    # Parse model components
    parts <- strsplit(model_id, "_")[[1]]
    data_source <- parts[1]
    model_type <- parts[2]
    
    # Create a readable model name
    model_name <- paste0(
      ifelse(data_source == "Combined", "Combined", 
             ifelse(data_source == "Outline", "Shape", data_source)),
      "-",
      ifelse(model_type == "rf", "Random Forest", 
             ifelse(model_type == "svm", "SVM", 
                    ifelse(model_type == "knn", "KNN", model_type)))
    )
    
    # Get predictions
    preds <- all_metrics[[model_id]]$predictions
    
    # Calculate overall accuracy
    overall_acc <- mean(preds$correct)
    
    # Calculate class-specific accuracy
    class_acc <- preds %>%
      group_by(Watershed) %>%
      summarize(
        Accuracy = mean(correct),
        Count = n(),
        .groups = "drop"
      )
    
    # Make sure all classes are represented, even with 0 accuracy
    all_classes <- unique(preds$Watershed)
    missing_classes <- setdiff(all_classes, class_acc$Watershed)
    
    if (length(missing_classes) > 0) {
      missing_df <- data.frame(
        Watershed = missing_classes,
        Accuracy = 0,
        Count = 0
      )
      class_acc <- bind_rows(class_acc, missing_df)
    }
    
    # Create confusion matrix
    conf_mat <- preds %>%
      count(Watershed, .pred_class) %>%
      complete(Watershed, .pred_class, fill = list(n = 0)) %>%  # Ensure all combinations exist
      group_by(Watershed) %>%
      mutate(Percent = n / sum(n)) %>%
      ungroup()
    
    # Create a visualization with both class accuracy and confusion matrix
    
    # Class accuracy plot with your custom color scheme
    p1 <- ggplot(class_acc, aes(x = Watershed, y = Accuracy, fill = Watershed)) +
      geom_bar(stat = "identity", width = 0.7) +
      geom_text(aes(label = sprintf("%.2f", Accuracy)), 
                vjust = -0.5, size = 4) +
      scale_fill_manual(values = c("Kuskokwim" = "#DB3A34", 
                                   "Nushagak" = "#508484", 
                                   "Yukon" = "#79C99E")) +
      labs(
        title = "Class-Specific Accuracy",
        x = "Watershed",
        y = "Accuracy"
      ) +
      ylim(0, 1) +  # Fixed y-axis range
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 14),
        legend.position = "none"
      )
    
    # Confusion matrix plot with blue-yellow-red color scale
    p2 <- ggplot(conf_mat, aes(x = .pred_class, y = Watershed, fill = Percent)) +
      geom_tile(color = "white") +
      geom_text(aes(label = sprintf("%d\n(%.1f%%)", n, Percent * 100)), 
                color = "black", size = 3.5) +
      scale_fill_gradientn(
        colors = c("dodgerblue4", "dodgerblue", "yellow", "orange", "firebrick"),
        limits = c(0, 1),
        labels = scales::percent_format()
      ) +
      labs(
        title = "Confusion Matrix",
        x = "Predicted",
        y = "Actual",
        fill = "Percent"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 14),
        axis.text = element_text(size = 10)
      )
    
    # Combine plots
    combined_plot <- p1 / p2 +
      plot_layout(heights = c(1, 1.5)) +
      plot_annotation(
        title = paste0(model_name, " (Overall Accuracy: ", scales::percent(overall_acc, accuracy = 0.1), ")"),
        theme = theme(plot.title = element_text(hjust = 0.5, size = 16))
      )
    
    # Save the combined plot
    filename <- paste0(output_dir, "/", model_id, "_performance.png")
    ggsave(
      here(filename),
      combined_plot,
      width = 10,
      height = 12,
      dpi = 300
    )
  }
  
  # Return success message
  return(paste("Created individual visualizations for", length(all_metrics), "models in", output_dir))
}

# Function to create watershed accuracy plot
create_watershed_accuracy_plot <- function(all_metrics, save_path = "figures/models/watershed_accuracy.png") {
  # Calculate accuracy by watershed for each model
  watershed_acc <- map_dfr(names(all_metrics), function(model_id) {
    # Get model info
    model_parts <- strsplit(model_id, "_")[[1]]
    data_source <- model_parts[1]
    model_type <- model_parts[2]
    
    # Get predictions
    preds <- all_metrics[[model_id]]$predictions
    
    # Calculate accuracy for each watershed
    preds %>%
      group_by(Watershed) %>%
      summarize(
        Accuracy = mean(correct),
        .groups = "drop"
      ) %>%
      mutate(
        Model_ID = model_id,
        Data_Source = data_source,
        Model_Type = model_type
      )
  })
  
  # Create a more readable model label
  watershed_acc <- watershed_acc %>%
    mutate(
      Model_Label = paste0(
        ifelse(Data_Source == "Combined", "Combined", 
               ifelse(Data_Source == "Outline", "Shape", Data_Source)),
        "-",
        ifelse(Model_Type == "rf", "RF", 
               ifelse(Model_Type == "svm", "SVM", 
                      ifelse(Model_Type == "knn", "KNN", Model_Type)))
      )
    )
  
  # Create the plot with your custom color scheme
  watershed_plot <- ggplot(watershed_acc, aes(x = Watershed, y = Accuracy, fill = Model_Label)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    geom_text(aes(label = sprintf("%.2f", Accuracy)), 
              position = position_dodge(width = 0.9),
              vjust = -0.5, size = 3) +
    scale_fill_manual(values = c("#DB3A34", "#FF784F", "#508484", "#79C99E", "#DB9D47")) +
    labs(
      title = "Classification Accuracy by Watershed",
      x = "Watershed",
      y = "Accuracy",
      fill = "Model"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      axis.text.x = element_text(angle = 0),
      legend.position = "right"
    )
  
  # Save the plot
  ggsave(
    save_path,
    watershed_plot,
    width = 12,
    height = 8,
    dpi = 300
  )
  
  return(watershed_plot)
}

# Train models for each time series data source
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
      predictions <- predict(final_fit, test_data, type = "prob") %>%
        bind_cols(predict(final_fit, test_data)) %>%
        bind_cols(test_data %>% select(Fish_id, Watershed)) %>%
        mutate(correct = Watershed == .pred_class)
      
      # Calculate accuracy
      acc <- mean(predictions$Watershed == predictions$.pred_class)
      
      # Store results
      all_models[[model_id]] <- final_fit
      all_metrics[[model_id]] <- list(
        accuracy = acc,
        predictions = predictions,
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
  model_viz1 <- create_model_visualizations(summary_metrics)      # Original visualizations
  model_viz2 <- create_accuracy_heatmap(summary_metrics)          # Accuracy heatmap
  model_viz3 <- create_confusion_matrices(all_metrics)            # Confusion matrices
  model_viz4 <- create_class_accuracy_heatmap(all_metrics)        # Class accuracy heatmap
  model_viz5 <- create_model_specific_visualizations(all_metrics) # Model-specific visualizations
  
  message("Model visualizations created and saved to figures/models/")
  
  return(list(
    models = all_models,
    metrics = all_metrics,
    summary = summary_metrics
  ))
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
  
  # Create watershed-specific accuracy visualization
  create_watershed_accuracy_plot(model_results$metrics)
  
  message("Time series modeling completed successfully!")
  return(model_results)
}

# Run the main function
ts_model_results <- main()
