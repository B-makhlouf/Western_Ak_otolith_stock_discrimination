# 05_evaluation.R
# Comprehensive evaluation and visualization of model results

library(tidyverse)
library(tidymodels)
library(viridis)
library(patchwork)
library(here)

# Load results
all_metrics <- readRDS(here("data/results/all_model_metrics.rds"))
calibrated_metrics <- readRDS(here("data/results/calibrated_metrics.rds"))
ensemble_results <- readRDS(here("data/results/ensemble_predictions.rds"))
ensemble_metrics <- readRDS(here("data/results/ensemble_metrics.rds"))

# Create comprehensive comparison visualization
create_model_comparison <- function(all_metrics, calibrated_metrics) {
  # Extract metrics for uncalibrated models
  uncal_metrics <- map_dfr(names(all_metrics), function(model_id) {
    # Parse model components
    parts <- strsplit(model_id, "_")[[1]]
    data_source <- parts[1]
    model_type <- parts[2]
    
    # Get accuracy
    accuracy <- all_metrics[[model_id]]$metrics$Accuracy
    
    # Get class-specific metrics
    predictions <- all_metrics[[model_id]]$predictions
    
    # Calculate metrics for each class
    class_metrics <- map_dfr(levels(predictions$Actual), function(cls) {
      # Binary classification: target class vs rest
      binary_actual <- factor(ifelse(predictions$Actual == cls, "Yes", "No"), 
                              levels = c("Yes", "No"))
      binary_pred <- factor(ifelse(predictions$Predicted == cls, "Yes", "No"), 
                            levels = c("Yes", "No"))
      
      # Calculate metrics
      tibble(
        Class = cls,
        Sensitivity = sens_vec(binary_actual, binary_pred),
        Specificity = spec_vec(binary_actual, binary_pred),
        Precision = precision_vec(binary_actual, binary_pred),
        F1 = f_meas_vec(binary_actual, binary_pred)
      )
    })
    
    # Combine with model info
    class_metrics %>%
      mutate(
        Model_ID = model_id,
        Data_Source = data_source,
        Model_Type = model_type,
        Calibration = "Uncalibrated"
      )
  })
  
  # Extract metrics for calibrated models
  cal_metrics <- map_dfr(names(calibrated_metrics), function(model_id) {
    # Parse model components
    parts <- strsplit(model_id, "_")[[1]]
    data_source <- parts[1]
    model_type <- parts[2]
    
    # Get accuracy
    accuracy <- calibrated_metrics[[model_id]]$metrics$Accuracy
    
    # Get class-specific metrics
    predictions <- calibrated_metrics[[model_id]]$predictions
    
    # Calculate metrics for each class
    class_metrics <- map_dfr(levels(predictions$Actual), function(cls) {
      # Binary classification: target class vs rest
      binary_actual <- factor(ifelse(predictions$Actual == cls, "Yes", "No"), 
                              levels = c("Yes", "No"))
      binary_pred <- factor(ifelse(predictions$Calibrated == cls, "Yes", "No"), 
                            levels = c("Yes", "No"))
      
      # Calculate metrics
      tibble(
        Class = cls,
        Sensitivity = sens_vec(binary_actual, binary_pred),
        Specificity = spec_vec(binary_actual, binary_pred),
        Precision = precision_vec(binary_actual, binary_pred),
        F1 = f_meas_vec(binary_actual, binary_pred)
      )
    })
    
    # Combine with model info
    class_metrics %>%
      mutate(
        Model_ID = model_id,
        Data_Source = data_source,
        Model_Type = model_type,
        Calibration = "Calibrated"
      )
  })
  
  # Combine all metrics
  all_comparison <- bind_rows(uncal_metrics, cal_metrics)
  
  # Add ensemble metrics
  ensemble_class_metrics <- map_dfr(levels(ensemble_results$Actual), function(cls) {
    # Binary classification: target class vs rest
    binary_actual <- factor(ifelse(ensemble_results$Actual == cls, "Yes", "No"), 
                            levels = c("Yes", "No"))
    binary_pred <- factor(ifelse(ensemble_results$Ensemble == cls, "Yes", "No"), 
                          levels = c("Yes", "No"))
    
    # Calculate metrics
    tibble(
      Class = cls,
      Sensitivity = sens_vec(binary_actual, binary_pred),
      Specificity = spec_vec(binary_actual, binary_pred),
      Precision = precision_vec(binary_actual, binary_pred),
      F1 = f_meas_vec(binary_actual, binary_pred),
      Model_ID = "Ensemble",
      Data_Source = "Ensemble",
      Model_Type = "Ensemble",
      Calibration = "Ensemble"
    )
  })
  
  all_comparison <- bind_rows(all_comparison, ensemble_class_metrics)
  
  # Create visualizations
  
  # 1. Heatmap of F1 scores by model and class
  p1 <- all_comparison %>%
    mutate(
      Model_Label = paste0(Data_Source, "-", Model_Type, " (", Calibration, ")"),
      Model_Label = factor(Model_Label, levels = unique(Model_Label)[order(unique(Model_ID))])
    ) %>%
    ggplot(aes(x = Class, y = Model_Label, fill = F1)) +
    geom_tile(color = "white", size = 0.2) +
    geom_text(aes(label = sprintf("%.2f", F1)), color = "white", size = 3) +
    scale_fill_viridis(option = "plasma", limits = c(0, 1)) +
    labs(
      title = "F1 Scores by Model and Watershed Class",
      x = "Watershed",
      y = NULL,
      fill = "F1 Score"
    ) +
    theme_minimal() +
    theme(
      axis.text.y = element_text(hjust = 1),
      legend.position = "right",
      plot.title = element_text(hjust = 0.5)
    )
  
  # 2. Performance comparison before/after calibration
  p2 <- all_comparison %>%
    filter(Calibration %in% c("Uncalibrated", "Calibrated")) %>%
    group_by(Data_Source, Model_Type, Calibration) %>%
    summarize(
      Mean_F1 = mean(F1),
      Mean_Precision = mean(Precision),
      Mean_Sensitivity = mean(Sensitivity),
      Mean_Specificity = mean(Specificity),
      .groups = "drop"
    ) %>%
    pivot_longer(
      cols = starts_with("Mean_"),
      names_to = "Metric",
      values_to = "Value"
    ) %>%
    mutate(
      Metric = gsub("Mean_", "", Metric),
      Model = paste(Data_Source, Model_Type, sep = "_")
    ) %>%
    ggplot(aes(x = Model, y = Value, fill = Calibration, group = interaction(Model, Calibration))) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
    facet_wrap(~Metric) +
    labs(
      title = "Model Performance Before and After Calibration",
      x = NULL,
      y = "Mean Value",
      fill = NULL
    ) +
    scale_fill_manual(values = c("Uncalibrated" = "#3498db", "Calibrated" = "#e74c3c")) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5)
    )
  
  # 3. Ensemble model performance
  p3 <- ensemble_results %>%
    pivot_longer(
      cols = c(Ensemble, High_Conf_Prediction),
      names_to = "Prediction_Type",
      values_to = "Prediction"
    ) %>%
    mutate(
      Correct = Prediction == Actual,
      Prediction_Type = factor(Prediction_Type, 
                               levels = c("Ensemble", "High_Conf_Prediction"),
                               labels = c("All Predictions", "High Confidence Only"))
    ) %>%
    ggplot(aes(x = Actual, fill = Correct)) +
    geom_bar(position = "fill") +
    facet_wrap(~Prediction_Type) +
    scale_fill_manual(values = c("TRUE" = "#2ecc71", "FALSE" = "#e74c3c")) +
    labs(
      title = "Ensemble Model Performance by Watershed",
      x = "Actual Watershed",
      y = "Proportion",
      fill = "Correct?"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5)
    )
  
  # Combine visualizations
  full_viz <- (p1 / p2 / p3) +
    plot_layout(heights = c(1, 1, 1)) +
    plot_annotation(
      title = "Comprehensive Model Evaluation",
      theme = theme(plot.title = element_text(hjust = 0.5, size = 16))
    )
  
  # Save visualizations
  # Create directory if it doesn't exist
  viz_dir <- here("figures")
  dir.create(viz_dir, recursive = TRUE, showWarnings = FALSE)
  
  ggsave(
    file.path(viz_dir, "model_f1_heatmap.png"),
    p1,
    width = 10,
    height = 8,
    dpi = 300
  )
  
  ggsave(
    file.path(viz_dir, "calibration_comparison.png"),
    p2,
    width = 10,
    height = 6,
    dpi = 300
  )
  
  ggsave(
    file.path(viz_dir, "ensemble_performance.png"),
    p3,
    width = 10,
    height = 6,
    dpi = 300
  )
  
  ggsave(
    file.path(viz_dir, "comprehensive_evaluation.png"),
    full_viz,
    width = 12,
    height = 18,
    dpi = 300
  )
  
  # Save metric data
  write_csv(all_comparison, here("data/results/all_metrics_comparison.csv"))
  
  return(all_comparison)
}

# Create confusion matrix visualization
create_confusion_matrices <- function(ensemble_results) {
  # Generate confusion matrix for ensemble
  conf_mat_data <- ensemble_results %>%
    count(Actual, Ensemble) %>%
    group_by(Actual) %>%
    mutate(Percent = n / sum(n)) %>%
    ungroup()
  
  # Plot confusion matrix
  conf_plot <- ggplot(conf_mat_data, aes(x = Ensemble, y = Actual, fill = Percent)) +
    geom_tile() +
    geom_text(aes(label = sprintf("%d\n(%.1f%%)", n, Percent * 100)), 
              color = "white", size = 3) +
    scale_fill_viridis(option = "plasma", limits = c(0, 1)) +
    labs(
      title = "Confusion Matrix - Ensemble Model",
      x = "Predicted",
      y = "Actual",
      fill = "Percent"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  # Generate confusion matrix for high confidence predictions
  high_conf_data <- ensemble_results %>%
    filter(High_Conf_Prediction != "Unknown") %>%
    count(Actual, High_Conf_Prediction) %>%
    group_by(Actual) %>%
    mutate(Percent = n / sum(n)) %>%
    ungroup()
  
  # Plot high confidence confusion matrix
  high_conf_plot <- ggplot(high_conf_data, aes(x = High_Conf_Prediction, y = Actual, fill = Percent)) +
    geom_tile() +
    geom_text(aes(label = sprintf("%d\n(%.1f%%)", n, Percent * 100)), 
              color = "white", size = 3) +
    scale_fill_viridis(option = "plasma", limits = c(0, 1)) +
    labs(
      title = "Confusion Matrix - High Confidence Predictions Only",
      x = "Predicted",
      y = "Actual",
      fill = "Percent"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  

  # Create model accuracy heatmap specifically focusing on overall accuracy by model type and data source
  create_model_accuracy_heatmap <- function(all_metrics) {
    # Extract and summarize accuracy data by data source and model type
    accuracy_summary <- map_dfr(names(all_metrics), function(model_id) {
      # Parse model components
      parts <- strsplit(model_id, "_")[[1]]
      data_source <- parts[1]
      model_type <- parts[2]
      
      # Get overall accuracy
      accuracy <- all_metrics[[model_id]]$metrics$Accuracy
      
      # Return as data frame row
      tibble(
        Data_Source = data_source,
        Model_Type = model_type,
        Accuracy = accuracy
      )
    })
    
    # Standardize data source and model type labels
    accuracy_summary <- accuracy_summary %>%
      mutate(
        Data_Source = factor(Data_Source, 
                             levels = c("GAM", "MA", "RAW", "Sr88", "Combined", "Outline")),
        Model_Type = factor(Model_Type, 
                            levels = c("rf", "svm", "knn"),
                            labels = c("Random Forest", "SVM", "KNN"))
      )
    
    # Create the heatmap
    heatmap_plot <- ggplot(accuracy_summary, aes(x = Model_Type, y = Data_Source, fill = Accuracy)) +
      geom_tile(color = "white", linewidth = 0.5) +
      geom_text(aes(label = sprintf("%.3f", Accuracy)), 
                color = "white", size = 3.5, fontface = "bold") +
      scale_fill_viridis(
        option = "plasma",
        direction = -1,
        labels = scales::percent_format(),
        limits = c(0.45, 0.95),
        begin = 0.2,
        end = 0.9
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
    
    # Save visualization
    ggsave(
      here("figures/model_accuracy_heatmap.png"),
      heatmap_plot,
      width = 10,
      height = 8,
      dpi = 300
    )
    
    return(heatmap_plot)
  }
  
  
  
  
  # Combine plots
  combined_plot <- conf_plot + high_conf_plot +
    plot_layout(ncol = 2) +
    plot_annotation(
      title = "Confusion Matrices",
      theme = theme(plot.title = element_text(hjust = 0.5, size = 16))
    )
  
  # Save visualization
  ggsave(
    here("figures/confusion_matrices.png"),
    combined_plot,
    width = 12,
    height = 6,
    dpi = 300
  )
  
  return(combined_plot)
}

# Create probability calibration plots
create_calibration_plots <- function(calibrated_metrics) {
  # Extract model names
  model_names <- names(calibrated_metrics)
  
  # Create plots for each model
  cal_plots <- map(model_names, function(model_id) {
    # Get predictions
    preds <- calibrated_metrics[[model_id]]$predictions
    
    # Extract class labels
    classes <- levels(preds$Actual)
    
    # Create calibration curves for each class
    class_curves <- map(classes, function(cls) {
      # Extract probability column
      prob_col <- paste0(".cal_", cls)
      if (!prob_col %in% names(preds)) {
        return(NULL)
      }
      
      # Create calibration data
      cal_data <- tibble(
        actual = preds$Actual == cls,
        prob = preds[[prob_col]]
      )
      
      # Create calibration curve
      cal_plot_windowed(
        cal_data,
        truth = actual,
        estimate = prob,
        window_size = 0.1,
        step_size = 0.05
      ) +
        labs(
          title = paste("Class:", cls),
          x = "Predicted Probability",
          y = "Observed Frequency"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, size = 10)
        )
    })
    
    # Filter out NULL plots
    class_curves <- compact(class_curves)
    
    # Arrange class plots
    if (length(class_curves) > 0) {
      combined <- wrap_plots(class_curves) +
        plot_annotation(
          title = paste("Calibration Curves -", model_id),
          theme = theme(plot.title = element_text(hjust = 0.5))
        )
      
      return(combined)
    } else {
      return(NULL)
    }
  })
  
  # Filter out NULL plots
  cal_plots <- compact(cal_plots)
  
  # Arrange and save calibration plots
  for (i in seq_along(cal_plots)) {
    model_id <- model_names[i]
    ggsave(
      here(paste0("figures/calibration_curves_", model_id, ".png")),
      cal_plots[[i]],
      width = 10,
      height = 6,
      dpi = 300
    )
  }
  
  return(cal_plots)
}

# Create PCA visualization of otolith data
create_pca_visualization <- function() {
  # Load data
  gam_data <- read_csv(here("data/processed/matrices/preprocessed_GAM.csv"))
  outline_data <- read_csv(here("data/processed/outlines/fourier_coefficients.csv"))
  
  # PCA for GAM data
  gam_numeric <- gam_data %>%
    select(starts_with("X")) %>%
    as.matrix()
  
  gam_pca <- prcomp(gam_numeric, scale. = TRUE)
  
  gam_scores <- as.data.frame(gam_pca$x[, 1:3]) %>%
    bind_cols(
      Fish_id = gam_data$Fish_id,
      Watershed = gam_data$Watershed,
      Natal_Iso = gam_data$Natal_Iso
    )
  
  # Create PCA plot for GAM data
  p1 <- ggplot(gam_scores, aes(x = PC1, y = PC2, color = Watershed)) +
    geom_point(alpha = 0.7) +
    labs(
      title = "PCA of Isotope Profiles (GAM Smoothed)",
      x = "PC1",
      y = "PC2",
      color = "Watershed"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  
  # PCA for outline data
  outline_numeric <- outline_data %>%
    select(-watershed, -picname) %>%
    as.matrix()
  
  outline_pca <- prcomp(outline_numeric, scale. = TRUE)
  
  outline_scores <- as.data.frame(outline_pca$x[, 1:3]) %>%
    bind_cols(
      Fish_id = extract_fish_ids(outline_data$picname),
      Watershed = outline_data$watershed
    )
  
  # Create PCA plot for outline data
  p2 <- ggplot(outline_scores, aes(x = PC1, y = PC2, color = Watershed)) +
    geom_point(alpha = 0.7) +
    labs(
      title = "PCA of Otolith Outline Shape",
      x = "PC1",
      y = "PC2",
      color = "Watershed"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  
  # Combine plots
  combined_plot <- p1 + p2 +
    plot_layout(ncol = 2) +
    plot_annotation(
      title = "Principal Component Analysis",
      theme = theme(plot.title = element_text(hjust = 0.5, size = 16))
    )
  
  # Save visualization
  ggsave(
    here("figures/pca_visualization.png"),
    combined_plot,
    width = 12,
    height = 6,
    dpi = 300
  )
  
  return(combined_plot)
}

# Extract fish ID from picname
extract_fish_ids <- function(picnames) {
  # Extract year, watershed, and ID number from picnames
  # Format expected: YYYY_ww_NNN
  gsub("^(\\d{4}_[a-z]{2}_\\d+).*$", "\\1", picnames)
}

# Run all visualizations
accuracy_heatmap <- create_model_accuracy_heatmap(all_metrics)
metrics_comparison <- create_model_comparison(all_metrics, calibrated_metrics)
confusion_matrices <- create_confusion_matrices(ensemble_results)
calibration_plots <- create_calibration_plots(calibrated_metrics)
pca_viz <- create_pca_visualization()

message("Evaluation and visualization completed.")