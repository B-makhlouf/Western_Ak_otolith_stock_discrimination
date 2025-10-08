# Cross-Validation Model Evaluation Script - ACCURACY ONLY
# Uses EXISTING train/test splits but adds CV for robust evaluation
# Cross-validation is performed ONLY on training data
# NO MODEL SAVING - JUST COMPARISON

library(here)
library(dplyr)
library(tidymodels)
library(ranger)
library(kernlab)
library(kknn)

################################################################################
#### CONFIGURATION
################################################################################

set.seed(123)

# Define data types
data_types <- c("RAW", "GAM", "MA", "Sr88", "Combined")

# Define paths - READING FROM EXISTING SPLITS
train_test_dir_total <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/data/LA_Data/TrainingTesting"
train_test_dir_overlap <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/data/LA_Data/TrainingTesting/Filtered"

# Output directory for results only
results_dir_total <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/Output/ModelResults_CV/Total"
results_dir_overlap <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/Output/ModelResults_CV/Filtered"

# Create directories
dir.create(results_dir_total, recursive = TRUE, showWarnings = FALSE)
dir.create(results_dir_overlap, recursive = TRUE, showWarnings = FALSE)

################################################################################
#### MAIN FUNCTION: CV ANALYSIS - ACCURACY COMPARISON ONLY
################################################################################

run_cv_analysis <- function(train_test_dir, analysis_name, results_dir) {
  
  # Storage for results
  results <- data.frame()
  
  cat("\n", strrep("=", 80), "\n")
  cat("RUNNING", toupper(analysis_name), "ANALYSIS WITH CROSS-VALIDATION\n")
  cat(strrep("=", 80), "\n\n")
  
  # Loop through each dataset
  for (data_type in data_types) {
    
    cat("\n", strrep("-", 80), "\n")
    cat("PROCESSING:", data_type, "\n")
    cat(strrep("-", 80), "\n")
    
    # Load EXISTING train/test split
    train_file <- file.path(train_test_dir, paste0("Train_", data_type, ".csv"))
    test_file <- file.path(train_test_dir, paste0("Test_", data_type, ".csv"))
    
    if (!file.exists(train_file) || !file.exists(test_file)) {
      cat("✗ Files not found for", data_type, "- skipping\n")
      next
    }
    
    train_data <- read.csv(train_file) %>%
      mutate(Watershed = as.factor(Watershed))
    
    test_data <- read.csv(test_file) %>%
      mutate(Watershed = as.factor(Watershed))
    
    # Skip if no test data
    if (nrow(test_data) == 0) {
      cat("✗ No test data for", data_type, "- skipping\n")
      next
    }
    
    cat("  Training samples:", nrow(train_data), "\n")
    cat("  Test samples:", nrow(test_data), "\n")
    cat("  Features:", ncol(train_data) - 1, "\n")
    
    # Create CV folds from TRAINING DATA ONLY
    set.seed(123)
    folds <- vfold_cv(train_data, v = 10, strata = Watershed)
    cat("  Created 10-fold CV (stratified by Watershed)\n\n")
    
    # Recipe
    base_recipe <- recipe(Watershed ~ ., data = train_data)
    
    # Define models
    n_predictors <- ncol(train_data) - 1
    
    models <- list(
      RF = rand_forest(trees = 500, mtry = floor(sqrt(n_predictors))) %>% 
        set_engine("ranger") %>%
        set_mode("classification"),
      
      SVM = svm_rbf() %>% 
        set_engine("kernlab") %>% 
        set_mode("classification"),
      
      KNN = nearest_neighbor(neighbors = 5) %>% 
        set_engine("kknn") %>% 
        set_mode("classification")
    )
    
    # Train and evaluate each model
    for (model_name in names(models)) {
      
      cat("  Training", model_name, "...\n")
      
      set.seed(123)
      
      # Create workflow
      workflow_obj <- workflow() %>%
        add_recipe(base_recipe) %>%
        add_model(models[[model_name]])
      
      # ======================================================================
      # STEP 1: Cross-validation on TRAINING data (ACCURACY ONLY)
      # ======================================================================
      
      cv_results <- workflow_obj %>%
        fit_resamples(
          resamples = folds,
          metrics = metric_set(accuracy),
          control = control_resamples(save_pred = FALSE, verbose = FALSE)
        )
      
      # Extract CV metrics
      cv_metrics <- collect_metrics(cv_results)
      
      cv_accuracy <- cv_metrics %>% 
        filter(.metric == "accuracy") %>% 
        pull(mean)
      cv_accuracy_se <- cv_metrics %>% 
        filter(.metric == "accuracy") %>% 
        pull(std_err)
      
      # Calculate 95% confidence interval
      cv_accuracy_ci <- cv_accuracy_se * 1.96
      
      cat("    CV Accuracy:  ", sprintf("%.3f ± %.3f [%.3f - %.3f]", 
                                        cv_accuracy, cv_accuracy_ci,
                                        cv_accuracy - cv_accuracy_ci,
                                        cv_accuracy + cv_accuracy_ci), "\n")
      
      # ======================================================================
      # STEP 2: Train final model on FULL training set and test
      # ======================================================================
      
      final_model <- workflow_obj %>%
        fit(train_data)
      
      # Evaluate on HOLDOUT test set
      test_predictions <- final_model %>%
        predict(test_data) %>%
        bind_cols(test_data %>% select(Watershed))
      
      # Calculate test accuracy
      test_accuracy <- mean(test_predictions$Watershed == test_predictions$.pred_class)
      
      cat("    Test Accuracy:", sprintf("%.3f", test_accuracy), "\n")
      
      # Check for overfitting
      accuracy_diff <- cv_accuracy - test_accuracy
      if (accuracy_diff > 0.05) {
        cat("    ⚠ Warning: Possible overfitting (CV accuracy", sprintf("%.3f", accuracy_diff), "higher than test)\n")
      } else if (accuracy_diff < -0.05) {
        cat("    ⚠ Warning: Unusual pattern (Test accuracy higher than CV)\n")
      }
      
      cat("\n")
      
      # ======================================================================
      # STEP 3: Store results
      # ======================================================================
      
      results <- rbind(results, data.frame(
        Dataset = data_type,
        Model = model_name,
        CV_Accuracy = round(cv_accuracy, 4),
        CV_Accuracy_SE = round(cv_accuracy_se, 4),
        CV_Accuracy_CI = round(cv_accuracy_ci, 4),
        Test_Accuracy = round(test_accuracy, 4),
        Accuracy_Diff = round(cv_accuracy - test_accuracy, 4)
      ))
    }
  }
  
  # ========================================================================
  # SUMMARY AND REPORTING
  # ========================================================================
  
  cat("\n", strrep("=", 80), "\n")
  cat("SUMMARY:", toupper(analysis_name), "ANALYSIS\n")
  cat(strrep("=", 80), "\n\n")
  
  # Sort by test accuracy
  results <- results[order(-results$Test_Accuracy), ]
  
  # Display results
  print(results, row.names = FALSE)
  
  # Save results table
  results_filename <- paste0(analysis_name, "_model_comparison_results.csv")
  write.csv(results, file.path(results_dir, results_filename), row.names = FALSE)
  cat("\n✓ Results table saved:", results_filename, "\n")
  
  # Identify best model
  best_model <- results[1, ]
  cat("\n", strrep("-", 80), "\n")
  cat("BEST MODEL (by Test Accuracy):\n")
  cat("  Dataset:", best_model$Dataset, "\n")
  cat("  Model:  ", best_model$Model, "\n")
  cat("  Test Accuracy:", sprintf("%.3f", best_model$Test_Accuracy), "\n")
  cat("  CV Accuracy:  ", sprintf("%.3f ± %.3f", best_model$CV_Accuracy, best_model$CV_Accuracy_CI), "\n")
  cat(strrep("-", 80), "\n")
  
  return(results)
}

################################################################################
#### RUN BOTH ANALYSES
################################################################################

cat("\n")
cat(strrep("#", 80), "\n")
cat("# CROSS-VALIDATION MODEL COMPARISON - ACCURACY ONLY\n")
cat(strrep("#", 80), "\n")

# Run Total analysis
results_total <- run_cv_analysis(
  train_test_dir = train_test_dir_total,
  analysis_name = "TOTAL",
  results_dir = results_dir_total
)

# Run Overlapping/Filtered analysis
results_overlap <- run_cv_analysis(
  train_test_dir = train_test_dir_overlap,
  analysis_name = "OVERLAP",
  results_dir = results_dir_overlap
)

################################################################################
#### CREATE COMPARISON VISUALIZATIONS
################################################################################

library(ggplot2)
library(tidyr)

figures_dir <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/Figures/ModelPerformance_CV"
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)

# Combine results from both analyses
combined_results <- bind_rows(
  results_total %>% mutate(Analysis = "Total"),
  results_overlap %>% mutate(Analysis = "Overlap")
) %>%
  filter(Dataset %in% c("RAW", "GAM", "MA"))  # Focus on Sr87/86 data

# ============================================================================
# PLOT 1: CV vs Test Accuracy Comparison
# ============================================================================

cv_vs_test_data <- combined_results %>%
  select(Dataset, Model, Analysis, CV_Accuracy, Test_Accuracy) %>%
  pivot_longer(cols = c(CV_Accuracy, Test_Accuracy), 
               names_to = "Metric_Type", 
               values_to = "Accuracy") %>%
  mutate(
    Metric_Type = factor(Metric_Type, 
                         levels = c("CV_Accuracy", "Test_Accuracy"),
                         labels = c("Cross-Validation", "Holdout Test")),
    Model = factor(Model, levels = c("RF", "SVM", "KNN"),
                   labels = c("Random Forest", "SVM", "KNN"))
  )

cv_vs_test_plot <- ggplot(cv_vs_test_data, 
                          aes(x = Model, y = Accuracy, fill = Metric_Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = sprintf("%.3f", Accuracy)), 
            position = position_dodge(width = 0.8),
            vjust = -0.5, size = 3) +
  facet_grid(Analysis ~ Dataset) +
  scale_fill_manual(values = c("Cross-Validation" = "#3498db", "Holdout Test" = "#e74c3c")) +
  labs(
    title = "Cross-Validation vs. Holdout Test Performance",
    subtitle = "Comparison across datasets, models, and analyses",
    x = "Model Type",
    y = "Accuracy",
    fill = "Evaluation Method"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, margin = margin(b = 15)),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave(file.path(figures_dir, "CV_vs_Test_Accuracy_Comparison.pdf"),
       cv_vs_test_plot, width = 12, height = 8, dpi = 300)

# ============================================================================
# PLOT 2: Error Bars showing CV Uncertainty
# ============================================================================

cv_uncertainty_plot <- ggplot(combined_results, 
                              aes(x = Model, y = CV_Accuracy, color = Dataset)) +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = CV_Accuracy - CV_Accuracy_CI, 
                    ymax = CV_Accuracy + CV_Accuracy_CI),
                width = 0.2, position = position_dodge(width = 0.5)) +
  geom_point(aes(y = Test_Accuracy), shape = 4, size = 4, stroke = 2,
             position = position_dodge(width = 0.5)) +
  facet_wrap(~Analysis) +
  scale_color_manual(values = c("RAW" = "#e74c3c", "GAM" = "#3498db", "MA" = "#2ecc71")) +
  labs(
    title = "Cross-Validation Performance with Uncertainty",
    subtitle = "Points = CV mean, Error bars = 95% CI, X = Test accuracy",
    x = "Model Type",
    y = "Accuracy",
    color = "Dataset"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5, margin = margin(b = 15)),
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 12),
    panel.grid.minor = element_blank()
  )

ggsave(file.path(figures_dir, "CV_Uncertainty_with_Test.pdf"),
       cv_uncertainty_plot, width = 10, height = 6, dpi = 300)

cat("\n✓ Visualization plots saved to:", figures_dir, "\n")

################################################################################
#### FINAL SUMMARY
################################################################################

cat("\n", strrep("=", 80), "\n")
cat("ANALYSIS COMPLETE\n")
cat(strrep("=", 80), "\n\n")

cat("Results saved:\n")
cat("  - Total:", results_dir_total, "\n")
cat("  - Overlap:", results_dir_overlap, "\n\n")

cat("Figures saved:\n")
cat("  -", figures_dir, "\n\n")

cat(strrep("=", 80), "\n\n")