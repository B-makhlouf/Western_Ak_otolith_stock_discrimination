# =============================================================================
# RF MODEL CALIBRATION SCRIPT
# =============================================================================
# Calibrates RF models and saves calibration mappings for future use
# =============================================================================

library(tidymodels)
library(probably)
library(tidyverse)
library(yardstick)
library(cowplot)

# =============================================================================
# CONFIGURATION
# =============================================================================

# Models to calibrate
data_types <- c("RAW", "GAM", "MA")
analyses <- c("TOTAL", "OVERLAP")

# Paths
results_dir_total <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/Output/ModelResultsPreCal/Total"
results_dir_overlap <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/Output/ModelResultsPreCal/Filtered"
calibrated_models_dir <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/data/CalibratedModels"
figures_dir <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/Figures/Calibration"

# Create directories
dir.create(calibrated_models_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# CALIBRATION FUNCTION
# =============================================================================

calibrate_rf_model <- function(data_type, analysis) {
  
  cat("Processing", data_type, "-", analysis, "...\n")
  
  # Load predictions
  results_dir <- if (analysis == "TOTAL") results_dir_total else results_dir_overlap
  pred_file <- file.path(results_dir, paste0(data_type, "_RF_", analysis, "_predictions.csv"))
  
  if (!file.exists(pred_file)) {
    cat("  ❌ File not found:", basename(pred_file), "\n")
    return(NULL)
  }
  
  predictions <- read.csv(pred_file) %>%
    mutate(Watershed = as.factor(Watershed), .pred_class = as.factor(.pred_class))
  
  # Calculate before metrics
  prob_cols <- predictions %>% select(starts_with(".pred_")) %>% select(-any_of(".pred_class"))
  before_log_loss <- mn_log_loss_vec(predictions$Watershed, as.matrix(prob_cols))
  before_brier <- brier_class_vec(predictions$Watershed, as.matrix(prob_cols))
  
  # Create before plot
  beforecal <- cal_plot_windowed(predictions, truth = Watershed, window_size = 0.3, step_size = 0.02) +
    labs(title = "Before Calibration") + theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"), legend.position = "bottom")
  
  # Perform calibration
  calibration <- cal_estimate_multinomial(predictions, truth = Watershed, 
                                          estimate = starts_with(".pred_"), method = "isotonic")
  calibrated_predictions <- cal_apply(predictions, calibration)
  
  # Calculate after metrics
  prob_cols_after <- calibrated_predictions %>% select(starts_with(".pred_")) %>% select(-any_of(".pred_class"))
  after_log_loss <- mn_log_loss_vec(calibrated_predictions$Watershed, as.matrix(prob_cols_after))
  after_brier <- brier_class_vec(calibrated_predictions$Watershed, as.matrix(prob_cols_after))
  
  # Create after plot
  aftercal <- cal_plot_windowed(calibrated_predictions, truth = Watershed, window_size = 0.3, step_size = 0.02) +
    labs(title = "After Calibration") + theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"), legend.position = "bottom")
  
  # Create combined figure
  combined_plot <- plot_grid(beforecal, aftercal, ncol = 2, labels = c("A", "B"))
  title <- ggdraw() + draw_label(paste("RF Calibration:", data_type, "-", analysis), fontface = "bold", size = 16)
  final_plot <- plot_grid(title, combined_plot, ncol = 1, rel_heights = c(0.1, 0.9))
  
  # Save figure
  ggsave(file.path(figures_dir, paste0("RF_Calibration_", data_type, "_", analysis, ".png")), 
         final_plot, width = 12, height = 6, dpi = 300, bg = "white")
  
  # Save calibration mapping
  saveRDS(calibration, file.path(calibrated_models_dir, paste0(data_type, "_RF_", analysis, "_calibration.rds")))
  
  # Calculate improvements
  log_loss_improvement <- before_log_loss - after_log_loss
  brier_improvement <- before_brier - after_brier
  
  cat("  ✅ Log Loss:", round(before_log_loss, 4), "→", round(after_log_loss, 4), 
      "(", ifelse(log_loss_improvement > 0, "+", ""), round(log_loss_improvement, 4), ")\n")
  cat("  ✅ Brier:", round(before_brier, 4), "→", round(after_brier, 4), 
      "(", ifelse(brier_improvement > 0, "+", ""), round(brier_improvement, 4), ")\n")
  
  # Return results
  return(data.frame(
    Data_Type = data_type, Analysis = analysis,
    Before_Log_Loss = before_log_loss, After_Log_Loss = after_log_loss, Log_Loss_Improvement = log_loss_improvement,
    Before_Brier = before_brier, After_Brier = after_brier, Brier_Improvement = brier_improvement,
    stringsAsFactors = FALSE
  ))
}

# =============================================================================
# PROCESS ALL MODELS
# =============================================================================

cat("🚀 Calibrating", length(data_types) * length(analyses), "RF models...\n\n")

# Process all combinations
results <- map_dfr(analyses, function(analysis) {
  map_dfr(data_types, function(data_type) {
    calibrate_rf_model(data_type, analysis)
  })
})

# =============================================================================
# SUMMARY
# =============================================================================

cat("\n📊 CALIBRATION SUMMARY:\n")
print(results)

# Save summary
write.csv(results, file.path(figures_dir, "RF_Calibration_Summary.csv"), row.names = FALSE)

# Final stats
improved_models <- sum(results$Log_Loss_Improvement > 0 & results$Brier_Improvement > 0, na.rm = TRUE)
cat("\n🎉 COMPLETE! Calibration mappings saved to:", calibrated_models_dir, "\n")
cat("📈", improved_models, "/", nrow(results), "models improved on both metrics\n")
cat("📊 Average improvements - Log Loss:", round(mean(results$Log_Loss_Improvement, na.rm = TRUE), 4), 
    "| Brier:", round(mean(results$Brier_Improvement, na.rm = TRUE), 4), "\n")

