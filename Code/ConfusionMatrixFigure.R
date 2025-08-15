# Confusion Matrix Figure Generator
# This script creates updated confusion matrices with diagonal coloring

library(dplyr)
library(ggplot2)
library(scales)
library(forcats)

################################################################################
#### CONFIGURATION
################################################################################

# Define paths
results_dir_total <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/Output/ModelResultsPreCal/Total"
results_dir_overlap <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/Output/ModelResultsPreCal/Filtered"
figures_dir <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/Figures/ModelPerformance"

# Create figures directory if it doesn't exist
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)

################################################################################
#### LOAD GAM RF PREDICTIONS
################################################################################

# Load GAM RF predictions for both analyses
gam_rf_total_file <- file.path(results_dir_total, "GAM_RF_TOTAL_predictions.csv")
gam_rf_overlap_file <- file.path(results_dir_overlap, "GAM_RF_OVERLAP_predictions.csv")

if (!file.exists(gam_rf_total_file) || !file.exists(gam_rf_overlap_file)) {
  stop("GAM RF prediction files not found! Please run the main modeling script first.")
}

# Load predictions
gam_rf_total <- read.csv(gam_rf_total_file)
gam_rf_overlap <- read.csv(gam_rf_overlap_file)

# Add analysis labels
gam_rf_total$Analysis <- "Total"
gam_rf_overlap$Analysis <- "Overlapping"

# Combine predictions
combined_gam_rf <- bind_rows(gam_rf_total, gam_rf_overlap)

cat("Loaded GAM RF predictions:\n")
cat("Total analysis:", nrow(gam_rf_total), "predictions\n")
cat("Overlapping analysis:", nrow(gam_rf_overlap), "predictions\n")

################################################################################
#### FUNCTION TO CREATE CONFUSION MATRIX DATA
################################################################################

create_confusion_data <- function(predictions, analysis_name) {
  
  # Ensure consistent factor levels
  watershed_levels <- c("Kusko", "Nush", "Yukon")
  
  conf_data <- predictions %>%
    mutate(
      Watershed = factor(Watershed, levels = watershed_levels),
      .pred_class = factor(.pred_class, levels = watershed_levels)
    ) %>%
    count(Watershed, .pred_class, .drop = FALSE) %>%
    group_by(Watershed) %>%
    mutate(
      Total = sum(n),
      Percentage = n / Total,
      Analysis = analysis_name
    ) %>%
    ungroup() %>%
    mutate(
      Analysis = analysis_name,
      Label = as.character(n),  # Always show count, including 0
      Percentage_Label = sprintf("%.1f%%", Percentage * 100),  # Always show percentage, including 0.0%
      Combined_Label = paste0(n, "\n(", sprintf("%.1f%%", Percentage * 100), ")"),  # Both count and percentage
      is_diagonal = Watershed == .pred_class,
      # Create fill variable: red for diagonal, dark blue for off-diagonal
      Fill_Color = ifelse(is_diagonal, "Correct", "Incorrect")
    )
  return(conf_data)
}

################################################################################
#### CREATE CONFUSION MATRIX DATA
################################################################################

# Create confusion matrix data for both analyses
conf_total <- create_confusion_data(gam_rf_total, "Total")
conf_overlap <- create_confusion_data(gam_rf_overlap, "Overlapping")
combined_conf <- bind_rows(conf_total, conf_overlap)

# Print summary of confusion matrix data
cat("\nConfusion matrix summary:\n")
print(combined_conf %>% 
        group_by(Analysis, Fill_Color) %>% 
        summarise(total_cases = sum(n), .groups = "drop"))

################################################################################
#### VERSION 1: CONFUSION MATRIX WITH COUNTS
################################################################################

confusion_plot_counts <- ggplot(combined_conf, aes(x = .pred_class, y = fct_rev(Watershed))) +
  geom_tile(aes(fill = Fill_Color), color = "white", size = 1.5) +
  geom_text(aes(label = Label), 
            color = "white", size = 5, fontface = "bold") +
  scale_fill_manual(
    values = c("Correct" = "#e74c3c", "Incorrect" = "#34495e"),
    name = "Classification"
  ) +
  facet_wrap(~Analysis, 
             labeller = labeller(Analysis = c("Total" = "Total Analysis", "Overlapping" = "Overlapping Analysis"))) +
  labs(
    title = "Classification Confusion Matrices (Counts)",
    subtitle = "GAM Random Forest Model - Predicted vs. Actual",
    x = "Predicted Watershed",
    y = "Actual Watershed"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18, margin = margin(b = 5)),
    plot.subtitle = element_text(hjust = 0.5, size = 14, color = "gray50", margin = margin(b = 25)),
    axis.title = element_text(face = "bold", size = 14, margin = margin(10, 10, 10, 10)),
    axis.text = element_text(size = 12, face = "bold"),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid = element_blank(),
    strip.text = element_text(size = 14, face = "bold", margin = margin(10, 10, 10, 10)),
    strip.background = element_rect(fill = "#ecf0f1", color = NA),
    legend.position = "bottom",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    legend.key.width = unit(2, "cm"),
    legend.key.height = unit(0.5, "cm"),
    plot.margin = margin(30, 30, 30, 30)
  ) +
  coord_equal()

################################################################################
#### VERSION 2: CONFUSION MATRIX WITH PERCENTAGES
################################################################################

confusion_plot_percentages <- ggplot(combined_conf, aes(x = .pred_class, y = fct_rev(Watershed))) +
  geom_tile(aes(fill = Fill_Color), color = "white", size = 1.5) +
  geom_text(aes(label = Percentage_Label), 
            color = "white", size = 5, fontface = "bold") +
  scale_fill_manual(
    values = c("Correct" = "#e74c3c", "Incorrect" = "#34495e"),
    name = "Classification"
  ) +
  facet_wrap(~Analysis, 
             labeller = labeller(Analysis = c("Total" = "Total Analysis", "Overlapping" = "Overlapping Analysis"))) +
  labs(
    title = "Classification Confusion Matrices (Percentages)",
    subtitle = "GAM Random Forest Model - Predicted vs. Actual",
    x = "Predicted Watershed",
    y = "Actual Watershed"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18, margin = margin(b = 5)),
    plot.subtitle = element_text(hjust = 0.5, size = 14, color = "gray50", margin = margin(b = 25)),
    axis.title = element_text(face = "bold", size = 14, margin = margin(10, 10, 10, 10)),
    axis.text = element_text(size = 12, face = "bold"),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid = element_blank(),
    strip.text = element_text(size = 14, face = "bold", margin = margin(10, 10, 10, 10)),
    strip.background = element_rect(fill = "#ecf0f1", color = NA),
    legend.position = "bottom",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    legend.key.width = unit(2, "cm"),
    legend.key.height = unit(0.5, "cm"),
    plot.margin = margin(30, 30, 30, 30)
  ) +
  coord_equal()

################################################################################
#### VERSION 3: CONFUSION MATRIX WITH BOTH COUNTS AND PERCENTAGES
################################################################################

confusion_plot_combined <- ggplot(combined_conf, aes(x = .pred_class, y = fct_rev(Watershed))) +
  geom_tile(aes(fill = Fill_Color), color = "white", size = 1.5) +
  geom_text(aes(label = Combined_Label), 
            color = "white", size = 4, fontface = "bold", lineheight = 0.9) +
  scale_fill_manual(
    values = c("Correct" = "#e74c3c", "Incorrect" = "#34495e"),
    name = "Classification"
  ) +
  facet_wrap(~Analysis, 
             labeller = labeller(Analysis = c("Total" = "Total Analysis", "Overlapping" = "Overlapping Analysis"))) +
  labs(
    title = "Classification Confusion Matrices",
    subtitle = "GAM Random Forest Model - Predicted vs. Actual",
    x = "Predicted Watershed",
    y = "Actual Watershed"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18, margin = margin(b = 5)),
    plot.subtitle = element_text(hjust = 0.5, size = 14, color = "gray50", margin = margin(b = 25)),
    axis.title = element_text(face = "bold", size = 14, margin = margin(10, 10, 10, 10)),
    axis.text = element_text(size = 12, face = "bold"),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid = element_blank(),
    strip.text = element_text(size = 14, face = "bold", margin = margin(10, 10, 10, 10)),
    strip.background = element_rect(fill = "#ecf0f1", color = NA),
    legend.position = "bottom",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    legend.key.width = unit(2, "cm"),
    legend.key.height = unit(0.5, "cm"),
    plot.margin = margin(30, 30, 30, 30)
  ) +
  coord_equal()

################################################################################
#### SAVE FIGURES
################################################################################

# Save all three confusion matrix versions as PDFs
ggsave(file.path(figures_dir, "GAM_RF_Confusion_Matrix_Counts.pdf"), 
       confusion_plot_counts, width = 12, height = 6, dpi = 300, bg = "white")

ggsave(file.path(figures_dir, "GAM_RF_Confusion_Matrix_Percentages.pdf"), 
       confusion_plot_percentages, width = 12, height = 6, dpi = 300, bg = "white")

ggsave(file.path(figures_dir, "GAM_RF_Confusion_Matrix_Combined.pdf"), 
       confusion_plot_combined, width = 12, height = 6, dpi = 300, bg = "white")

# Also save as high-res PNGs for easy viewing
ggsave(file.path(figures_dir, "GAM_RF_Confusion_Matrix_Counts.png"), 
       confusion_plot_counts, width = 12, height = 6, dpi = 300, bg = "white")

ggsave(file.path(figures_dir, "GAM_RF_Confusion_Matrix_Percentages.png"), 
       confusion_plot_percentages, width = 12, height = 6, dpi = 300, bg = "white")

ggsave(file.path(figures_dir, "GAM_RF_Confusion_Matrix_Combined.png"), 
       confusion_plot_combined, width = 12, height = 6, dpi = 300, bg = "white")

################################################################################
#### DISPLAY RESULTS
################################################################################

cat("\n✓ Updated confusion matrix figures saved:\n")
cat("  - GAM_RF_Confusion_Matrix_Counts.pdf/.png\n")
cat("  - GAM_RF_Confusion_Matrix_Percentages.pdf/.png\n")
cat("  - GAM_RF_Confusion_Matrix_Combined.pdf/.png (NEW: counts & percentages)\n")
cat("\nFigures saved to:", figures_dir, "\n")

# Print accuracy by class for verification
cat("\nClass-specific accuracy verification:\n")
accuracy_by_class <- combined_gam_rf %>%
  group_by(Analysis, Watershed) %>%
  summarise(
    Total = n(),
    Correct = sum(Watershed == .pred_class),
    Accuracy = mean(Watershed == .pred_class),
    .groups = "drop"
  )
print(accuracy_by_class)

cat("\nScript completed successfully!\n")