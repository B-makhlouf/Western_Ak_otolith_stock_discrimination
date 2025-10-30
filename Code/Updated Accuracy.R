################################################################################
# REMAKE HEATMAPS WITH "RESTRICTED" INSTEAD OF "OVERLAPPING"
# WITH COMBINED TWO-PANEL FIGURE AND A/B LABELS
################################################################################

library(ggplot2)
library(dplyr)
library(scales)
library(patchwork)  # For combining plots

# Sample data structure - replace with your actual results
# This assumes you have results_total and results_restricted data frames

# Example data structure (replace with your actual data):
# results_total should have columns: Dataset, Model, Accuracy, F1_Score
# results_restricted should have columns: Dataset, Model, Accuracy, F1_Score

# For demonstration, creating sample data:
# REPLACE THIS SECTION WITH YOUR ACTUAL DATA LOADING
results_total <- data.frame(
  Dataset = rep(c("RAW", "GAM", "MA"), each = 3),
  Model = rep(c("RF", "SVM", "KNN"), 3),
  Accuracy = c(0.880, 0.898, 0.781, 0.925, 0.907, 0.878, 0.917, 0.902, 0.896),
  F1_Score = c(0.879, 0.896, 0.770, 0.922, 0.905, 0.875, 0.915, 0.899, 0.892)
)

results_restricted <- data.frame(
  Dataset = rep(c("RAW", "GAM", "MA"), each = 3),
  Model = rep(c("RF", "SVM", "KNN"), 3),
  Accuracy = c(0.847, 0.870, 0.719, 0.908, 0.882, 0.847, 0.898, 0.875, 0.867),
  F1_Score = c(0.840, 0.865, 0.726, 0.909, 0.882, 0.845, 0.897, 0.872, 0.866)
)

################################################################################
# FUNCTION TO CREATE COMBINED HEATMAPS WITH A/B LABELS
################################################################################

create_combined_heatmaps_restricted <- function(results_total, results_restricted, 
                                                output_dir = ".") {
  
  # Add analysis identifier to each dataset
  results_total_labeled <- results_total %>%
    mutate(Analysis = "Total")
  
  results_restricted_labeled <- results_restricted %>%
    mutate(Analysis = "Restricted")
  
  # Combine the results
  combined_results <- bind_rows(results_total_labeled, results_restricted_labeled)
  
  # Create combined dataset names
  combined_results <- combined_results %>%
    mutate(
      Dataset_Combined = paste(Dataset, Analysis, sep = " - "),
      Dataset = factor(Dataset, levels = c("RAW", "GAM", "MA")),
      Model = factor(Model, levels = c("RF", "SVM", "KNN"),
                     labels = c("Random Forest", "SVM", "KNN")),
      Dataset_Combined = factor(Dataset_Combined, 
                                levels = c(paste(c("RAW", "GAM", "MA"), "- Total"),
                                           paste(c("RAW", "GAM", "MA"), "- Restricted")))
    )
  
  # ============================================================================
  # ACCURACY HEATMAP
  # ============================================================================
  
  accuracy_plot <- ggplot(combined_results, aes(x = Model, y = Dataset_Combined, fill = Accuracy)) +
    geom_tile(color = NA, width = 1, height = 1) +
    geom_text(aes(label = sprintf("%.3f", Accuracy)), 
              color = "black", size = 6, fontface = "bold") +
    scale_fill_gradient(
      low = "white", 
      high = "#9AB87A",
      limits = c(min(combined_results$Accuracy) * 0.99, max(combined_results$Accuracy) * 1.01),
      labels = scales::percent_format(accuracy = 0.1)
    ) +
    labs(
      title = "Accuracy",
      x = "Model Type",
      y = "Data Source",
      fill = "Accuracy",
      tag = "A"  # Add A label
    ) +
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 20, margin = margin(b = 10)),
      plot.subtitle = element_text(hjust = 0.5, size = 16, color = "gray40", margin = margin(b = 20)),
      plot.tag = element_text(size = 20, face = "bold", hjust = 0, vjust = 0),  # Style A label
      plot.tag.position = c(0.02, 0.02),  # Position at bottom-left
      axis.title.x = element_text(face = "bold", size = 16, margin = margin(t = 15)),
      axis.title.y = element_text(face = "bold", size = 16, margin = margin(r = 15), angle = 90),
      axis.text.x = element_text(size = 14, margin = margin(t = 8)),
      axis.text.y = element_text(size = 14, margin = margin(r = 8)),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      legend.position = "right",
      legend.key.height = unit(1.5, "cm"),
      legend.key.width = unit(0.8, "cm"),
      legend.title = element_text(size = 16, face = "bold"),
      legend.text = element_text(size = 14),
      legend.margin = margin(l = 20),
      plot.margin = margin(20, 25, 20, 25)
    ) +
    # Add horizontal line to separate Total and Restricted
    annotate("segment", x = 0.5, xend = 3.5, y = 3.5, yend = 3.5, 
             color = "white", size = 2)
  
  # ============================================================================
  # F1-SCORE HEATMAP
  # ============================================================================
  
  f1_plot <- ggplot(combined_results, aes(x = Model, y = Dataset_Combined, fill = F1_Score)) +
    geom_tile(color = NA, width = 1, height = 1) +
    geom_text(aes(label = sprintf("%.3f", F1_Score)), 
              color = "black", size = 6, fontface = "bold") +
    scale_fill_gradient(
      low = "white", 
      high = "#9AB87A",
      limits = c(min(combined_results$F1_Score) * 0.99, max(combined_results$F1_Score) * 1.01),
      labels = scales::percent_format(accuracy = 0.1)
    ) +
    labs(
      title = "F1-Score",
      x = "Model Type",
      y = "Data Source",
      fill = "F1-Score",
      tag = "B"  # Add B label
    ) +
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 20, margin = margin(b = 10)),
      plot.subtitle = element_text(hjust = 0.5, size = 16, color = "gray40", margin = margin(b = 20)),
      plot.tag = element_text(size = 20, face = "bold", hjust = 0, vjust = 0),  # Style B label
      plot.tag.position = c(0.02, 0.02),  # Position at bottom-left
      axis.title.x = element_text(face = "bold", size = 16, margin = margin(t = 15)),
      axis.title.y = element_text(face = "bold", size = 16, margin = margin(r = 15), angle = 90),
      axis.text.x = element_text(size = 14, margin = margin(t = 8)),
      axis.text.y = element_text(size = 14, margin = margin(r = 8)),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      legend.position = "right",
      legend.key.height = unit(1.5, "cm"),
      legend.key.width = unit(0.8, "cm"),
      legend.title = element_text(size = 16, face = "bold"),
      legend.text = element_text(size = 14),
      legend.margin = margin(l = 20),
      plot.margin = margin(20, 25, 20, 25)
    ) +
    # Add horizontal line to separate Total and Restricted
    annotate("segment", x = 0.5, xend = 3.5, y = 3.5, yend = 3.5, 
             color = "white", size = 2)
  
  # ============================================================================
  # SAVE INDIVIDUAL HEATMAPS
  # ============================================================================
  
  ggsave(file.path(output_dir, "Combined_Accuracy_Heatmap_Restricted.pdf"), 
         accuracy_plot, width = 10, height = 6, dpi = 300, bg = "white")
  
  ggsave(file.path(output_dir, "Combined_F1Score_Heatmap_Restricted.pdf"), 
         f1_plot, width = 10, height = 6, dpi = 300, bg = "white")
  
  cat("✓ Individual heatmaps saved as PDFs:\n")
  cat("  -", file.path(output_dir, "Combined_Accuracy_Heatmap_Restricted.pdf"), "\n")
  cat("  -", file.path(output_dir, "Combined_F1Score_Heatmap_Restricted.pdf"), "\n")
  
  # ============================================================================
  # CREATE COMBINED TWO-PANEL FIGURE WITH A/B LABELS
  # ============================================================================
  
  # Combine plots side by side with patchwork
  combined_plot <- accuracy_plot + f1_plot + 
    plot_annotation(
      title = "Sr87/86 isotope data performance across analysis approaches",
      theme = theme(
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold", 
                                  margin = margin(b = 15))
      )
    ) +
    plot_layout(ncol = 2)
  
  # Save combined figure
  ggsave(file.path(output_dir, "Combined_Heatmaps_TwoPanel_Restricted.pdf"), 
         combined_plot, width = 20, height = 6, dpi = 300, bg = "white")
  
  cat("\n✓ Combined two-panel figure saved:\n")
  cat("  -", file.path(output_dir, "Combined_Heatmaps_TwoPanel_Restricted.pdf"), "\n")
  
  return(list(
    accuracy_plot = accuracy_plot, 
    f1_plot = f1_plot,
    combined_plot = combined_plot
  ))
}

################################################################################
# EXECUTE: Create the plots
################################################################################

# Create output directory
output_dir <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/Figures"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Generate the plots
plots <- create_combined_heatmaps_restricted(results_total, results_restricted, output_dir)

# Display the plots
print(plots$accuracy_plot)
print(plots$f1_plot)
print(plots$combined_plot)

cat("\n✓ All heatmaps successfully created with 'Restricted' instead of 'Overlapping'\n")
cat("✓ A/B panel labels added to bottom-left of combined figure\n")