# =============================================================================
# WATERSHED PERFORMANCE LINE PLOT - PROBABILITY THRESHOLDS
# =============================================================================
# Creates line plot showing performance (accuracy) changes from 60-90% thresholds
# Separate lines for each watershed plus average line
# Linear script - no functions for easy line-by-line execution
# With direct line labeling like The Economist style
# FIXED: Label overlap issue in TOTAL dataset plot
# UPDATED: Increased label sizes for publication quality
# UPDATED: Changed "OVERLAP" to "Restricted" throughout
# =============================================================================

library(tidyverse)
library(probably)
library(ggplot2)
library(scales)
library(patchwork)

# =============================================================================
# SETUP PATHS AND DIRECTORIES
# =============================================================================

# Paths
calibrated_models_dir <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/data/CalibratedModels"
results_dir_total <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/Output/ModelResultsPreCal/Total"
results_dir_overlap <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/Output/ModelResultsPreCal/Filtered"
output_dir <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/Figures/ProbabilityDistributions"

# Create output directory
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# PROCESS TOTAL DATASET
# =============================================================================

cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("PROCESSING TOTAL DATASET\n")
cat(paste(rep("=", 60), collapse = ""), "\n")

# Load TOTAL dataset predictions and calibration
predictions_total <- read.csv(file.path(results_dir_total, "GAM_RF_TOTAL_predictions.csv")) %>%
  mutate(Watershed = as.factor(Watershed), .pred_class = as.factor(.pred_class))

calibration_total <- readRDS(file.path(calibrated_models_dir, "GAM_RF_TOTAL_calibration.rds"))

# Apply calibration to TOTAL dataset
calibrated_predictions_total <- cal_apply(predictions_total, calibration_total)

cat("Total samples:", nrow(calibrated_predictions_total), "\n")
cat("Watersheds:", table(calibrated_predictions_total$Watershed), "\n")

# Get prediction probability columns for TOTAL
pred_cols_total <- grep("^\\.pred_", colnames(calibrated_predictions_total), value = TRUE)
pred_cols_total <- pred_cols_total[pred_cols_total != ".pred_class"]  # Remove .pred_class if it exists
cat("Prediction probability columns:", pred_cols_total, "\n")

# Extract maximum probabilities and predictions for TOTAL
sample_results_total <- calibrated_predictions_total %>%
  select(Watershed, all_of(pred_cols_total)) %>%
  mutate(
    # Extract probabilities for each watershed
    prob1 = .[[pred_cols_total[1]]],
    prob2 = .[[pred_cols_total[2]]],
    prob3 = .[[pred_cols_total[3]]],
    # Find max probability
    Max_Probability = pmax(prob1, prob2, prob3, na.rm = TRUE),
    # Find predicted watershed
    Predicted_Watershed = case_when(
      prob1 == Max_Probability ~ gsub("\\.pred_", "", pred_cols_total[1]),
      prob2 == Max_Probability ~ gsub("\\.pred_", "", pred_cols_total[2]),
      prob3 == Max_Probability ~ gsub("\\.pred_", "", pred_cols_total[3]),
      TRUE ~ "Unknown"
    ),
    # Check correctness
    Correct = (as.character(Watershed) == Predicted_Watershed)
  ) %>%
  select(Watershed, Predicted_Watershed, Max_Probability, Correct)

cat("Overall accuracy for TOTAL:", round(mean(sample_results_total$Correct), 3), "\n")

# Define thresholds from 60% to 90% by 5% increments
thresholds <- c(0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9)

# Calculate threshold performance for TOTAL dataset
threshold_results_total <- map_dfr(thresholds, function(thresh) {
  
  # For each watershed, see how many samples are above threshold AND correct
  watershed_results <- map_dfr(c("Kusko", "Nush", "Yukon"), function(ws) {
    
    # Get samples for this watershed
    watershed_samples <- sample_results_total %>% filter(Watershed == ws)
    
    # Find samples above threshold AND correct
    above_threshold_correct <- watershed_samples %>% 
      filter(Max_Probability >= thresh, Correct == TRUE)
    
    data.frame(
      Threshold = thresh,
      Watershed = ws,
      Total_Samples = nrow(watershed_samples),
      Above_Threshold_Correct = nrow(above_threshold_correct),
      Percent_Correct_Above_Threshold = round(nrow(above_threshold_correct) / nrow(watershed_samples) * 100, 1)
    )
  })
  
  return(watershed_results)
})

# Prepare line plot data for TOTAL
line_plot_data_total <- threshold_results_total %>%
  select(Threshold, Watershed, Percent_Correct_Above_Threshold) %>%
  mutate(Threshold_Percent = Threshold * 100)

# Calculate average performance across all watersheds for TOTAL
average_performance_total <- line_plot_data_total %>%
  group_by(Threshold, Threshold_Percent) %>%
  summarise(
    Average_Performance = mean(Percent_Correct_Above_Threshold, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Watershed = "Average")

# Combine watershed data with average for TOTAL
combined_line_data_total <- line_plot_data_total %>%
  select(Threshold, Threshold_Percent, Watershed, Performance = Percent_Correct_Above_Threshold) %>%
  bind_rows(
    average_performance_total %>%
      select(Threshold, Threshold_Percent, Watershed, Performance = Average_Performance)
  )

# =============================================================================
# PROCESS RESTRICTED DATASET
# =============================================================================

cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("PROCESSING RESTRICTED DATASET\n")
cat(paste(rep("=", 60), collapse = ""), "\n")

# Load RESTRICTED dataset predictions and calibration
predictions_restricted <- read.csv(file.path(results_dir_overlap, "GAM_RF_OVERLAP_predictions.csv")) %>%
  mutate(Watershed = as.factor(Watershed), .pred_class = as.factor(.pred_class))

calibration_restricted <- readRDS(file.path(calibrated_models_dir, "GAM_RF_OVERLAP_calibration.rds"))

# Apply calibration to RESTRICTED dataset
calibrated_predictions_restricted <- cal_apply(predictions_restricted, calibration_restricted)

cat("Total samples:", nrow(calibrated_predictions_restricted), "\n")
cat("Watersheds:", table(calibrated_predictions_restricted$Watershed), "\n")

# Get prediction probability columns for RESTRICTED
pred_cols_restricted <- grep("^\\.pred_", colnames(calibrated_predictions_restricted), value = TRUE)
pred_cols_restricted <- pred_cols_restricted[pred_cols_restricted != ".pred_class"]  # Remove .pred_class if it exists
cat("Prediction probability columns:", pred_cols_restricted, "\n")

# Extract maximum probabilities and predictions for RESTRICTED
sample_results_restricted <- calibrated_predictions_restricted %>%
  select(Watershed, all_of(pred_cols_restricted)) %>%
  mutate(
    # Extract probabilities for each watershed
    prob1 = .[[pred_cols_restricted[1]]],
    prob2 = .[[pred_cols_restricted[2]]],
    prob3 = .[[pred_cols_restricted[3]]],
    # Find max probability
    Max_Probability = pmax(prob1, prob2, prob3, na.rm = TRUE),
    # Find predicted watershed
    Predicted_Watershed = case_when(
      prob1 == Max_Probability ~ gsub("\\.pred_", "", pred_cols_restricted[1]),
      prob2 == Max_Probability ~ gsub("\\.pred_", "", pred_cols_restricted[2]),
      prob3 == Max_Probability ~ gsub("\\.pred_", "", pred_cols_restricted[3]),
      TRUE ~ "Unknown"
    ),
    # Check correctness
    Correct = (as.character(Watershed) == Predicted_Watershed)
  ) %>%
  select(Watershed, Predicted_Watershed, Max_Probability, Correct)

cat("Overall accuracy for RESTRICTED:", round(mean(sample_results_restricted$Correct), 3), "\n")

# Calculate threshold performance for RESTRICTED dataset
threshold_results_restricted <- map_dfr(thresholds, function(thresh) {
  
  # For each watershed, see how many samples are above threshold AND correct
  watershed_results <- map_dfr(c("Kusko", "Nush", "Yukon"), function(ws) {
    
    # Get samples for this watershed
    watershed_samples <- sample_results_restricted %>% filter(Watershed == ws)
    
    # Find samples above threshold AND correct
    above_threshold_correct <- watershed_samples %>% 
      filter(Max_Probability >= thresh, Correct == TRUE)
    
    data.frame(
      Threshold = thresh,
      Watershed = ws,
      Total_Samples = nrow(watershed_samples),
      Above_Threshold_Correct = nrow(above_threshold_correct),
      Percent_Correct_Above_Threshold = round(nrow(above_threshold_correct) / nrow(watershed_samples) * 100, 1)
    )
  })
  
  return(watershed_results)
})

# Prepare line plot data for RESTRICTED
line_plot_data_restricted <- threshold_results_restricted %>%
  select(Threshold, Watershed, Percent_Correct_Above_Threshold) %>%
  mutate(Threshold_Percent = Threshold * 100)

# Calculate average performance across all watersheds for RESTRICTED
average_performance_restricted <- line_plot_data_restricted %>%
  group_by(Threshold, Threshold_Percent) %>%
  summarise(
    Average_Performance = mean(Percent_Correct_Above_Threshold, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Watershed = "Average")

# Combine watershed data with average for RESTRICTED
combined_line_data_restricted <- line_plot_data_restricted %>%
  select(Threshold, Threshold_Percent, Watershed, Performance = Percent_Correct_Above_Threshold) %>%
  bind_rows(
    average_performance_restricted %>%
      select(Threshold, Threshold_Percent, Watershed, Performance = Average_Performance)
  )

# =============================================================================
# DEFINE PLOT STYLING
# =============================================================================

# Define colors inspired by The Economist style
watershed_colors <- c(
  "Kusko" = "#E3120B",      # Economist red
  "Nush" = "#00847E",       # Economist teal  
  "Yukon" = "#00609C",      # Economist blue
  "Average" = "#9E9E9E"     # Neutral grey
)

line_types <- c(
  "Kusko" = "solid",
  "Nush" = "solid", 
  "Yukon" = "solid",
  "Average" = "dashed"
)

# =============================================================================
# CREATE TOTAL DATASET PLOT - FIXED LABEL POSITIONING
# =============================================================================

# Create labels for the end of lines - use actual line positions but adjust x position to avoid overlap
end_labels_total <- combined_line_data_total %>% 
  filter(Threshold_Percent == 90) %>%
  mutate(
    # Stagger both x and y positions to prevent overlap
    label_x = case_when(
      Watershed == "Yukon" ~ 91.2,      # Yukon slightly further right
      Watershed == "Nush" ~ 91.8,       # Nush further right to avoid overlap
      Watershed == "Average" ~ 91.5,    # Average in middle
      Watershed == "Kusko" ~ 91.0,      # Kusko closest
      TRUE ~ 91.5
    ),
    label_y = case_when(
      Watershed == "Yukon" ~ Performance,        # Keep Yukon at line position
      Watershed == "Nush" ~ Performance - 1.5,   # Move Nush down slightly
      Watershed == "Average" ~ Performance + 1,  # Move Average up slightly
      Watershed == "Kusko" ~ Performance,        # Keep Kusko at line position
      TRUE ~ Performance
    )
  )

plot_total <- ggplot(combined_line_data_total, aes(x = Threshold_Percent, y = Performance, 
                                                   color = Watershed, linetype = Watershed)) +
  # Simple, clean lines
  geom_line(linewidth = 1.8, alpha = 0.9) +
  # Simple points
  geom_point(size = 3, alpha = 0.9) +
  # Add labels at the end of each line - staggered horizontally to avoid overlap
  geom_text(data = end_labels_total,
            aes(label = Watershed, x = label_x, y = label_y, color = Watershed),
            hjust = 0, size = 6.5, fontface = "bold", show.legend = FALSE) +
  # Scales
  scale_color_manual(values = watershed_colors) +
  scale_linetype_manual(values = line_types) +
  scale_x_continuous(
    breaks = seq(60, 90, 5),
    labels = function(x) paste0(x, "%"),
    limits = c(60, 100),  # Extended to make room for labels
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    breaks = seq(50, 100, 10),
    labels = function(x) paste0(x, "%"),
    limits = c(50, 100),
    expand = c(0, 0)
  ) +
  # Clean, minimal labels
  labs(
    title = "Total Dataset",
    x = "Probability Threshold",
    y = "Classification Accuracy"
  ) +
  # Clean, Economist-style theme
  theme_minimal(base_size = 16) +
  theme(
    # Clean white background
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    # Subtle grid - only major horizontal lines
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "#E5E5E5", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    # No panel border
    panel.border = element_blank(),
    # Clean typography
    plot.title = element_text(
      hjust = 0, size = 20, face = "bold", 
      color = "#2E2E2E", margin = margin(b = 15)
    ),
    plot.caption = element_text(
      hjust = 0, size = 9, 
      color = "#999999", margin = margin(t = 15)
    ),
    axis.title.x = element_text(
      size = 16, color = "#333333", face = "bold",
      margin = margin(t = 10)
    ),
    axis.title.y = element_text(
      size = 16, color = "#333333", face = "bold",
      margin = margin(r = 10)
    ),
    axis.text = element_text(size = 16, color = "#333333", face = "bold"),
    # Simple axis lines
    axis.line.x = element_line(color = "#CCCCCC", linewidth = 0.3),
    axis.line.y = element_line(color = "#CCCCCC", linewidth = 0.3),
    axis.ticks = element_line(color = "#CCCCCC", linewidth = 0.3),
    axis.ticks.length = unit(2, "pt"),
    # Hide legend since we have direct labels
    legend.position = "none",
    # Clean spacing
    plot.margin = margin(20, 30, 15, 15)  # Extra right margin for labels
  )

# =============================================================================
# CREATE RESTRICTED DATASET PLOT
# =============================================================================

# Create labels for the end of lines with adjusted positions to avoid overlap
end_labels_restricted <- combined_line_data_restricted %>% 
  filter(Threshold_Percent == 90) %>%
  mutate(
    # Adjust y positions to prevent overlap
    label_y = case_when(
      Watershed == "Nush" ~ Performance + 2,    # Move Nush up slightly
      Watershed == "Average" ~ Performance - 2, # Move Average down slightly
      TRUE ~ Performance                        # Keep others at line position
    )
  )

plot_restricted <- ggplot(combined_line_data_restricted, aes(x = Threshold_Percent, y = Performance, 
                                                             color = Watershed, linetype = Watershed)) +
  # Simple, clean lines
  geom_line(linewidth = 1.8, alpha = 0.9) +
  # Simple points
  geom_point(size = 3, alpha = 0.9) +
  # Add labels at the end of each line - increased size with adjusted positions
  geom_text(data = end_labels_restricted,
            aes(label = Watershed, x = Threshold_Percent + 1.5, y = label_y, color = Watershed),
            hjust = 0, size = 6.5, fontface = "bold", show.legend = FALSE) +
  # Scales
  scale_color_manual(values = watershed_colors) +
  scale_linetype_manual(values = line_types) +
  scale_x_continuous(
    breaks = seq(60, 90, 5),
    labels = function(x) paste0(x, "%"),
    limits = c(60, 102),  # Extended to make room for staggered labels
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    breaks = seq(50, 100, 10),
    labels = function(x) paste0(x, "%"),
    limits = c(50, 100),
    expand = c(0, 0)
  ) +
  # Clean, minimal labels
  labs(
    title = "Restricted Dataset",
    x = "Probability Threshold",
    y = "Classification Accuracy"
  ) +
  # Clean, Economist-style theme
  theme_minimal(base_size = 16) +
  theme(
    # Clean white background
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    # Subtle grid - only major horizontal lines
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "#E5E5E5", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    # No panel border
    panel.border = element_blank(),
    # Clean typography
    plot.title = element_text(
      hjust = 0, size = 20, face = "bold", 
      color = "#2E2E2E", margin = margin(b = 15)
    ),
    plot.caption = element_text(
      hjust = 0, size = 9, 
      color = "#999999", margin = margin(t = 15)
    ),
    axis.title.x = element_text(
      size = 16, color = "#666666",face = "bold",
      margin = margin(t = 10)
    ),
    axis.title.y = element_text(
      size = 16, color = "#666666", 
      margin = margin(r = 10)
    ),
    axis.text = element_text(size = 16, color = "#333333", face = "bold"),
    # Simple axis lines
    axis.line.x = element_line(color = "#CCCCCC", linewidth = 0.3),
    axis.line.y = element_line(color = "#CCCCCC", linewidth = 0.3),
    axis.ticks = element_line(color = "#CCCCCC", linewidth = 0.3),
    axis.ticks.length = unit(2, "pt"),
    # Hide legend since we have direct labels
    legend.position = "none",
    # Clean spacing
    plot.margin = margin(20, 40, 15, 15)  # Extra right margin for staggered labels
  )

# =============================================================================
# CREATE COMBINED TWO-PANEL FIGURE
# =============================================================================

# Combine plots using patchwork
combined_plot <- plot_total + plot_restricted + 
  plot_layout(ncol = 2) +
  plot_annotation(
    title = "Watershed Classification Performance Across Probability Thresholds",
    theme = theme(
      plot.title = element_text(hjust = 0.5, size = 24, face = "bold", color = "#2E2E2E"),
      plot.subtitle = element_text(hjust = 0.5, size = 18, color = "#666666"),
      plot.caption = element_text(hjust = 0.5, size = 10, color = "#999999")
    )
  )

# =============================================================================
# SAVE PLOTS
# =============================================================================

# Save individual plots
ggsave(file.path(output_dir, "GAM_RF_TOTAL_Performance_Line_Plot.png"), 
       plot_total, width = 12, height = 8, dpi = 300, bg = "white", 
       device = "png", type = "cairo")

ggsave(file.path(output_dir, "GAM_RF_Restricted_Performance_Line_Plot.png"), 
       plot_restricted, width = 12, height = 8, dpi = 300, bg = "white",
       device = "png", type = "cairo")

# Save combined two-panel figure
ggsave(file.path(output_dir, "GAM_RF_Combined_Performance_Line_Plot.png"), 
       combined_plot, width = 20, height = 10, dpi = 300, bg = "white",
       device = "png", type = "cairo")

# =============================================================================
# COMPLETION MESSAGE
# =============================================================================

cat("\n🎉 ALL PLOTS COMPLETE!\n")
cat(paste(rep("=", 60), collapse = ""), "\n")
cat("Performance line plots created for TOTAL and Restricted datasets\n")
cat("✅ TOTAL plot saved:", "GAM_RF_TOTAL_Performance_Line_Plot.png\n")
cat("✅ Restricted plot saved:", "GAM_RF_Restricted_Performance_Line_Plot.png\n")
cat("✅ COMBINED plot saved:", "GAM_RF_Combined_Performance_Line_Plot.png\n")
cat("📁 Location:", output_dir, "\n")
cat("📝 Features: Direct line labeling, Economist-style theme, clean layout, two-panel combined figure\n")