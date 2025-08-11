# =============================================================================
# SIMPLE PROBABILITY THRESHOLD ANALYSIS - BOTH TOTAL AND OVERLAP
# =============================================================================
# Test different probability thresholds to see correct assignment rates by watershed
# Creates individual and combined figures for both datasets
# =============================================================================

library(tidyverse)
library(probably)
library(ggplot2)
library(scales)
library(viridis)
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
# FUNCTION TO PROCESS DATASET AND CREATE FIGURES
# =============================================================================

process_dataset <- function(analysis_type) {
  
  cat("\n", paste(rep("=", 60), collapse = ""), "\n")
  cat("PROCESSING", analysis_type, "DATASET\n")
  cat(paste(rep("=", 60), collapse = ""), "\n")
  
  # Load data based on analysis type
  results_dir <- if (analysis_type == "TOTAL") results_dir_total else results_dir_overlap
  
  # Load original predictions and calibration
  predictions <- read.csv(file.path(results_dir, paste0("GAM_RF_", analysis_type, "_predictions.csv"))) %>%
    mutate(Watershed = as.factor(Watershed), .pred_class = as.factor(.pred_class))
  
  calibration <- readRDS(file.path(calibrated_models_dir, paste0("GAM_RF_", analysis_type, "_calibration.rds")))
  
  # Apply calibration
  calibrated_predictions <- cal_apply(predictions, calibration)
  
  cat("Total samples:", nrow(calibrated_predictions), "\n")
  cat("Watersheds:", table(calibrated_predictions$Watershed), "\n")
  
  # =============================================================================
  # GET MAXIMUM PROBABILITY FOR EACH SAMPLE
  # =============================================================================
  
  # First, let's examine the probability columns
  prob_cols <- calibrated_predictions %>% select(starts_with(".pred_")) %>% select(-any_of(".pred_class"))
  cat("Probability columns found:", colnames(prob_cols), "\n")
  
  # Get the column names and values for first few rows
  pred_cols <- grep("^\\.pred_", colnames(calibrated_predictions), value = TRUE)
  pred_cols <- pred_cols[pred_cols != ".pred_class"]  # Remove .pred_class if it exists
  cat("Prediction probability columns:", pred_cols, "\n")
  
  # Alternative simpler approach
  if (length(pred_cols) == 3) {
    sample_results <- calibrated_predictions %>%
      select(Watershed, all_of(pred_cols)) %>%
      mutate(
        # Extract probabilities for each watershed
        prob1 = .[[pred_cols[1]]],
        prob2 = .[[pred_cols[2]]],
        prob3 = .[[pred_cols[3]]],
        # Find max probability
        Max_Probability = pmax(prob1, prob2, prob3, na.rm = TRUE),
        # Find predicted watershed
        Predicted_Watershed = case_when(
          prob1 == Max_Probability ~ gsub("\\.pred_", "", pred_cols[1]),
          prob2 == Max_Probability ~ gsub("\\.pred_", "", pred_cols[2]),
          prob3 == Max_Probability ~ gsub("\\.pred_", "", pred_cols[3]),
          TRUE ~ "Unknown"
        ),
        # Check correctness
        Correct = (as.character(Watershed) == Predicted_Watershed)
      ) %>%
      select(Watershed, Predicted_Watershed, Max_Probability, Correct)
    
    cat("Using alternative approach for", analysis_type, "\n")
  }
  
  cat("Sample results summary:\n")
  print(head(sample_results))
  cat("Overall accuracy:", round(mean(sample_results$Correct), 3), "\n")
  
  # =============================================================================
  # TEST DIFFERENT PROBABILITY THRESHOLDS
  # =============================================================================
  
  # Define thresholds to test
  thresholds <- c(0.5, 0.6, 0.7, 0.8, 0.85, 0.9, 0.95)
  
  cat("\n=== THRESHOLD ANALYSIS FOR", analysis_type, "===\n")
  
  # Create results table
  threshold_results <- map_dfr(thresholds, function(thresh) {
    
    # For each watershed, see how many samples are above threshold AND correct
    watershed_results <- map_dfr(c("Kusko", "Nush", "Yukon"), function(ws) {
      
      # Get samples for this watershed
      watershed_samples <- sample_results %>% filter(Watershed == ws)
      
      # Find samples above threshold
      above_threshold <- watershed_samples %>% filter(Max_Probability >= thresh)
      
      # Find samples above threshold AND correct
      above_threshold_correct <- above_threshold %>% filter(Correct == TRUE)
      
      data.frame(
        Threshold = thresh,
        Watershed = ws,
        Total_Samples = nrow(watershed_samples),
        Above_Threshold = nrow(above_threshold),
        Above_Threshold_Correct = nrow(above_threshold_correct),
        Percent_Above_Threshold = round(nrow(above_threshold) / nrow(watershed_samples) * 100, 1),
        Percent_Correct_Above_Threshold = round(nrow(above_threshold_correct) / nrow(watershed_samples) * 100, 1),
        Accuracy_Above_Threshold = ifelse(nrow(above_threshold) > 0, 
                                          round(nrow(above_threshold_correct) / nrow(above_threshold) * 100, 1), 
                                          NA)
      )
    })
    
    return(watershed_results)
  })
  
  # =============================================================================
  # DISPLAY RESULTS
  # =============================================================================
  
  # Print results in a nice format
  cat("Results: Percent of each watershed correctly assigned above each threshold\n")
  cat(paste(rep("=", 80), collapse = ""), "\n")
  
  for (thresh in thresholds) {
    cat(sprintf("\nThreshold: %.0f%%\n", thresh * 100))
    cat(paste(rep("-", 30), collapse = ""), "\n")
    
    thresh_data <- threshold_results %>% filter(Threshold == thresh)
    
    for (i in 1:nrow(thresh_data)) {
      row <- thresh_data[i,]
      cat(sprintf("%s: %s/%s samples (%.1f%%) correctly assigned above threshold\n",
                  row$Watershed,
                  row$Above_Threshold_Correct,
                  row$Total_Samples,
                  row$Percent_Correct_Above_Threshold))
    }
    
    total_samples <- sum(thresh_data$Total_Samples)  # Sum all watershed samples
    total_correct_above <- sum(thresh_data$Above_Threshold_Correct)
    overall_percent <- round(total_correct_above / total_samples * 100, 1)
    
    cat(sprintf("OVERALL: %s/%s samples (%.1f%%) correctly assigned above threshold\n",
                total_correct_above, total_samples, overall_percent))
  }
  
  # =============================================================================
  # CREATE SUMMARY TABLE
  # =============================================================================
  
  # Create a wide format table for easy viewing
  summary_table <- threshold_results %>%
    select(Threshold, Watershed, Percent_Correct_Above_Threshold) %>%
    pivot_wider(names_from = Watershed, values_from = Percent_Correct_Above_Threshold) %>%
    rowwise() %>%
    mutate(
      Overall = round(mean(c(Kusko, Nush, Yukon)), 1)
    ) %>%
    ungroup()
  
  cat("\n=== SUMMARY TABLE FOR", analysis_type, "===\n")
  cat("Percent of each watershed correctly assigned above threshold:\n\n")
  print(summary_table)
  
  # =============================================================================
  # SAVE RESULTS
  # =============================================================================
  
  write.csv(threshold_results, file.path(output_dir, paste0("GAM_RF_", analysis_type, "_Threshold_Analysis.csv")), row.names = FALSE)
  write.csv(summary_table, file.path(output_dir, paste0("GAM_RF_", analysis_type, "_Threshold_Summary.csv")), row.names = FALSE)
  
  cat("\n✅ Results saved for", analysis_type, "to:", output_dir, "\n")
  
  # Return the threshold_results for plotting
  return(threshold_results)
}

# =============================================================================
# FUNCTION TO CREATE FIGURES
# =============================================================================

create_figures <- function(threshold_results, analysis_type) {
  
  cat("\n📊 Creating figures for", analysis_type, "dataset...\n")
  
  # Calculate overall performance for each threshold
  overall_performance <- threshold_results %>%
    group_by(Threshold) %>%
    summarise(
      Total_Samples = sum(Total_Samples),
      Total_Correct_Above = sum(Above_Threshold_Correct),
      Overall_Percent = round(Total_Correct_Above / Total_Samples * 100, 1),
      .groups = "drop"
    )
  
  # Define colors and styling
  watershed_colors <- c("Kusko" = "#d32f2f", "Nush" = "#388e3c", "Yukon" = "#1976d2")
  bg_color <- "#fafafa"
  text_color <- "#2e2e2e"
  grid_color <- "#e0e0e0"
  
  # =============================================================================
  # PLOT 1: MAIN LINE PLOT WITH AREA FILL
  # =============================================================================
  
  p1 <- ggplot(threshold_results, aes(x = Threshold * 100, y = Percent_Correct_Above_Threshold, 
                                      color = Watershed, fill = Watershed)) +
    # Add subtle area fill
    geom_area(alpha = 0.15, position = "identity") +
    # Add main lines
    geom_line(size = 2.5, alpha = 0.9) +
    geom_line(size = 1.5, color = "white", alpha = 0.6) +  # White inner line for depth
    # Add points
    geom_point(size = 4, alpha = 0.9, stroke = 0) +
    geom_point(size = 2.5, color = "white", alpha = 0.8) +  # White inner points
    # Add overall performance line
    geom_line(data = overall_performance, aes(x = Threshold * 100, y = Overall_Percent),
              color = text_color, size = 2, linetype = "dashed", alpha = 0.7, inherit.aes = FALSE) +
    geom_point(data = overall_performance, aes(x = Threshold * 100, y = Overall_Percent),
               color = text_color, size = 3, shape = 18, inherit.aes = FALSE) +
    # Scales and styling
    scale_color_manual(values = watershed_colors, name = "Watershed") +
    scale_fill_manual(values = watershed_colors, name = "Watershed") +
    scale_x_continuous(
      breaks = seq(50, 95, 5),
      labels = function(x) paste0(x, "%"),
      expand = expansion(mult = c(0.02, 0.02))
    ) +
    scale_y_continuous(
      breaks = seq(0, 100, 10),
      labels = function(x) paste0(x, "%"),
      limits = c(0, 100),
      expand = expansion(mult = c(0.02, 0.02))
    ) +
    # Labels and theme
    labs(
      title = paste("Watershed Classification Performance -", analysis_type),
      subtitle = paste("GAM Random Forest Model • Percent correctly assigned above probability threshold\nDashed line shows overall performance across all watersheds"),
      x = "Probability Threshold",
      y = "Percent Correctly Assigned",
      caption = "Higher thresholds increase confidence but reduce sample retention"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      # Plot background
      plot.background = element_rect(fill = bg_color, color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      panel.grid.major = element_line(color = grid_color, size = 0.5, linetype = "dotted"),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "#cccccc", fill = NA, size = 0.5),
      # Text styling
      plot.title = element_text(hjust = 0.5, margin = margin(b = 5), color = text_color, size = 18, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 20), color = "#666666", size = 12),
      plot.caption = element_text(hjust = 0.5, color = "#999999", size = 11, margin = margin(t = 15)),
      axis.title = element_text(size = 14, color = text_color, face = "bold"),
      axis.text = element_text(size = 12, color = text_color),
      # Legend styling
      legend.position = "bottom",
      legend.title = element_text(size = 13, face = "bold", color = text_color),
      legend.text = element_text(size = 12, color = text_color),
      legend.key.size = unit(1.2, "cm"),
      legend.margin = margin(t = 20),
      legend.box.background = element_rect(fill = "white", color = "#cccccc"),
      legend.box.margin = margin(10, 10, 10, 10),
      # Plot margins
      plot.margin = margin(25, 25, 25, 25)
    ) +
    guides(
      color = guide_legend(override.aes = list(size = 4, alpha = 1)),
      fill = "none"
    )
  
  # =============================================================================
  # PLOT 2: SAMPLE RETENTION HEATMAP
  # =============================================================================
  
  # Calculate sample retention data
  retention_data <- threshold_results %>%
    select(Threshold, Watershed, Above_Threshold, Total_Samples) %>%
    mutate(
      Retention_Rate = Above_Threshold / Total_Samples,
      Threshold_Label = paste0(Threshold * 100, "%")
    )
  
  p2 <- ggplot(retention_data, aes(x = Threshold_Label, y = Watershed, fill = Retention_Rate)) +
    geom_tile(color = "white", size = 2) +
    geom_text(aes(label = paste0(round(Retention_Rate * 100, 0), "%")), 
              color = "white", size = 5, fontface = "bold") +
    scale_fill_viridis_c(
      option = "plasma",
      direction = -1,
      labels = percent_format(),
      name = "Sample\nRetention",
      guide = guide_colorbar(
        title.position = "top",
        title.hjust = 0.5,
        barwidth = 12,
        barheight = 1
      )
    ) +
    labs(
      title = paste("Sample Retention by Threshold -", analysis_type),
      subtitle = "Percent of samples retained above each probability threshold",
      x = "Probability Threshold",
      y = "Watershed"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.background = element_rect(fill = bg_color, color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      panel.grid = element_blank(),
      plot.title = element_text(hjust = 0.5, size = 16, color = text_color, margin = margin(b = 5), face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 12, color = "#666666", margin = margin(b = 15)),
      axis.title = element_text(size = 12, color = text_color, face = "bold"),
      axis.text = element_text(size = 11, color = text_color),
      legend.position = "bottom",
      legend.title = element_text(size = 11, face = "bold"),
      legend.margin = margin(t = 15),
      plot.margin = margin(20, 20, 20, 20)
    )
  
  # =============================================================================
  # PLOT 3: KEY THRESHOLD COMPARISON
  # =============================================================================
  
  # Focus on key thresholds
  key_thresholds <- threshold_results %>%
    filter(Threshold %in% c(0.7, 0.8, 0.9)) %>%
    mutate(
      Threshold_Label = paste0(Threshold * 100, "%"),
      Performance_Category = case_when(
        Percent_Correct_Above_Threshold >= 90 ~ "Excellent (≥90%)",
        Percent_Correct_Above_Threshold >= 80 ~ "Good (80-89%)",
        Percent_Correct_Above_Threshold >= 70 ~ "Fair (70-79%)",
        TRUE ~ "Poor (<70%)"
      )
    )
  
  p3 <- ggplot(key_thresholds, aes(x = Watershed, y = Percent_Correct_Above_Threshold, fill = Watershed)) +
    geom_col(alpha = 0.8, width = 0.7) +
    geom_text(aes(label = paste0(round(Percent_Correct_Above_Threshold, 1), "%")), 
              vjust = -0.5, size = 4, fontface = "bold", color = text_color) +
    facet_wrap(~paste("Threshold:", Threshold_Label), ncol = 3) +
    scale_fill_manual(values = watershed_colors) +
    scale_y_continuous(
      limits = c(0, 105),
      breaks = seq(0, 100, 20),
      labels = function(x) paste0(x, "%")
    ) +
    labs(
      title = paste("Performance at Key Thresholds -", analysis_type),
      subtitle = "Comparison of watershed classification success",
      x = "Watershed",
      y = "Percent Correctly Assigned"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.background = element_rect(fill = bg_color, color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      panel.grid.major.y = element_line(color = grid_color, size = 0.3),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = "#e8e8e8", color = "#cccccc"),
      strip.text = element_text(size = 12, face = "bold", color = text_color),
      plot.title = element_text(hjust = 0.5, size = 16, color = text_color, margin = margin(b = 5), face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 12, color = "#666666", margin = margin(b = 15)),
      axis.title = element_text(size = 12, color = text_color, face = "bold"),
      axis.text = element_text(size = 10, color = text_color),
      legend.position = "none",
      plot.margin = margin(20, 20, 20, 20)
    )
  
  # =============================================================================
  # SAVE INDIVIDUAL PLOTS
  # =============================================================================
  
  # Save each plot individually
  ggsave(file.path(output_dir, paste0("GAM_RF_", analysis_type, "_Performance_Lines.png")), 
         p1, width = 14, height = 10, dpi = 300, bg = bg_color)
  
  ggsave(file.path(output_dir, paste0("GAM_RF_", analysis_type, "_Retention_Heatmap.png")), 
         p2, width = 12, height = 8, dpi = 300, bg = bg_color)
  
  ggsave(file.path(output_dir, paste0("GAM_RF_", analysis_type, "_Key_Thresholds.png")), 
         p3, width = 12, height = 8, dpi = 300, bg = bg_color)
  
  # =============================================================================
  # COMBINE ALL PLOTS
  # =============================================================================
  
  # Create the final combined figure
  final_figure <- (p1) / (p2 | p3) + 
    plot_layout(heights = c(2, 1)) +
    plot_annotation(
      title = paste("SR87/86 ISOTOPE CLASSIFICATION THRESHOLD ANALYSIS -", analysis_type),
      subtitle = "GAM Random Forest Model Performance • Western Alaska Chinook Salmon",
      caption = "Higher probability thresholds increase classification confidence but reduce sample retention",
      theme = theme(
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5, color = text_color, margin = margin(b = 5)),
        plot.subtitle = element_text(size = 14, hjust = 0.5, color = "#666666", margin = margin(b = 20)),
        plot.caption = element_text(size = 11, hjust = 0.5, color = "#999999", margin = margin(t = 20)),
        plot.background = element_rect(fill = bg_color, color = NA),
        plot.margin = margin(30, 30, 30, 30)
      )
    )
  
  # Save combined figure
  ggsave(file.path(output_dir, paste0("GAM_RF_", analysis_type, "_Combined_Analysis.png")), 
         final_figure, width = 16, height = 12, dpi = 300, bg = bg_color)
  
  cat("🎨 Individual plots saved:\n")
  cat("  -", paste0("GAM_RF_", analysis_type, "_Performance_Lines.png"), "\n")
  cat("  -", paste0("GAM_RF_", analysis_type, "_Retention_Heatmap.png"), "\n")
  cat("  -", paste0("GAM_RF_", analysis_type, "_Key_Thresholds.png"), "\n")
  cat("  -", paste0("GAM_RF_", analysis_type, "_Combined_Analysis.png"), "\n")
}

# =============================================================================
# MAIN EXECUTION
# =============================================================================

# Process TOTAL dataset
threshold_results_total <- process_dataset("TOTAL")

# Process OVERLAP dataset  
threshold_results_overlap <- process_dataset("OVERLAP")

# Create figures for TOTAL dataset
create_figures(threshold_results_total, "TOTAL")

# Create figures for OVERLAP dataset
create_figures(threshold_results_overlap, "OVERLAP")

cat("\n🎉 ALL ANALYSIS COMPLETE!\n")
cat(paste(rep("=", 60), collapse = ""), "\n")
cat("Files created for both TOTAL and OVERLAP datasets:\n")
cat("📊 Individual plots: Performance lines, retention heatmaps, key thresholds\n")
cat("🎨 Combined figures: All plots in one comprehensive visualization\n")
cat("📈 CSV files: Detailed threshold analysis and summary tables\n")
cat("📁 Location:", output_dir, "\n")
