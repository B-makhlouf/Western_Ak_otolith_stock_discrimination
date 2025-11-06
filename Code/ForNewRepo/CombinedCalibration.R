# =============================================================================
# COMBINED CALIBRATION AND THRESHOLD ANALYSIS
# =============================================================================
# Calibrates Random Forest model probabilities and evaluates threshold performance
# =============================================================================

library(tidymodels)
library(probably)
library(tidyverse)
library(yardstick)
library(cowplot)
library(ggplot2)
library(scales)
library(patchwork)

options(dplyr.summarise.inform = FALSE)
options(tidymodels.quiet = TRUE)

# =============================================================================
# CONFIGURATION
# =============================================================================

# Models to process
data_types <- c("RAW", "GAM", "MA")
analyses <- c("TOTAL", "OVERLAP")

# Input directories
results_dir_total <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/Output/ModelResultsPreCal/Total"
results_dir_overlap <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/Output/ModelResultsPreCal/Filtered"

# Output directory
output_dir <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/Figures/Calibration Figures"
calibrated_models_dir <- file.path(output_dir, "CalibratedModels")

# Create directories
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(calibrated_models_dir, recursive = TRUE, showWarnings = FALSE)

# Probability thresholds
thresholds <- c(0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9)

# =============================================================================
# CALIBRATION FUNCTION
# =============================================================================

calibrate_rf_model <- function(data_type, analysis) {
  
  # Load predictions
  results_dir <- if (analysis == "TOTAL") results_dir_total else results_dir_overlap
  pred_file <- file.path(results_dir, paste0(data_type, "_RF_", analysis, "_predictions.csv"))
  
  if (!file.exists(pred_file)) {
    return(NULL)
  }
  
  predictions <- read.csv(pred_file, stringsAsFactors = FALSE) %>%
    mutate(Watershed = as.factor(Watershed), 
           .pred_class = as.factor(.pred_class))
  
  # Calculate pre-calibration metrics
  prob_cols <- predictions %>% 
    select(starts_with(".pred_")) %>% 
    select(-any_of(".pred_class"))
  
  before_log_loss <- mn_log_loss_vec(predictions$Watershed, as.matrix(prob_cols))
  before_brier <- brier_class_vec(predictions$Watershed, as.matrix(prob_cols))
  
  # Create before plot
  beforecal <- cal_plot_windowed(predictions, truth = Watershed, 
                                 window_size = 0.3, step_size = 0.02) +
    labs(title = "Before Calibration") + 
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14), 
          legend.position = "bottom")
  
  # Perform calibration
  calibration <- cal_estimate_multinomial(predictions, truth = Watershed, 
                                          estimate = starts_with(".pred_"), 
                                          method = "isotonic")
  
  calibrated_predictions <- cal_apply(predictions, calibration)
  
  # Calculate post-calibration metrics
  prob_cols_after <- calibrated_predictions %>% 
    select(starts_with(".pred_")) %>% 
    select(-any_of(".pred_class"))
  
  after_log_loss <- mn_log_loss_vec(calibrated_predictions$Watershed, 
                                    as.matrix(prob_cols_after))
  after_brier <- brier_class_vec(calibrated_predictions$Watershed, 
                                 as.matrix(prob_cols_after))
  
  # Create after plot
  aftercal <- cal_plot_windowed(calibrated_predictions, truth = Watershed, 
                                window_size = 0.3, step_size = 0.02) +
    labs(title = "After Calibration") + 
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14), 
          legend.position = "bottom")
  
  # Combine plots
  combined_plot <- plot_grid(beforecal, aftercal, ncol = 2, labels = c("A", "B"))
  title <- ggdraw() + 
    draw_label(paste("RF Calibration:", data_type, "-", analysis), 
               fontface = "bold", size = 16)
  final_plot <- plot_grid(title, combined_plot, ncol = 1, rel_heights = c(0.1, 0.9))
  
  # Save figure
  ggsave(file.path(output_dir, paste0("RF_Calibration_", data_type, "_", analysis, ".png")), 
         final_plot, width = 12, height = 6, dpi = 300, bg = "white")
  
  # Save calibration mapping
  saveRDS(calibration, file.path(calibrated_models_dir, 
                                 paste0(data_type, "_RF_", analysis, "_calibration.rds")))
  
  # Return results
  return(data.frame(
    Data_Type = data_type, 
    Analysis = analysis,
    Before_Log_Loss = before_log_loss, 
    After_Log_Loss = after_log_loss, 
    Log_Loss_Improvement = before_log_loss - after_log_loss,
    Before_Brier = before_brier, 
    After_Brier = after_brier, 
    Brier_Improvement = before_brier - after_brier,
    stringsAsFactors = FALSE
  ))
}

# =============================================================================
# THRESHOLD ANALYSIS FUNCTION
# =============================================================================

threshold_analysis <- function(data_type, analysis) {
  
  # Load predictions
  results_dir <- if (analysis == "TOTAL") results_dir_total else results_dir_overlap
  pred_file <- file.path(results_dir, paste0(data_type, "_RF_", analysis, "_predictions.csv"))
  
  if (!file.exists(pred_file)) {
    return(NULL)
  }
  
  # Load calibration
  calibration_file <- file.path(calibrated_models_dir, 
                                paste0(data_type, "_RF_", analysis, "_calibration.rds"))
  
  if (!file.exists(calibration_file)) {
    return(NULL)
  }
  
  predictions <- read.csv(pred_file, stringsAsFactors = FALSE) %>%
    mutate(Watershed = as.factor(Watershed), 
           .pred_class = as.factor(.pred_class))
  
  calibration <- readRDS(calibration_file)
  calibrated_predictions <- cal_apply(predictions, calibration)
  
  # Get prediction columns
  pred_cols <- grep("^\\.pred_", colnames(calibrated_predictions), value = TRUE)
  pred_cols <- pred_cols[pred_cols != ".pred_class"]
  
  # Extract max probabilities and predictions
  sample_results <- calibrated_predictions %>%
    select(Watershed, all_of(pred_cols)) %>%
    mutate(
      prob1 = .[[pred_cols[1]]],
      prob2 = .[[pred_cols[2]]],
      prob3 = .[[pred_cols[3]]],
      Max_Probability = pmax(prob1, prob2, prob3, na.rm = TRUE),
      Predicted_Watershed = case_when(
        prob1 == Max_Probability ~ gsub("\\.pred_", "", pred_cols[1]),
        prob2 == Max_Probability ~ gsub("\\.pred_", "", pred_cols[2]),
        prob3 == Max_Probability ~ gsub("\\.pred_", "", pred_cols[3]),
        TRUE ~ "Unknown"
      ),
      Correct = (as.character(Watershed) == Predicted_Watershed)
    ) %>%
    select(Watershed, Predicted_Watershed, Max_Probability, Correct)
  
  # Calculate threshold performance
  threshold_results <- map_dfr(thresholds, function(thresh) {
    watershed_results <- map_dfr(c("Kusko", "Nush", "Yukon"), function(ws) {
      watershed_samples <- sample_results %>% filter(Watershed == ws)
      above_threshold_correct <- watershed_samples %>% 
        filter(Max_Probability >= thresh, Correct == TRUE)
      
      data.frame(
        Threshold = thresh,
        Watershed = ws,
        Total_Samples = nrow(watershed_samples),
        Above_Threshold_Correct = nrow(above_threshold_correct),
        Percent_Correct_Above_Threshold = round(
          nrow(above_threshold_correct) / nrow(watershed_samples) * 100, 1
        )
      )
    })
    return(watershed_results)
  })
  
  # Prepare plot data
  line_plot_data <- threshold_results %>%
    select(Threshold, Watershed, Percent_Correct_Above_Threshold) %>%
    mutate(Threshold_Percent = Threshold * 100)
  
  # Calculate average
  average_performance <- line_plot_data %>%
    group_by(Threshold, Threshold_Percent) %>%
    summarise(
      Average_Performance = mean(Percent_Correct_Above_Threshold, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(Watershed = "Average")
  
  # Combine data
  combined_line_data <- line_plot_data %>%
    select(Threshold, Threshold_Percent, Watershed, 
           Performance = Percent_Correct_Above_Threshold) %>%
    bind_rows(
      average_performance %>%
        select(Threshold, Threshold_Percent, Watershed, Performance = Average_Performance)
    )
  
  return(list(
    threshold_results = threshold_results,
    line_plot_data = combined_line_data,
    sample_results = sample_results
  ))
}

# =============================================================================
# THRESHOLD PLOT STYLING
# =============================================================================

# Economist-style colors
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
# MAIN EXECUTION
# =============================================================================

cat("\n=== CALIBRATION AND THRESHOLD ANALYSIS ===\n\n")

# STEP 1: Calibrate all models
cat("Step 1: Calibrating RF models...\n")

calibration_results <- map_dfr(analyses, function(analysis) {
  map_dfr(data_types, function(data_type) {
    calibrate_rf_model(data_type, analysis)
  })
})

write.csv(calibration_results, 
          file.path(output_dir, "RF_Calibration_Summary.csv"), 
          row.names = FALSE)

cat("  Calibrated", nrow(calibration_results), "models\n\n")

# STEP 2: Threshold analysis
cat("Step 2: Analyzing threshold performance...\n")

threshold_results_list <- list()
threshold_results_list$GAM_TOTAL <- threshold_analysis("GAM", "TOTAL")
threshold_results_list$GAM_OVERLAP <- threshold_analysis("GAM", "OVERLAP")

cat("  Threshold analysis complete\n\n")

# STEP 3: Create figures
cat("Step 3: Generating figures...\n")

if (!is.null(threshold_results_list$GAM_TOTAL) && 
    !is.null(threshold_results_list$GAM_OVERLAP)) {
  
  # Get data for plots
  data_total <- threshold_results_list$GAM_TOTAL$line_plot_data
  data_restricted <- threshold_results_list$GAM_OVERLAP$line_plot_data
  
  # ============================================================================
  # CREATE TOTAL DATASET PLOT
  # ============================================================================
  
  # Create labels with staggered positions
  end_labels_total <- data_total %>% 
    filter(Threshold_Percent == 90) %>%
    mutate(
      label_x = case_when(
        Watershed == "Yukon" ~ 91.2,
        Watershed == "Nush" ~ 91.8,
        Watershed == "Average" ~ 91.5,
        Watershed == "Kusko" ~ 91.0,
        TRUE ~ 91.5
      ),
      label_y = case_when(
        Watershed == "Yukon" ~ Performance,
        Watershed == "Nush" ~ Performance - 1.5,
        Watershed == "Average" ~ Performance + 1,
        Watershed == "Kusko" ~ Performance,
        TRUE ~ Performance
      )
    )
  
  plot_total <- ggplot(data_total, aes(x = Threshold_Percent, y = Performance, 
                                       color = Watershed, linetype = Watershed)) +
    geom_line(linewidth = 1.8, alpha = 0.9) +
    geom_point(size = 3, alpha = 0.9) +
    geom_text(data = end_labels_total,
              aes(label = Watershed, x = label_x, y = label_y, color = Watershed),
              hjust = 0, size = 6.5, fontface = "bold", show.legend = FALSE) +
    scale_color_manual(values = watershed_colors) +
    scale_linetype_manual(values = line_types) +
    scale_x_continuous(
      breaks = seq(60, 90, 5),
      labels = function(x) paste0(x, "%"),
      limits = c(60, 100),
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      breaks = seq(50, 100, 10),
      labels = function(x) paste0(x, "%"),
      limits = c(50, 100),
      expand = c(0, 0)
    ) +
    labs(
      title = "Total Dataset",
      x = "Probability Threshold",
      y = "Classification Accuracy"
    ) +
    theme_minimal(base_size = 16) +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "#E5E5E5", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      plot.title = element_text(
        hjust = 0, size = 20, face = "bold", 
        color = "#2E2E2E", margin = margin(b = 15)
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
      axis.line.x = element_line(color = "#CCCCCC", linewidth = 0.3),
      axis.line.y = element_line(color = "#CCCCCC", linewidth = 0.3),
      axis.ticks = element_line(color = "#CCCCCC", linewidth = 0.3),
      axis.ticks.length = unit(2, "pt"),
      legend.position = "none",
      plot.margin = margin(20, 30, 15, 15)
    )
  
  # ============================================================================
  # CREATE RESTRICTED DATASET PLOT
  # ============================================================================
  
  # Create labels with adjusted positions
  end_labels_restricted <- data_restricted %>% 
    filter(Threshold_Percent == 90) %>%
    mutate(
      label_y = case_when(
        Watershed == "Nush" ~ Performance + 2,
        Watershed == "Average" ~ Performance - 2,
        TRUE ~ Performance
      )
    )
  
  plot_restricted <- ggplot(data_restricted, aes(x = Threshold_Percent, y = Performance, 
                                                 color = Watershed, linetype = Watershed)) +
    geom_line(linewidth = 1.8, alpha = 0.9) +
    geom_point(size = 3, alpha = 0.9) +
    geom_text(data = end_labels_restricted,
              aes(label = Watershed, x = Threshold_Percent + 1.5, y = label_y, color = Watershed),
              hjust = 0, size = 6.5, fontface = "bold", show.legend = FALSE) +
    scale_color_manual(values = watershed_colors) +
    scale_linetype_manual(values = line_types) +
    scale_x_continuous(
      breaks = seq(60, 90, 5),
      labels = function(x) paste0(x, "%"),
      limits = c(60, 102),
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      breaks = seq(50, 100, 10),
      labels = function(x) paste0(x, "%"),
      limits = c(50, 100),
      expand = c(0, 0)
    ) +
    labs(
      title = "Restricted Dataset",
      x = "Probability Threshold",
      y = "Classification Accuracy"
    ) +
    theme_minimal(base_size = 16) +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "#E5E5E5", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      plot.title = element_text(
        hjust = 0, size = 20, face = "bold", 
        color = "#2E2E2E", margin = margin(b = 15)
      ),
      axis.title.x = element_text(
        size = 16, color = "#666666", face = "bold",
        margin = margin(t = 10)
      ),
      axis.title.y = element_text(
        size = 16, color = "#666666",
        margin = margin(r = 10)
      ),
      axis.text = element_text(size = 16, color = "#333333", face = "bold"),
      axis.line.x = element_line(color = "#CCCCCC", linewidth = 0.3),
      axis.line.y = element_line(color = "#CCCCCC", linewidth = 0.3),
      axis.ticks = element_line(color = "#CCCCCC", linewidth = 0.3),
      axis.ticks.length = unit(2, "pt"),
      legend.position = "none",
      plot.margin = margin(20, 40, 15, 15)
    )
  
  # ============================================================================
  # COMBINE AND SAVE
  # ============================================================================
  
  # Combined plot
  combined_plot <- plot_total + plot_restricted + 
    plot_layout(ncol = 2) +
    plot_annotation(
      title = "Watershed Classification Performance Across Probability Thresholds",
      theme = theme(
        plot.title = element_text(hjust = 0.5, size = 24, face = "bold", 
                                  color = "#2E2E2E")
      )
    )
  
  # Save plots
  ggsave(file.path(output_dir, "GAM_RF_TOTAL_Performance_Line_Plot.png"), 
         plot_total, width = 12, height = 8, dpi = 300, bg = "white", 
         device = "png", type = "cairo")
  
  ggsave(file.path(output_dir, "GAM_RF_Restricted_Performance_Line_Plot.png"), 
         plot_restricted, width = 12, height = 8, dpi = 300, bg = "white",
         device = "png", type = "cairo")
  
  ggsave(file.path(output_dir, "GAM_RF_Combined_Performance_Line_Plot.png"), 
         combined_plot, width = 20, height = 10, dpi = 300, bg = "white",
         device = "png", type = "cairo")
  
  cat("  Figures saved\n\n")
}

cat("=== COMPLETE ===\n")
cat("Output directory:", output_dir, "\n\n")