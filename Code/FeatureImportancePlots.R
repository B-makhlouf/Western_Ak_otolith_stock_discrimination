# Feature Importance Timeseries Visualization
# Creates PCA loadings-style plots colored by Random Forest feature importance values
# Based on PCA_Loadings_Individual_TimeSeries.R structure

# =============================================================================
# SETUP
# =============================================================================
library(tidyverse)
library(ggplot2)
library(viridis)
library(cowplot)
library(patchwork)
library(scales)

# Set seed for reproducible random sampling
set.seed(42)

# =============================================================================
# PATHS AND CONFIGURATION
# =============================================================================

# Original feature importance paths
IMPORTANCE_DIR <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/Output/FeatureImportance"
OUTPUT_DIR <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/Figures/FeatureImportance"

# Data paths (same as PCA script)
gam_data_path <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/data/LA_Data/Preprocessed_ts_matrices/NatalToMarine_Processed_GAM.csv"
raw_data_path <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/data/LA_Data/Preprocessed_ts_matrices/NatalToMarine_Processed_RAW.csv"

# New output directory for feature importance timeseries
feature_importance_ts_dir <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/Figures/FeatureImportance_Timeseries"

# Create output directories
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(feature_importance_ts_dir, recursive = TRUE, showWarnings = FALSE)

# Target fish IDs (same as in your PCA script)
target_fish_ids <- c("2011_nk_87", "2015_yk_422", "2019_kk_143")

# Colors
watershed_colors <- c("Kusko" = "firebrick", "Nush" = "darkgreen", "Yukon" = "dodgerblue")

# =============================================================================
# ORIGINAL FEATURE IMPORTANCE PLOTS
# =============================================================================

# Load feature importance data
gam_files <- list.files(IMPORTANCE_DIR, pattern = "GAM_RF.*feature_importance.csv", full.names = TRUE)
if (length(gam_files) == 0) {
  stop("No GAM feature importance files found in: ", IMPORTANCE_DIR)
}

importance_df <- read.csv(gam_files[1])
cat("Loaded feature importance data with", nrow(importance_df), "features\n")

# PLOT 1: Feature Importance Line Plot
p1 <- ggplot(importance_df, aes(x = Time_Point, y = Importance)) +
  geom_line(color = "steelblue", linewidth = 1.2) +
  geom_point(aes(color = Importance_Normalized), size = 2) +
  scale_color_viridis_c(name = "Normalized\nImportance", option = "plasma", direction = -1) +
  labs(
    title = "Random Forest Feature Importance Across Time Series",
    subtitle = "GAM Smoothed Sr87/86 Data",
    x = "Time Point Index",
    y = "Variable Importance",
    caption = "Higher values indicate greater importance for watershed classification"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  )

# PLOT 2: Feature Importance Regions
threshold <- quantile(importance_df$Importance, 0.8)
importance_df$High_Importance <- importance_df$Importance >= threshold
importance_df$Region <- cumsum(c(1, diff(importance_df$High_Importance) != 0))

high_regions <- importance_df %>%
  filter(High_Importance) %>%
  group_by(Region) %>%
  summarise(
    Start_Point = min(Time_Point),
    End_Point = max(Time_Point),
    Region_Length = n(),
    .groups = "drop"
  ) %>%
  filter(Region_Length >= 3)

p2 <- ggplot(importance_df, aes(x = Time_Point, y = Importance)) +
  geom_line(color = "gray60", linewidth = 0.8) +
  geom_point(aes(color = High_Importance), size = 2, alpha = 0.7) +
  geom_hline(yintercept = threshold, linetype = "dashed", color = "red", alpha = 0.7) +
  scale_color_manual(
    values = c("FALSE" = "gray70", "TRUE" = "red"),
    name = "Top 20%",
    labels = c("Lower importance", "High importance")
  ) +
  labs(
    title = "Feature Importance Regions",
    subtitle = "Red line shows 80th percentile threshold",
    x = "Time Point Index",
    y = "Variable Importance",
    caption = paste("Found", nrow(high_regions), "high-importance regions")
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "bottom"
  )

# Add shaded regions
if (nrow(high_regions) > 0) {
  for (i in 1:nrow(high_regions)) {
    p2 <- p2 + annotate("rect", 
                        xmin = high_regions$Start_Point[i], 
                        xmax = high_regions$End_Point[i],
                        ymin = -Inf, ymax = Inf, 
                        alpha = 0.1, fill = "red")
  }
}

# Save original plots
ggsave(file.path(OUTPUT_DIR, "RF_GAM_Feature_Importance_Line.png"), p1, width = 12, height = 8, dpi = 300)
ggsave(file.path(OUTPUT_DIR, "RF_GAM_Importance_Regions.png"), p2, width = 14, height = 8, dpi = 300)

cat("✓ Original plots saved to:", OUTPUT_DIR, "\n")

# =============================================================================
# LOAD TIMESERIES DATA
# =============================================================================

# Load GAM and RAW data (same as PCA script)
gam_data_full <- read.csv(gam_data_path) %>%
  mutate(Watershed = as.factor(Watershed))

raw_data_full <- read.csv(raw_data_path) %>%
  mutate(Watershed = as.factor(Watershed))

# Create same natal origin subset (0.7075 to 0.7080) - same as PCA script
gam_data_same_no <- gam_data_full %>%
  filter(Natal_Iso >= 0.7075 & Natal_Iso <= 0.7080)

raw_data_same_no <- raw_data_full %>%
  filter(Natal_Iso >= 0.7075 & Natal_Iso <= 0.7080)

metadata_cols <- c("Fish_id", "Watershed", "Natal_Iso", "Year", "Natal_Start", 
                   "Marine_Start", "Marine_End", "Original_Data_Points", 
                   "Interpolated_Points", "Micron_Size")
feature_cols <- grep("^X", names(gam_data_full), value = TRUE)

# Extract time points from feature column names
time_points <- as.numeric(gsub("^X", "", feature_cols))

cat("GAM SAME NO dataset (0.7075-0.7080):", nrow(gam_data_same_no), "samples with", length(feature_cols), "time points\n")

# =============================================================================
# CREATE FEATURE IMPORTANCE LOOKUP
# =============================================================================

# Create feature importance lookup table
importance_lookup <- setNames(importance_df$Importance_Normalized, importance_df$Feature)

# Get the range for consistent color scaling
importance_range <- range(importance_df$Importance_Normalized, na.rm = TRUE)

cat("Feature importance range:", round(importance_range[1], 3), "to", round(importance_range[2], 3), "\n")

# =============================================================================
# CREATE THREE-PANEL FIGURE WITH FEATURE IMPORTANCE
# =============================================================================

cat("\nCreating three-panel feature importance figure for specific fish...\n")

# Check which target fish are available in the same NO dataset
available_fish <- gam_data_same_no %>%
  filter(Fish_id %in% target_fish_ids) %>%
  select(Fish_id, Watershed, Natal_Iso) %>%
  arrange(match(Fish_id, target_fish_ids))

cat("Target fish found for feature importance figure:\n")
print(available_fish)

if(nrow(available_fish) >= 1) {
  
  # Function to create individual panels for the combined figure
  create_fish_importance_panel <- function(fish_id, show_legend = FALSE, show_y_title = FALSE, is_middle = FALSE) {
    
    # Get fish data
    fish_data_gam <- gam_data_same_no %>% filter(Fish_id == fish_id)
    fish_data_raw <- raw_data_same_no %>% filter(Fish_id == fish_id)
    
    if(nrow(fish_data_gam) == 0 || nrow(fish_data_raw) == 0) {
      return(NULL)
    }
    
    watershed <- fish_data_gam$Watershed
    
    # Extract time series values
    gam_values <- as.numeric(fish_data_gam[feature_cols])
    raw_values <- as.numeric(fish_data_raw[feature_cols])
    
    # Create data frame with feature importance
    fish_ts_df <- data.frame(
      time_point = time_points,
      sr_ratio_gam = gam_values,
      sr_ratio_raw = raw_values,
      feature_name = feature_cols
    )
    
    # Add feature importance values
    fish_ts_df$importance <- importance_lookup[fish_ts_df$feature_name]
    
    # Handle any missing importance values
    fish_ts_df$importance[is.na(fish_ts_df$importance)] <- 0
    
    # Create the plot (same style as PCA loadings)
    p <- ggplot(fish_ts_df, aes(x = time_point)) +
      # Raw data points (darker grey, smaller)
      geom_point(aes(y = sr_ratio_raw), color = "grey70", size = 1.0, alpha = 0.6) +
      # GAM smoothed line (thicker and darker)
      geom_line(aes(y = sr_ratio_gam), color = "grey40", alpha = 0.9, size = 1.0) +
      # GAM points colored by feature importance (same style as PCA)
      geom_point(aes(y = sr_ratio_gam, color = importance), size = 1.8, alpha = 1.0, stroke = 0) +
      # Plasma color scale - darker for higher importance (same as PCA style)
      scale_color_viridis_c(
        name = "Feature\nImportance",
        option = "plasma",
        begin = 0.9,
        end = 0.1,
        direction = -1,
        limits = importance_range,
        guide = if(show_legend) {
          guide_colorbar(
            barwidth = 8,
            barheight = 0.6,
            title.position = "top",
            title.hjust = 0.5,
            frame.colour = "grey70",
            frame.linewidth = 0.3
          )
        } else {
          "none"
        }
      ) +
      # Fixed y-axis limits (same as PCA)
      scale_x_continuous(
        breaks = pretty_breaks(n = 5),
        expand = expansion(mult = 0.01)
      ) +
      scale_y_continuous(
        limits = c(0.7065, 0.713),
        breaks = pretty_breaks(n = 4),
        expand = expansion(mult = 0.01),
        labels = label_number(accuracy = 0.001)
      ) +
      labs(
        title = paste0(fish_id, " • ", watershed),
        x = "Time Point",
        y = if(show_y_title) expression(paste(""^87, "Sr/", ""^86, "Sr")) else NULL
      ) +
      # Clean theme (same as PCA)
      theme_minimal(base_size = 10) +
      theme(
        plot.title = element_text(size = 11, face = "bold", hjust = 0.5, 
                                  color = "grey15", margin = margin(b = 8)),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        
        # Axes
        axis.title.y = element_text(size = 10, face = "bold", color = "grey20", 
                                    margin = margin(r = 6)),
        axis.title.x = element_text(size = 10, face = "bold", color = "grey20", 
                                    margin = margin(t = 6)),
        axis.text = element_text(size = 9, color = "grey30"),
        axis.line = element_line(color = "grey60", size = 0.4),
        axis.ticks = element_line(color = "grey60", size = 0.3),
        axis.ticks.length = unit(2, "pt"),
        
        # Grid
        panel.grid.major = element_line(color = "grey90", size = 0.25),
        panel.grid.minor = element_blank(),
        
        # Legend
        legend.position = if(show_legend) "bottom" else "none",
        legend.title = element_text(size = 9, face = "bold", color = "grey20"),
        legend.text = element_text(size = 8, color = "grey30"),
        legend.key = element_blank(),
        legend.margin = margin(t = 6),
        legend.box.margin = margin(t = 4),
        legend.justification = if(is_middle) "center" else "center",
        
        # Spacing
        plot.margin = margin(6, 8, 6, 8),
        panel.spacing = unit(4, "pt")
      )
    
    return(p)
  }
  
  # Create panels for available fish
  panels <- list()
  
  if(nrow(available_fish) >= 3) {
    # All three fish available
    panels$fish1 <- create_fish_importance_panel(target_fish_ids[1], show_legend = FALSE, show_y_title = TRUE)
    panels$fish2 <- create_fish_importance_panel(target_fish_ids[2], show_legend = TRUE, show_y_title = FALSE, is_middle = TRUE)
    panels$fish3 <- create_fish_importance_panel(target_fish_ids[3], show_legend = FALSE, show_y_title = FALSE)
    
    # Combine using patchwork (horizontal layout)
    combined_figure <- panels$fish1 | panels$fish2 | panels$fish3
    
  } else if(nrow(available_fish) >= 2) {
    # Two fish available
    panels$fish1 <- create_fish_importance_panel(available_fish$Fish_id[1], show_legend = FALSE, show_y_title = TRUE)
    panels$fish2 <- create_fish_importance_panel(available_fish$Fish_id[2], show_legend = TRUE, show_y_title = FALSE, is_middle = TRUE)
    
    combined_figure <- panels$fish1 | panels$fish2
    
  } else {
    # One fish available
    panels$fish1 <- create_fish_importance_panel(available_fish$Fish_id[1], show_legend = TRUE, show_y_title = TRUE)
    
    combined_figure <- panels$fish1
  }
  
  # Add title and subtitle
  combined_figure <- combined_figure +
    plot_annotation(
      title = "Random Forest Feature Importance on Individual Timeseries",
      subtitle = "Individuals with similar natal origin (0.7075-0.7080) • Darker colors = Higher importance",
      theme = theme_void() +
        theme(
          plot.title = element_text(size = 14, face = "bold", hjust = 0.5, 
                                    color = "grey10", margin = margin(t = 15, b = 8)),
          plot.subtitle = element_text(size = 11, hjust = 0.5, color = "grey40",
                                       margin = margin(b = 15)),
          plot.background = element_rect(fill = "white", color = NA),
          plot.margin = margin(15, 15, 15, 15)
        )
    )
  
  # Save combined figure
  combined_filename <- "Feature_Importance_Three_Panel_Comparison.pdf"
  combined_filepath <- file.path(feature_importance_ts_dir, combined_filename)
  
  ggsave(combined_filepath, combined_figure, 
         width = 12, height = 4, 
         device = cairo_pdf,
         dpi = 300,
         units = "in")
  
  # Also save as PNG
  combined_filename_png <- "Feature_Importance_Three_Panel_Comparison.png"
  combined_filepath_png <- file.path(feature_importance_ts_dir, combined_filename_png)
  
  ggsave(combined_filepath_png, combined_figure, 
         width = 12, height = 4, 
         dpi = 300,
         units = "in")
  
  print(combined_figure)
  cat("Saved feature importance comparison figure:", combined_filename, "\n")
  cat("Saved feature importance comparison figure:", combined_filename_png, "\n")
  
} else {
  cat("Warning: No target fish found in same NO dataset\n")
}

# =============================================================================
# SUMMARY
# =============================================================================
cat("\n", "="*80, "\n")
cat("FEATURE IMPORTANCE TIMESERIES ANALYSIS COMPLETE\n")
cat("="*80, "\n")
cat("Original Feature Importance Plots:\n")
cat("  - RF_GAM_Feature_Importance_Line.png\n")
cat("  - RF_GAM_Importance_Regions.png\n")
cat("\nNew Feature Importance Timeseries:\n")
cat("  - Feature_Importance_Three_Panel_Comparison.pdf\n")
cat("  - Feature_Importance_Three_Panel_Comparison.png\n")
cat("\nOutput Directories:\n")
cat("  - Original plots:", OUTPUT_DIR, "\n")
cat("  - Timeseries plots:", feature_importance_ts_dir, "\n")
cat("\nFish analyzed:\n")
if(exists("available_fish")) {
  for(i in 1:nrow(available_fish)) {
    cat("  -", available_fish$Fish_id[i], "(", available_fish$Watershed[i], ")\n")
  }
}
cat("="*80, "\n")