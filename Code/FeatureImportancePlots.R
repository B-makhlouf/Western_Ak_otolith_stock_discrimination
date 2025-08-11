# Clean Random Forest GAM Feature Importance Visualization
# Creates only the two key plots: Line plot and Regions plot

library(tidyverse)
library(ggplot2)
library(viridis)

# Paths
IMPORTANCE_DIR <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/Output/FeatureImportance"
OUTPUT_DIR <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/Figures/FeatureImportance"
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

# Load data
gam_files <- list.files(IMPORTANCE_DIR, pattern = "GAM_RF.*feature_importance.csv", full.names = TRUE)
importance_df <- read.csv(gam_files[1])

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

# Save plots
ggsave(file.path(OUTPUT_DIR, "RF_GAM_Feature_Importance_Line.png"), p1, width = 12, height = 8, dpi = 300)
ggsave(file.path(OUTPUT_DIR, "RF_GAM_Importance_Regions.png"), p2, width = 14, height = 8, dpi = 300)

cat("✓ Two plots saved to:", OUTPUT_DIR, "\n")