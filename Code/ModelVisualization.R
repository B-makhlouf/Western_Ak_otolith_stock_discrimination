# ModelVisualization.R
# Professional PCA visualization for GAM Sr87/86 data

# =============================================================================
# SETUP
# =============================================================================
library(tidyverse)
library(ggplot2)

# Paths and colors
data_path <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/data/LA_Data/Preprocessed_ts_matrices/NatalToMarine_Processed_GAM.csv"
output_dir <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/Figures"
watershed_colors <- c("Kusko" = "firebrick", "Nush" = "darkgreen", "Yukon" = "dodgerblue")

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# LOAD AND PREPARE DATA
# =============================================================================
gam_data <- read.csv(data_path) %>%
  mutate(Watershed = as.factor(Watershed)) %>%
  filter(Natal_Iso <= 0.715)

metadata_cols <- c("Fish_id", "Watershed", "Natal_Iso", "Year", "Natal_Start", 
                   "Marine_Start", "Marine_End", "Original_Data_Points", 
                   "Interpolated_Points", "Micron_Size")
feature_cols <- grep("^X", names(gam_data), value = TRUE)

cat("Loaded", nrow(gam_data), "samples with", length(feature_cols), "features\n")

# =============================================================================
# PCA ANALYSIS
# =============================================================================
pca_result <- prcomp(gam_data[, feature_cols], scale. = TRUE)
pca_scores <- as.data.frame(pca_result$x[, 1:3])
pca_data <- bind_cols(pca_scores, gam_data[, metadata_cols])

var_explained <- (pca_result$sdev^2 / sum(pca_result$sdev^2))[1:2]
cat("PC1:", round(var_explained[1] * 100, 1), "% | PC2:", round(var_explained[2] * 100, 1), "%\n")

# =============================================================================
# CREATE PROFESSIONAL PCA PLOT
# =============================================================================
pca_plot <- ggplot(pca_data, aes(x = PC1, y = PC2, color = Watershed, fill = Watershed)) +
  # Confidence ellipses
  stat_ellipse(level = 0.95, type = "norm", geom = "polygon", size = 0, alpha = 0.15) +
  stat_ellipse(level = 0.95, type = "norm", size = 1.2, alpha = 0.8, geom = "path", show.legend = FALSE) +
  
  # Data points
  geom_point(alpha = 0.6, size = 1.5, stroke = 0) +
  
  # Colors
  scale_color_manual(values = watershed_colors) +
  scale_fill_manual(values = watershed_colors) +
  
  # Reference lines
  geom_hline(yintercept = 0, color = "grey85", size = 0.3, alpha = 0.7) +
  geom_vline(xintercept = 0, color = "grey85", size = 0.3, alpha = 0.7) +
  
  # Labels
  labs(
    title = "Principal Component Analysis",
    subtitle = "GAM-Smoothed Sr 87/86 Profiles by Watershed",
    x = sprintf("PC1 (%s%%)", round(var_explained[1] * 100, 1)),
    y = sprintf("PC2 (%s%%)", round(var_explained[2] * 100, 1)),
    color = "Watershed",
    caption = "95% confidence ellipses"
  ) +
  
  # Theme
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5, margin = margin(b = 5), color = "grey15"),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "grey40", margin = margin(b = 15)),
    plot.caption = element_text(size = 9, color = "grey50", hjust = 0, margin = margin(t = 10)),
    axis.title = element_text(size = 12, face = "bold", color = "grey20"),
    axis.text = element_text(size = 10, color = "grey30"),
    axis.line = element_line(color = "grey80", size = 0.5),
    panel.grid.major = element_line(color = "grey90", size = 0.3),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "bottom",
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    legend.key = element_blank(),
    legend.margin = margin(t = 15),
    plot.margin = margin(20, 25, 20, 25)
  ) +
  
  guides(color = guide_legend(override.aes = list(alpha = 1, size = 3, stroke = 0), ncol = 3))

print(pca_plot)

ggsave(file.path(output_dir, "pca_professional.png"), pca_plot, width = 12, height = 10, dpi = 300)

cat("Analysis complete!\n")




