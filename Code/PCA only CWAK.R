# PCA Analysis - CWAK Only (No Middle/Upper Yukon)
# Removes Yukon individuals with "Middle" and "Upper" genetic assignments
# Keeps: All Nushagak, All Kuskokwim, and Lower Yukon only

library(tidyverse)
library(ggplot2)
library(scales)

# =============================================================================
# CONFIGURATION - Update these paths to match your system
# =============================================================================

# Path to GAM smoothed data
gam_data_path <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/data/LA_Data/Preprocessed_ts_matrices/NatalToMarine_Processed_GAM.csv"

# Path to metadata with genetic information
metadata_path <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/Data/Final/Metadata_and_QC.csv"

# Output directory
output_dir <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/Figures/PCA"

# Create output directory if needed
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# LOAD DATA
# =============================================================================

cat("Loading GAM smoothed data...\n")
gam_data <- read_csv(gam_data_path) %>%
  mutate(Watershed = as.factor(Watershed))

cat("Loading metadata with genetic information...\n")
metadata <- read_csv(metadata_path)

# =============================================================================
# CHECK FOR AND HANDLE DUPLICATES
# =============================================================================

cat("Checking for duplicate Fish_IDs...\n")

# Check for duplicates in GAM data
gam_duplicates <- gam_data %>%
  group_by(Fish_id) %>%
  filter(n() > 1) %>%
  ungroup()

if (nrow(gam_duplicates) > 0) {
  cat("WARNING: Found", nrow(gam_duplicates), "duplicate Fish_IDs in GAM data:\n")
  print(table(gam_duplicates$Fish_id))
  cat("Keeping only the first occurrence of each duplicate...\n")
  gam_data <- gam_data %>%
    distinct(Fish_id, .keep_all = TRUE)
}

# Check for duplicates in metadata
metadata_duplicates <- metadata %>%
  group_by(Fish_ID) %>%
  filter(n() > 1) %>%
  ungroup()

if (nrow(metadata_duplicates) > 0) {
  cat("WARNING: Found", nrow(metadata_duplicates), "duplicate Fish_IDs in metadata:\n")
  print(table(metadata_duplicates$Fish_ID))
  cat("Keeping only the first occurrence of each duplicate...\n")
  metadata <- metadata %>%
    distinct(Fish_ID, .keep_all = TRUE)
}

# =============================================================================
# ADD GENETIC INFORMATION TO GAM DATA
# =============================================================================

cat("Merging genetic information...\n")

# Add genetic assignments to GAM data
gam_data_genetics <- gam_data %>%
  left_join(
    metadata %>% select(Fish_ID, likely_gen, Lower_gen, Middle_gen, Upper_gen),
    by = c("Fish_id" = "Fish_ID"),
    relationship = "many-to-one"
  )

cat("Original dataset:", nrow(gam_data_genetics), "fish\n")
cat("Watershed distribution:\n")
print(table(gam_data_genetics$Watershed))

# =============================================================================
# FILTER TO CWAK ONLY (Remove Middle and Upper Yukon)
# =============================================================================

cat("\nFiltering to CWAK individuals only...\n")

gam_cwak <- gam_data_genetics %>%
  filter(
    # Keep all Nushagak and Kuskokwim
    Watershed %in% c("Nush", "Kusko") |
      # Keep only Lower Yukon (exclude Middle and Upper)
      (Watershed == "Yukon" & likely_gen == "Lower_gen")
  )

cat("CWAK dataset:", nrow(gam_cwak), "fish\n")
cat("CWAK Watershed distribution:\n")
print(table(gam_cwak$Watershed))

# =============================================================================
# VERIFY ALL YUKON SAMPLES HAVE LOWER GENETIC ASSIGNMENT
# =============================================================================

cat("\nVerifying Yukon genetic assignments...\n")

yukon_fish <- gam_cwak %>%
  filter(Watershed == "Yukon")

if (nrow(yukon_fish) > 0) {
  # Check if all Yukon fish have Lower_gen assignment
  non_lower_yukon <- yukon_fish %>%
    filter(likely_gen != "Lower_gen" | is.na(likely_gen))
  
  if (nrow(non_lower_yukon) > 0) {
    cat("WARNING: Found", nrow(non_lower_yukon), "Yukon fish without Lower_gen assignment:\n")
    print(non_lower_yukon %>% select(Fish_id, Watershed, likely_gen, Lower_gen, Middle_gen, Upper_gen))
    cat("\nRemoving these fish from CWAK dataset...\n")
    
    # Remove non-Lower Yukon fish
    gam_cwak <- gam_cwak %>%
      filter(!(Watershed == "Yukon" & (likely_gen != "Lower_gen" | is.na(likely_gen))))
  } else {
    cat("✓ All", nrow(yukon_fish), "Yukon fish have Lower_gen assignment\n")
  }
  
  # Show genetic probabilities for Yukon fish
  cat("\nYukon genetic assignment summary:\n")
  yukon_summary <- gam_cwak %>%
    filter(Watershed == "Yukon") %>%
    select(Fish_id, likely_gen, Lower_gen, Middle_gen, Upper_gen)
  
  cat("  Lower_gen range:", round(min(yukon_summary$Lower_gen, na.rm = TRUE), 3), 
      "-", round(max(yukon_summary$Lower_gen, na.rm = TRUE), 3), "\n")
  if (any(!is.na(yukon_summary$Middle_gen))) {
    cat("  Middle_gen range:", round(min(yukon_summary$Middle_gen, na.rm = TRUE), 3), 
        "-", round(max(yukon_summary$Middle_gen, na.rm = TRUE), 3), "\n")
  }
  if (any(!is.na(yukon_summary$Upper_gen))) {
    cat("  Upper_gen range:", round(min(yukon_summary$Upper_gen, na.rm = TRUE), 3), 
        "-", round(max(yukon_summary$Upper_gen, na.rm = TRUE), 3), "\n")
  }
}

cat("\nFinal CWAK dataset:", nrow(gam_cwak), "fish\n")
cat("  - Nushagak:", sum(gam_cwak$Watershed == "Nush"), "\n")
cat("  - Kuskokwim:", sum(gam_cwak$Watershed == "Kusko"), "\n")
cat("  - Yukon (Lower only):", sum(gam_cwak$Watershed == "Yukon"), "\n")

# =============================================================================
# PREPARE DATA FOR PCA
# =============================================================================

# Identify feature columns (time series data)
feature_cols <- grep("^X", names(gam_cwak), value = TRUE)

cat("\nPreparing PCA with", length(feature_cols), "features...\n")

# Extract feature matrix
X_cwak <- gam_cwak[, feature_cols]

# =============================================================================
# PERFORM PCA
# =============================================================================

cat("Performing PCA...\n")
pca_result <- prcomp(X_cwak, scale. = TRUE)

# Calculate variance explained
var_explained <- (pca_result$sdev^2 / sum(pca_result$sdev^2))[1:2]

cat("PC1 explains:", round(var_explained[1] * 100, 1), "%\n")
cat("PC2 explains:", round(var_explained[2] * 100, 1), "%\n")

# Get PC scores
pc_scores <- data.frame(
  PC1 = pca_result$x[, 1],
  PC2 = pca_result$x[, 2],
  Watershed = gam_cwak$Watershed,
  Fish_id = gam_cwak$Fish_id
)

# =============================================================================
# CREATE PCA PLOT
# =============================================================================

cat("Creating PCA plot...\n")

# Define watershed colors (matching your original code)
watershed_colors <- c(
  "Nush" = "#d62728",      # Red
  "Kusko" = "#2ca02c",     # Green  
  "Yukon" = "#1f77b4"      # Blue
)

pca_plot <- ggplot(pc_scores, aes(x = PC1, y = PC2, color = Watershed)) +
  # Add points first
  geom_point(size = 2.5, alpha = 0.8, stroke = 0.2) +
  # Use the same watershed colors
  scale_color_manual(values = watershed_colors, name = "Watershed") +
  # Clean axis formatting
  scale_x_continuous(
    breaks = pretty_breaks(n = 6),
    expand = expansion(mult = 0.05)
  ) +
  scale_y_continuous(
    breaks = pretty_breaks(n = 6),
    expand = expansion(mult = 0.05)
  ) +
  # Labels with variance explained (NO SUBTITLE)
  labs(
    title = "Principal Component Analysis",
    x = paste0("PC1 (", round(var_explained[1] * 100, 1), "%)"),
    y = paste0("PC2 (", round(var_explained[2] * 100, 1), "%)")
  ) +
  # Clean theme
  theme_minimal(base_size = 12) +
  theme(
    # Plot elements
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5, 
                              color = "grey15", margin = margin(b = 5)),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    
    # Axes
    axis.title = element_text(size = 13, face = "bold", color = "grey20"),
    axis.title.x = element_text(margin = margin(t = 8)),
    axis.title.y = element_text(margin = margin(r = 8)),
    axis.text = element_text(size = 11, color = "grey30"),
    axis.line = element_line(color = "grey60", linewidth = 0.4),
    axis.ticks = element_line(color = "grey60", linewidth = 0.3),
    axis.ticks.length = unit(3, "pt"),
    
    # Grid
    panel.grid.major = element_line(color = "grey90", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    
    # Legend
    legend.position = "bottom",
    legend.title = element_text(size = 12, face = "bold", color = "grey20"),
    legend.text = element_text(size = 11, color = "grey30"),
    legend.key = element_blank(),
    legend.margin = margin(t = 10),
    legend.box.margin = margin(t = 5),
    
    # Margins
    plot.margin = margin(15, 15, 15, 15)
  )

# Add ellipses with colored borders (matching point colors)
tryCatch({
  pca_plot <- pca_plot + 
    stat_ellipse(aes(fill = Watershed, color = Watershed), 
                 alpha = 0.15, 
                 level = 0.95, 
                 geom = "polygon", 
                 show.legend = FALSE,
                 linewidth = 1) +
    scale_fill_manual(values = watershed_colors, guide = "none")
  cat("  Added 95% confidence ellipses with colored borders successfully\n")
}, error = function(e) {
  cat("  Warning: Could not add ellipses -", e$message, "\n")
  cat("  Plot will show points only\n")
})

# =============================================================================
# SAVE PLOT
# =============================================================================

output_file <- file.path(output_dir, "PCA_CWAK_only.png")

ggsave(
  filename = output_file,
  plot = pca_plot,
  width = 10,
  height = 8,
  dpi = 300,
  bg = "white"
)

cat("\n", paste(rep("=", 70), collapse = ""), "\n")
cat("PCA CWAK-ONLY ANALYSIS COMPLETE\n")
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("Total fish in CWAK dataset:", nrow(gam_cwak), "\n")
cat("  - Nushagak:", sum(gam_cwak$Watershed == "Nush"), "\n")
cat("  - Kuskokwim:", sum(gam_cwak$Watershed == "Kusko"), "\n")
cat("  - Yukon (Lower only):", sum(gam_cwak$Watershed == "Yukon"), "\n")
cat("\nRemoved from original dataset:", nrow(gam_data_genetics) - nrow(gam_cwak), "fish\n")
cat("  (Middle and Upper Yukon genetic assignments)\n")
cat("\nPlot saved to:", output_file, "\n")
cat(paste(rep("=", 70), collapse = ""), "\n")