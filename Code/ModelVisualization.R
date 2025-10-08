# PCA_Loadings_Individual_TimeSeries.R
# Creates individual three-panel figures for each selected fish
# Top panel: time series colored by PC1 loadings
# Middle panel: time series colored by PC2 loadings  
# Bottom panel: time series colored by PC3 loadings
# PLUS: Creates summary PCA plots showing all samples
# PLUS: Creates 2D PCA plots showing PC1 vs PC2 and PC2 vs PC3

# =============================================================================
# SETUP
# =============================================================================
library(tidyverse)
library(ggplot2)
library(viridis)
library(cowplot)
library(patchwork)
library(scales)

# Use system fonts - no need for extrafont package

# Set seed for reproducible random sampling
set.seed(42)

# =============================================================================
# CONFIGURABLE PARAMETERS - EASY TO MODIFY
# =============================================================================

# SAME NO RANGE CONFIGURATION - Change these three values to experiment:
same_no_range <- list(
  min = 0.7080,
  max = 0.7085,
  name = "SAME_NO_7080_7085"  # This will be used in filenames and descriptions
)

# Alternative ranges you can easily switch to (uncomment one to use):
# same_no_range <- list(min = 0.7070, max = 0.7085, name = "SAME_NO_7070_7085")
# same_no_range <- list(min = 0.7072, max = 0.7078, name = "SAME_NO_7072_7078")
# same_no_range <- list(min = 0.7074, max = 0.7082, name = "SAME_NO_7074_7082")
# same_no_range <- list(min = 0.7076, max = 0.7079, name = "SAME_NO_7076_7079")

# Paths and colors
gam_data_path <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/data/LA_Data/Preprocessed_ts_matrices/NatalToMarine_Processed_GAM.csv"
raw_data_path <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/data/LA_Data/Preprocessed_ts_matrices/NatalToMarine_Processed_RAW.csv"
output_dir <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/Figures/PCA/TS_PCA_Loadings"
same_no_output_dir <- paste0("/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/Figures/PCA/", same_no_range$name, "_ts_loadings")
additional_2d_dir <- paste0("/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/Figures/PCA/", same_no_range$name, "_2d_plots")
watershed_colors <- c("Kusko" = "firebrick", "Nush" = "darkgreen", "Yukon" = "dodgerblue")

# Create output directories
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(same_no_output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(additional_2d_dir, recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# LOAD AND PREPARE DATA
# =============================================================================
gam_data_full <- read.csv(gam_data_path) %>%
  mutate(Watershed = as.factor(Watershed))

# Load raw (unsmoothed) data
raw_data_full <- read.csv(raw_data_path) %>%
  mutate(Watershed = as.factor(Watershed))

# Create filtered versions
gam_data_filtered <- gam_data_full %>%
  filter(Natal_Iso <= 0.715)

raw_data_filtered <- raw_data_full %>%
  filter(Natal_Iso <= 0.715)

# Create same natal origin subset using configurable range
gam_data_same_no <- gam_data_full %>%
  filter(Natal_Iso >= same_no_range$min & Natal_Iso <= same_no_range$max)

raw_data_same_no <- raw_data_full %>%
  filter(Natal_Iso >= same_no_range$min & Natal_Iso <= same_no_range$max)

metadata_cols <- c("Fish_id", "Watershed", "Natal_Iso", "Year", "Natal_Start", 
                   "Marine_Start", "Marine_End", "Original_Data_Points", 
                   "Interpolated_Points", "Micron_Size")
feature_cols <- grep("^X", names(gam_data_full), value = TRUE)

# Extract time points from feature column names (assuming X1, X2, X3, etc.)
time_points <- as.numeric(gsub("^X", "", feature_cols))

cat("GAM TOTAL dataset:", nrow(gam_data_full), "samples with", length(feature_cols), "time points\n")
cat("GAM OVERLAP dataset:", nrow(gam_data_filtered), "samples with", length(feature_cols), "time points\n")
cat("GAM SAME NO dataset (", same_no_range$min, "-", same_no_range$max, "):", nrow(gam_data_same_no), "samples with", length(feature_cols), "time points\n")
cat("RAW TOTAL dataset:", nrow(raw_data_full), "samples\n")
cat("RAW OVERLAP dataset:", nrow(raw_data_filtered), "samples\n")
cat("RAW SAME NO dataset:", nrow(raw_data_same_no), "samples\n")

# =============================================================================
# FUNCTION TO CREATE PCA SUMMARY PLOTS
# =============================================================================
create_pca_summary_plot <- function(gam_data, dataset_name, title_suffix = "") {
  
  # Run PCA on GAM-smoothed data
  pca_result <- prcomp(gam_data[, feature_cols], scale. = TRUE)
  var_explained <- (pca_result$sdev^2 / sum(pca_result$sdev^2))[1:2]
  
  cat("PCA Summary for", dataset_name, ":\n")
  cat("  PC1:", round(var_explained[1] * 100, 1), "%\n")
  cat("  PC2:", round(var_explained[2] * 100, 1), "%\n")
  
  # Get PC scores for plotting
  pc_scores <- data.frame(
    PC1 = pca_result$x[, 1],
    PC2 = pca_result$x[, 2],
    Watershed = gam_data$Watershed,
    Fish_id = gam_data$Fish_id
  )
  
  cat("  PC1 range:", round(range(pc_scores$PC1), 2), "\n")
  cat("  PC2 range:", round(range(pc_scores$PC2), 2), "\n")
  cat("  Samples per watershed:\n")
  print(table(pc_scores$Watershed))
  
  # Create the PCA plot
  pca_plot <- ggplot(pc_scores, aes(x = PC1, y = PC2, color = Watershed)) +
    # Add points first
    geom_point(size = 2.5, alpha = 0.8, stroke = 0.2) +
    # Use the same watershed colors as defined in your script
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
    # Labels with variance explained
    labs(
      title = paste0("Principal Component Analysis", title_suffix),
      subtitle = paste0("GAM-smoothed ", nrow(gam_data), " fish by Watershed"),
      x = paste0("PC1 (", round(var_explained[1] * 100, 1), "%)"),
      y = paste0("PC2 (", round(var_explained[2] * 100, 1), "%)")
    ) +
    # Clean theme
    theme_minimal(base_size = 12) +
    theme(
      # Plot elements
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5, 
                                color = "grey15", margin = margin(b = 5)),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "grey40",
                                   margin = margin(b = 15)),
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
  
  # Try to add ellipses if possible
  tryCatch({
    pca_plot <- pca_plot + 
      stat_ellipse(aes(fill = Watershed), alpha = 0.15, level = 0.95, 
                   geom = "polygon", show.legend = FALSE) +
      scale_fill_manual(values = watershed_colors, guide = "none")
    cat("  Added confidence ellipses successfully\n")
  }, error = function(e) {
    cat("  Warning: Could not add ellipses -", e$message, "\n")
    cat("  Plot will show points only\n")
  })
  
  return(list(
    plot = pca_plot,
    pc_scores = pc_scores,
    var_explained = var_explained,
    pca_result = pca_result
  ))
}

# =============================================================================
# FUNCTION TO CREATE INDIVIDUAL PCA LOADINGS PLOTS (NOW WITH PC1, PC2, PC3)
# =============================================================================
create_individual_pca_plots <- function(gam_data, raw_data, dataset_name, title_suffix, output_directory = output_dir, use_all_samples = FALSE) {
  
  # Run PCA on GAM-smoothed data
  pca_result <- prcomp(gam_data[, feature_cols], scale. = TRUE)
  var_explained <- (pca_result$sdev^2 / sum(pca_result$sdev^2))[1:3]  # Now get first 3 PCs
  
  cat("\n", dataset_name, "PCA Analysis:\n")
  cat("PC1:", round(var_explained[1] * 100, 1), "% | PC2:", round(var_explained[2] * 100, 1), "% | PC3:", round(var_explained[3] * 100, 1), "%\n")
  
  # Get loadings for PC1, PC2, and PC3
  loadings_df <- data.frame(
    time_point = time_points,
    PC1_loading = pca_result$rotation[, 1],
    PC2_loading = pca_result$rotation[, 2],
    PC3_loading = pca_result$rotation[, 3]  # Add PC3
  ) %>%
    mutate(
      PC1_abs = abs(PC1_loading),
      PC2_abs = abs(PC2_loading),
      PC3_abs = abs(PC3_loading)  # Add PC3 absolute values
    )
  
  # Randomly select 20 samples from each watershed (unless using all samples)
  if(use_all_samples) {
    selected_samples <- gam_data
    cat("Using all", nrow(selected_samples), "samples\n")
  } else {
    selected_samples <- gam_data %>%
      group_by(Watershed) %>%
      group_modify(~ {
        n_samples <- min(20, nrow(.x))
        slice_sample(.x, n = n_samples)
      }) %>%
      ungroup()
    
    cat("Selected samples per watershed:\n")
    print(table(selected_samples$Watershed))
  }
  
  # Create individual plots for each selected sample
  for(i in 1:nrow(selected_samples)) {
    fish_data_gam <- selected_samples[i, ]
    fish_id <- fish_data_gam$Fish_id
    watershed <- fish_data_gam$Watershed
    
    # Find corresponding raw data
    fish_data_raw <- raw_data[raw_data$Fish_id == fish_id, ]
    
    if(nrow(fish_data_raw) == 0) {
      cat("Warning: No raw data found for Fish_id", fish_id, "- skipping\n")
      next
    }
    
    # Extract time series values
    gam_values <- as.numeric(fish_data_gam[feature_cols])
    raw_values <- as.numeric(fish_data_raw[feature_cols])
    
    # Create data frame for this fish
    fish_ts_df <- data.frame(
      time_point = time_points,
      sr_ratio_gam = gam_values,
      sr_ratio_raw = raw_values,
      PC1_loading = loadings_df$PC1_loading,
      PC2_loading = loadings_df$PC2_loading,
      PC3_loading = loadings_df$PC3_loading,  # Add PC3
      PC1_abs = loadings_df$PC1_abs,
      PC2_abs = loadings_df$PC2_abs,
      PC3_abs = loadings_df$PC3_abs  # Add PC3 absolute values
    )
    
    # Create PC1 panel
    pc1_panel <- ggplot(fish_ts_df, aes(x = time_point)) +
      # Raw data points (darker grey, smaller)
      geom_point(aes(y = sr_ratio_raw), color = "grey70", size = 1.2, alpha = 0.6) +
      # GAM smoothed line (thicker and darker)
      geom_line(aes(y = sr_ratio_gam), color = "grey40", alpha = 0.9, size = 1.2) +
      # GAM points colored by loadings (full opacity, darker = higher values)
      geom_point(aes(y = sr_ratio_gam, color = PC1_abs), size = 2.2, alpha = 1.0, stroke = 0) +
      # Reversed color scale - darker colors for higher values
      scale_color_viridis_c(
        name = "|PC1|",
        option = "plasma",
        begin = 0.9,
        end = 0.1,
        direction = -1,
        guide = guide_colorbar(
          barwidth = 10,
          barheight = 0.8,
          title.position = "top",
          title.hjust = 0.5,
          frame.colour = "grey70",
          frame.linewidth = 0.3
        )
      ) +
      # Clean axis formatting with fixed y-axis limits
      scale_x_continuous(
        breaks = pretty_breaks(n = 6),
        expand = expansion(mult = 0.01)
      ) +
      scale_y_continuous(
        limits = c(0.7065, 0.713),
        breaks = pretty_breaks(n = 5),
        expand = expansion(mult = 0.01),
        labels = label_number(accuracy = 0.001)
      ) +
      labs(
        title = paste0("PC1 (", round(var_explained[1] * 100, 1), "%)"),
        x = NULL,
        y = expression(paste(""^87, "Sr/", ""^86, "Sr"))
      ) +
      # Modern minimal theme with larger plot area
      theme_minimal(base_size = 11) +
      theme(
        # Plot elements
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5, 
                                  color = "grey15", margin = margin(b = 8)),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        
        # Axes
        axis.title.y = element_text(size = 12, face = "bold", color = "grey20", 
                                    margin = margin(r = 6)),
        axis.text = element_text(size = 11, color = "grey30"),
        axis.text.x = element_blank(),
        axis.line = element_line(color = "grey60", size = 0.4),
        axis.ticks = element_line(color = "grey60", size = 0.3),
        axis.ticks.length = unit(3, "pt"),
        
        # Grid - lighter and thinner
        panel.grid.major = element_line(color = "grey90", size = 0.25),
        panel.grid.minor = element_blank(),
        
        # Legend
        legend.position = "bottom",
        legend.title = element_text(size = 11, face = "bold", color = "grey20"),
        legend.text = element_text(size = 10, color = "grey30"),
        legend.key = element_blank(),
        legend.margin = margin(t = 8),
        legend.box.margin = margin(t = 5),
        
        # Reduced margins for larger plot area
        plot.margin = margin(8, 12, 4, 12),
        panel.spacing = unit(8, "pt")
      )
    
    # Create PC2 panel
    pc2_panel <- ggplot(fish_ts_df, aes(x = time_point)) +
      # Raw data points (darker grey, smaller)
      geom_point(aes(y = sr_ratio_raw), color = "grey70", size = 1.2, alpha = 0.6) +
      # GAM smoothed line (thicker and darker)
      geom_line(aes(y = sr_ratio_gam), color = "grey40", alpha = 0.9, size = 1.2) +
      # GAM points colored by loadings (full opacity, darker = higher values)
      geom_point(aes(y = sr_ratio_gam, color = PC2_abs), size = 2.2, alpha = 1.0, stroke = 0) +
      # Reversed color scale - darker colors for higher values
      scale_color_viridis_c(
        name = "|PC2|",
        option = "plasma",
        begin = 0.9,
        end = 0.1,
        direction = -1,
        guide = guide_colorbar(
          barwidth = 10,
          barheight = 0.8,
          title.position = "top",
          title.hjust = 0.5,
          frame.colour = "grey70",
          frame.linewidth = 0.3
        )
      ) +
      # Clean axis formatting with fixed y-axis limits
      scale_x_continuous(
        breaks = pretty_breaks(n = 6),
        expand = expansion(mult = 0.01)
      ) +
      scale_y_continuous(
        limits = c(0.7065, 0.713),
        breaks = pretty_breaks(n = 5),
        expand = expansion(mult = 0.01),
        labels = label_number(accuracy = 0.001)
      ) +
      labs(
        title = paste0("PC2 (", round(var_explained[2] * 100, 1), "%)"),
        x = NULL,  # Remove x-axis label for middle panel
        y = expression(paste(""^87, "Sr/", ""^86, "Sr"))
      ) +
      # Modern minimal theme with larger plot area
      theme_minimal(base_size = 11) +
      theme(
        # Plot elements
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5, 
                                  color = "grey15", margin = margin(b = 8)),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        
        # Axes
        axis.title.y = element_text(size = 12, face = "bold", color = "grey20", 
                                    margin = margin(r = 6)),
        axis.text = element_text(size = 11, color = "grey30"),
        axis.text.x = element_blank(),  # Hide x-axis text for middle panel
        axis.line = element_line(color = "grey60", size = 0.4),
        axis.ticks = element_line(color = "grey60", size = 0.3),
        axis.ticks.length = unit(3, "pt"),
        
        # Grid - lighter and thinner
        panel.grid.major = element_line(color = "grey90", size = 0.25),
        panel.grid.minor = element_blank(),
        
        # Legend
        legend.position = "bottom",
        legend.title = element_text(size = 11, face = "bold", color = "grey20"),
        legend.text = element_text(size = 10, color = "grey30"),
        legend.key = element_blank(),
        legend.margin = margin(t = 8),
        legend.box.margin = margin(t = 5),
        
        # Reduced margins for larger plot area
        plot.margin = margin(4, 12, 4, 12),
        panel.spacing = unit(8, "pt")
      )
    
    # Create PC3 panel
    pc3_panel <- ggplot(fish_ts_df, aes(x = time_point)) +
      # Raw data points (darker grey, smaller)
      geom_point(aes(y = sr_ratio_raw), color = "grey70", size = 1.2, alpha = 0.6) +
      # GAM smoothed line (thicker and darker)
      geom_line(aes(y = sr_ratio_gam), color = "grey40", alpha = 0.9, size = 1.2) +
      # GAM points colored by loadings (full opacity, darker = higher values)
      geom_point(aes(y = sr_ratio_gam, color = PC3_abs), size = 2.2, alpha = 1.0, stroke = 0) +
      # Reversed color scale - darker colors for higher values
      scale_color_viridis_c(
        name = "|PC3|",
        option = "plasma",
        begin = 0.9,
        end = 0.1,
        direction = -1,
        guide = guide_colorbar(
          barwidth = 10,
          barheight = 0.8,
          title.position = "top",
          title.hjust = 0.5,
          frame.colour = "grey70",
          frame.linewidth = 0.3
        )
      ) +
      # Clean axis formatting with fixed y-axis limits
      scale_x_continuous(
        breaks = pretty_breaks(n = 6),
        expand = expansion(mult = 0.01)
      ) +
      scale_y_continuous(
        limits = c(0.7065, 0.713),
        breaks = pretty_breaks(n = 5),
        expand = expansion(mult = 0.01),
        labels = label_number(accuracy = 0.001)
      ) +
      labs(
        title = paste0("PC3 (", round(var_explained[3] * 100, 1), "%)"),
        x = "Time Point",
        y = expression(paste(""^87, "Sr/", ""^86, "Sr"))
      ) +
      # Modern minimal theme with larger plot area
      theme_minimal(base_size = 11) +
      theme(
        # Plot elements
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5, 
                                  color = "grey15", margin = margin(b = 8)),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        
        # Axes
        axis.title = element_text(size = 12, face = "bold", color = "grey20"),
        axis.title.x = element_text(margin = margin(t = 6)),
        axis.title.y = element_text(margin = margin(r = 6)),
        axis.text = element_text(size = 11, color = "grey30"),
        axis.line = element_line(color = "grey60", size = 0.4),
        axis.ticks = element_line(color = "grey60", size = 0.3),
        axis.ticks.length = unit(3, "pt"),
        
        # Grid - lighter and thinner
        panel.grid.major = element_line(color = "grey90", size = 0.25),
        panel.grid.minor = element_blank(),
        
        # Legend
        legend.position = "bottom",
        legend.title = element_text(size = 11, face = "bold", color = "grey20"),
        legend.text = element_text(size = 10, color = "grey30"),
        legend.key = element_blank(),
        legend.margin = margin(t = 8),
        legend.box.margin = margin(t = 5),
        
        # Reduced margins for larger plot area
        plot.margin = margin(4, 12, 8, 12),
        panel.spacing = unit(8, "pt")
      )
    
    # Combine panels using patchwork with minimal layout (NOW THREE PANELS IN COLUMN)
    combined_plot <- pc1_panel / pc2_panel / pc3_panel +
      plot_layout(heights = c(1, 1, 1)) +
      plot_annotation(
        title = paste0(fish_id, " • ", watershed),
        theme = theme_void() +
          theme(
            plot.title = element_text(size = 18, face = "bold", hjust = 0.5, 
                                      color = "grey10", margin = margin(t = 12, b = 15)),
            plot.background = element_rect(fill = "white", color = NA),
            plot.margin = margin(10, 10, 10, 10)
          )
      )
    
    # Save individual plot as PDF with high quality (adjusted height for 3 panels)
    filename <- paste0(dataset_name, "_", fish_id, "_", watershed, "_PCA_Loadings.pdf")
    filepath <- file.path(output_directory, filename)
    
    # Use cairo_pdf for better quality and font embedding (increased height for 3 panels)
    ggsave(filepath, combined_plot, 
           width = 8.5, height = 14,  # Increased height from 11 to 14 for 3 panels
           device = cairo_pdf,
           dpi = 300,
           units = "in")
    
    cat("Saved:", filename, "\n")
  }
  
  cat("Created", nrow(selected_samples), "individual PCA loading plots for", dataset_name, "dataset\n")
  
  return(list(
    selected_samples = selected_samples,
    loadings_data = loadings_df,
    var_explained = var_explained,
    pca_result = pca_result
  ))
}

# =============================================================================
# ADDITIONAL 2D PCA PLOTS FUNCTION (PC1 vs PC2 and PC2 vs PC3 in COLUMN layout)
# =============================================================================
# Modified create_additional_pca_plots function
# Changes: Removed main title, increased all font sizes significantly

# Modified create_additional_pca_plots function
# Changes: Removed main title, increased all font sizes significantly

# Modified create_additional_pca_plots function
# Changes: Removed main title, increased all font sizes significantly

create_additional_pca_plots <- function(gam_data, dataset_name, output_directory) {
  
  # Run PCA on GAM-smoothed data
  pca_result <- prcomp(gam_data[, feature_cols], scale. = TRUE)
  var_explained <- (pca_result$sdev^2 / sum(pca_result$sdev^2))[1:3]
  
  cat("\nAdditional 2D PCA Analysis for", dataset_name, ":\n")
  cat("PC1:", round(var_explained[1] * 100, 2), "%\n")
  cat("PC2:", round(var_explained[2] * 100, 2), "%\n") 
  cat("PC3:", round(var_explained[3] * 100, 2), "%\n")
  cat("Total variance explained (PC1-3):", round(sum(var_explained) * 100, 1), "%\n")
  
  # Get PC scores for plotting
  pc_scores <- data.frame(
    PC1 = pca_result$x[, 1],
    PC2 = pca_result$x[, 2],
    PC3 = pca_result$x[, 3],
    Watershed = gam_data$Watershed,
    Fish_id = gam_data$Fish_id,
    Natal_Iso = gam_data$Natal_Iso
  )
  
  cat("Samples per watershed:\n")
  print(table(pc_scores$Watershed))
  
  # Define colors for watersheds (same as existing plots)
  colors <- c("Kusko" = "firebrick", "Nush" = "darkgreen", "Yukon" = "dodgerblue")
  
  # =============================================================================
  # PC1 vs PC2 PLOT - INCREASED FONT SIZES, NO LEGEND (for combined figure)
  # =============================================================================
  
  pc1_pc2_plot <- ggplot(pc_scores, aes(x = PC1, y = PC2, color = Watershed)) +
    # Add points with transparency
    geom_point(size = 4, alpha = 0.7, stroke = 0.3) +  # Increased from 2.5 to 4
    # Use the same watershed colors
    scale_color_manual(values = colors, name = "Watershed") +
    # Clean axis formatting
    scale_x_continuous(
      breaks = pretty_breaks(n = 6),
      expand = expansion(mult = 0.05)
    ) +
    scale_y_continuous(
      breaks = pretty_breaks(n = 6),
      expand = expansion(mult = 0.05)
    ) +
    # Labels with variance explained - NO TITLE
    labs(
      title = NULL,  # Removed title
      x = paste0("PC1 (", round(var_explained[1] * 100, 2), "% variance)"),
      y = paste0("PC2 (", round(var_explained[2] * 100, 2), "% variance)")
    ) +
    # Clean theme with MUCH LARGER fonts
    theme_minimal(base_size = 20) +  # Increased from 12 to 20
    theme(
      # Plot elements
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      
      # Axes - INCREASED SIZES
      axis.title = element_text(size = 22, face = "bold", color = "grey20"),  # Increased from 13 to 22
      axis.title.x = element_text(margin = margin(t = 12)),  # Increased margin
      axis.title.y = element_text(margin = margin(r = 12)),  # Increased margin
      axis.text = element_text(size = 18, color = "grey30"),  # Increased from 11 to 18
      axis.line = element_line(color = "grey60", linewidth = 0.6),  # Increased from 0.4
      axis.ticks = element_line(color = "grey60", linewidth = 0.5),  # Increased from 0.3
      axis.ticks.length = unit(5, "pt"),  # Increased from 3
      
      # Grid
      panel.grid.major = element_line(color = "grey90", linewidth = 0.4),  # Increased from 0.3
      panel.grid.minor = element_blank(),
      
      # Legend - REMOVED for combined figure
      legend.position = "none",
      
      # Margins
      plot.margin = margin(20, 20, 20, 20)  # Increased from 15
    )
  
  # Try to add ellipses if possible
  tryCatch({
    pc1_pc2_plot <- pc1_pc2_plot + 
      stat_ellipse(aes(fill = Watershed), alpha = 0.15, level = 0.95, 
                   geom = "polygon", show.legend = FALSE, linewidth = 1) +  # Increased linewidth
      scale_fill_manual(values = colors, guide = "none")
    cat("  Added confidence ellipses to PC1 vs PC2 plot\n")
  }, error = function(e) {
    cat("  Warning: Could not add ellipses to PC1 vs PC2 plot\n")
  })
  
  # Save PC1 vs PC2 plot
  pc1_pc2_filename <- paste0(dataset_name, "_PC1_vs_PC2.pdf")
  pc1_pc2_filepath <- file.path(output_directory, pc1_pc2_filename)
  ggsave(pc1_pc2_filepath, pc1_pc2_plot, 
         width = 10, height = 8, 
         device = cairo_pdf,
         dpi = 300,
         units = "in")
  cat("Saved PC1 vs PC2 plot:", pc1_pc2_filename, "\n")
  
  # =============================================================================
  # PC2 vs PC3 PLOT - INCREASED FONT SIZES, NO LEGEND (for combined figure)
  # =============================================================================
  
  pc2_pc3_plot <- ggplot(pc_scores, aes(x = PC2, y = PC3, color = Watershed)) +
    # Add points with transparency
    geom_point(size = 4, alpha = 0.7, stroke = 0.3) +  # Increased from 2.5 to 4
    # Use the same watershed colors
    scale_color_manual(values = colors, name = "Watershed") +
    # Clean axis formatting
    scale_x_continuous(
      breaks = pretty_breaks(n = 6),
      expand = expansion(mult = 0.05)
    ) +
    scale_y_continuous(
      breaks = pretty_breaks(n = 6),
      expand = expansion(mult = 0.05)
    ) +
    # Labels with variance explained - NO TITLE
    labs(
      title = NULL,  # Removed title
      x = paste0("PC2 (", round(var_explained[2] * 100, 2), "% variance)"),
      y = paste0("PC3 (", round(var_explained[3] * 100, 2), "% variance)")
    ) +
    # Clean theme with MUCH LARGER fonts
    theme_minimal(base_size = 20) +  # Increased from 12 to 20
    theme(
      # Plot elements
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      
      # Axes - INCREASED SIZES
      axis.title = element_text(size = 22, face = "bold", color = "grey20"),  # Increased from 13 to 22
      axis.title.x = element_text(margin = margin(t = 12)),  # Increased margin
      axis.title.y = element_text(margin = margin(r = 12)),  # Increased margin
      axis.text = element_text(size = 18, color = "grey30"),  # Increased from 11 to 18
      axis.line = element_line(color = "grey60", linewidth = 0.6),  # Increased from 0.4
      axis.ticks = element_line(color = "grey60", linewidth = 0.5),  # Increased from 0.3
      axis.ticks.length = unit(5, "pt"),  # Increased from 3
      
      # Grid
      panel.grid.major = element_line(color = "grey90", linewidth = 0.4),  # Increased from 0.3
      panel.grid.minor = element_blank(),
      
      # Legend - REMOVED for combined figure
      legend.position = "none",
      
      # Margins
      plot.margin = margin(20, 20, 20, 20)  # Increased from 15
    )
  
  # Try to add ellipses if possible
  tryCatch({
    pc2_pc3_plot <- pc2_pc3_plot + 
      stat_ellipse(aes(fill = Watershed), alpha = 0.15, level = 0.95, 
                   geom = "polygon", show.legend = FALSE, linewidth = 1) +  # Increased linewidth
      scale_fill_manual(values = colors, guide = "none")
    cat("  Added confidence ellipses to PC2 vs PC3 plot\n")
  }, error = function(e) {
    cat("  Warning: Could not add ellipses to PC2 vs PC3 plot\n")
  })
  
  # Save PC2 vs PC3 plot
  pc2_pc3_filename <- paste0(dataset_name, "_PC2_vs_PC3.pdf")
  pc2_pc3_filepath <- file.path(output_directory, pc2_pc3_filename)
  ggsave(pc2_pc3_filepath, pc2_pc3_plot, 
         width = 10, height = 8, 
         device = cairo_pdf,
         dpi = 300,
         units = "in")
  cat("Saved PC2 vs PC3 plot:", pc2_pc3_filename, "\n")
  
  # =============================================================================
  # COMBINED PLOT - ONE SHARED LEGEND AT BOTTOM, LARGER FONTS
  # =============================================================================
  
  # Create PC2 vs PC3 plot WITH LEGEND (will be used for combined plot)
  pc2_pc3_plot_with_legend <- ggplot(pc_scores, aes(x = PC2, y = PC3, color = Watershed)) +
    # Add points with transparency
    geom_point(size = 4, alpha = 0.7, stroke = 0.3) +
    # Use the same watershed colors
    scale_color_manual(values = colors, name = "Watershed") +
    # Clean axis formatting
    scale_x_continuous(
      breaks = pretty_breaks(n = 6),
      expand = expansion(mult = 0.05)
    ) +
    scale_y_continuous(
      breaks = pretty_breaks(n = 6),
      expand = expansion(mult = 0.05)
    ) +
    # Labels with variance explained - NO TITLE
    labs(
      title = NULL,
      x = paste0("PC2 (", round(var_explained[2] * 100, 2), "% variance)"),
      y = paste0("PC3 (", round(var_explained[3] * 100, 2), "% variance)")
    ) +
    # Clean theme with MUCH LARGER fonts
    theme_minimal(base_size = 20) +
    theme(
      # Plot elements
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      
      # Axes - INCREASED SIZES
      axis.title = element_text(size = 22, face = "bold", color = "grey20"),
      axis.title.x = element_text(margin = margin(t = 12)),
      axis.title.y = element_text(margin = margin(r = 12)),
      axis.text = element_text(size = 18, color = "grey30"),
      axis.line = element_line(color = "grey60", linewidth = 0.6),
      axis.ticks = element_line(color = "grey60", linewidth = 0.5),
      axis.ticks.length = unit(5, "pt"),
      
      # Grid
      panel.grid.major = element_line(color = "grey90", linewidth = 0.4),
      panel.grid.minor = element_blank(),
      
      # Legend - LARGE AND VISIBLE
      legend.position = "bottom",
      legend.title = element_text(size = 24, face = "bold", color = "grey20"),
      legend.text = element_text(size = 22, color = "grey30"),
      legend.key.size = unit(2, "lines"),
      legend.key = element_blank(),
      legend.spacing.x = unit(0.8, "cm"),
      legend.margin = margin(t = 15),
      legend.box.margin = margin(t = 10),
      
      # Margins
      plot.margin = margin(20, 20, 20, 20)
    )
  
  # Try to add ellipses to the version with legend
  tryCatch({
    pc2_pc3_plot_with_legend <- pc2_pc3_plot_with_legend + 
      stat_ellipse(aes(fill = Watershed), alpha = 0.15, level = 0.95, 
                   geom = "polygon", show.legend = FALSE, linewidth = 1) +
      scale_fill_manual(values = colors, guide = "none")
  }, error = function(e) {
    cat("  Warning: Could not add ellipses to PC2 vs PC3 plot with legend\n")
  })
  
  # Create combined plot using patchwork with legend on bottom plot only
  combined_plot_with_legend <- (pc1_pc2_plot / pc2_pc3_plot_with_legend) +
    plot_layout(nrow = 2) +
    plot_annotation(
      title = NULL,  # REMOVED MAIN TITLE
      theme = theme_void() +
        theme(
          plot.background = element_rect(fill = "white", color = NA),
          plot.margin = margin(15, 15, 15, 15)
        )
    )
  
  # Save combined plot (adjusted dimensions for column layout)
  combined_filename <- paste0(dataset_name, "_Combined_PCA_Views.pdf")
  combined_filepath <- file.path(output_directory, combined_filename)
  ggsave(combined_filepath, combined_plot_with_legend, 
         width = 10, height = 16,
         device = cairo_pdf,
         dpi = 300,
         units = "in")
  cat("Saved combined PCA views:", combined_filename, "\n")
  
  return(list(
    pc_scores = pc_scores,
    var_explained = var_explained,
    pca_result = pca_result,
    pc1_pc2_plot = pc1_pc2_plot,
    pc2_pc3_plot = pc2_pc3_plot,
    combined_plot = combined_plot_with_legend
  ))
}

# =============================================================================
# CREATE PCA SUMMARY PLOTS FIRST
# =============================================================================

# Create TOTAL dataset PCA plot
cat("Creating PCA summary plot for TOTAL dataset...\n")
total_pca <- create_pca_summary_plot(
  gam_data = gam_data_full,
  dataset_name = "TOTAL",
  title_suffix = ""
)

# Save TOTAL PCA plot
total_pca_filename <- "TOTAL_PCA_Summary.pdf"
total_pca_filepath <- file.path(output_dir, total_pca_filename)
ggsave(total_pca_filepath, total_pca$plot, 
       width = 10, height = 8, 
       device = cairo_pdf,
       dpi = 300,
       units = "in")
cat("Saved:", total_pca_filename, "\n")

# Create OVERLAP dataset PCA plot
cat("Creating PCA summary plot for OVERLAP dataset...\n")
overlap_pca <- create_pca_summary_plot(
  gam_data = gam_data_filtered,
  dataset_name = "OVERLAP",
  title_suffix = " (Natal_Iso ≤ 0.715)"
)

# Save OVERLAP PCA plot
overlap_pca_filename <- "OVERLAP_PCA_Summary.pdf"
overlap_pca_filepath <- file.path(output_dir, overlap_pca_filename)
ggsave(overlap_pca_filepath, overlap_pca$plot, 
       width = 10, height = 8, 
       device = cairo_pdf,
       dpi = 300,
       units = "in")
cat("Saved:", overlap_pca_filename, "\n")

# =============================================================================
# CREATE INDIVIDUAL PLOTS FOR ALL DATASETS
# =============================================================================

# TOTAL dataset individual plots
cat("\nCreating individual PCA loading plots for TOTAL dataset...\n")
total_results <- create_individual_pca_plots(
  gam_data = gam_data_full,
  raw_data = raw_data_full,
  dataset_name = "TOTAL",
  title_suffix = ""
)

# OVERLAP dataset individual plots
cat("\nCreating individual PCA loading plots for OVERLAP dataset...\n")
overlap_results <- create_individual_pca_plots(
  gam_data = gam_data_filtered,
  raw_data = raw_data_filtered,
  dataset_name = "OVERLAP",
  title_suffix = " (Natal_Iso ≤ 0.715)"
)

# SAME NO dataset individual plots (all samples with configurable natal origin range)
cat("\nCreating individual PCA loading plots for", same_no_range$name, "dataset (", same_no_range$min, "-", same_no_range$max, ")...\n")
same_no_results <- create_individual_pca_plots(
  gam_data = gam_data_same_no,
  raw_data = raw_data_same_no,
  dataset_name = same_no_range$name,
  title_suffix = paste0(" (Natal_Iso: ", same_no_range$min, "-", same_no_range$max, ")"),
  output_directory = same_no_output_dir,
  use_all_samples = TRUE
)

# =============================================================================
# CREATE COMBINED FIGURE FOR SPECIFIC FISH IDs (NOW WITH PC1, PC2, PC3)
# =============================================================================
cat("\nCreating combined figure for specific fish with same natal origin...\n")

# Target fish IDs to combine - updated to match the desired figure
target_fish_ids <- c("2011_nk_42", "2016_yk_197", "2017_kk_134redo")

# Check which fish are available in the same NO dataset
available_fish <- gam_data_same_no %>%
  filter(Fish_id %in% target_fish_ids) %>%
  select(Fish_id, Watershed, Natal_Iso) %>%
  arrange(match(Fish_id, target_fish_ids))

cat("Target fish found for combined figure:\n")
print(available_fish)

if(nrow(available_fish) == 3) {
  
  # Use PCA results from same_no_results
  pca_result_combined <- same_no_results$pca_result
  var_explained_combined <- same_no_results$var_explained
  loadings_df_combined <- same_no_results$loadings_data
  
  # Calculate global color scale limits for consistency across panels
  pc1_range <- range(loadings_df_combined$PC1_abs, na.rm = TRUE)
  pc2_range <- range(loadings_df_combined$PC2_abs, na.rm = TRUE)
  pc3_range <- range(loadings_df_combined$PC3_abs, na.rm = TRUE)  # Add PC3 range
  
  # Function to create individual panels for combined figure
  create_fish_panel <- function(fish_id, pc_num, show_legend = FALSE, show_y_title = FALSE, is_middle = FALSE) {
    
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
    
    # Create data frame
    fish_ts_df <- data.frame(
      time_point = time_points,
      sr_ratio_gam = gam_values,
      sr_ratio_raw = raw_values,
      PC1_abs = loadings_df_combined$PC1_abs,
      PC2_abs = loadings_df_combined$PC2_abs,
      PC3_abs = loadings_df_combined$PC3_abs  # Add PC3
    )
    
    # Select the appropriate PC and color range
    if(pc_num == 1) {
      color_var <- fish_ts_df$PC1_abs
      legend_name <- "|PC1|"
      color_limits <- pc1_range
    } else if(pc_num == 2) {
      color_var <- fish_ts_df$PC2_abs
      legend_name <- "|PC2|"
      color_limits <- pc2_range
    } else {  # pc_num == 3
      color_var <- fish_ts_df$PC3_abs
      legend_name <- "|PC3|"
      color_limits <- pc3_range
    }
    
    # Create the plot
    p <- ggplot(fish_ts_df, aes(x = time_point)) +
      # Raw data points
      geom_point(aes(y = sr_ratio_raw), color = "grey70", size = 1.0, alpha = 0.6) +
      # GAM smoothed line
      geom_line(aes(y = sr_ratio_gam), color = "grey40", alpha = 0.9, size = 1.0) +
      # GAM points colored by loadings
      geom_point(aes(y = sr_ratio_gam, color = color_var), size = 1.8, alpha = 1.0, stroke = 0) +
      # Plasma color scale - darker for higher values, consistent across row
      scale_color_viridis_c(
        name = legend_name,
        option = "plasma",
        begin = 0.9,
        end = 0.1,
        direction = -1,
        limits = color_limits,
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
      # Fixed y-axis limits
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
        x = if(pc_num == 3) "Time Point" else NULL,  # Only show x-axis label on bottom panel (PC3)
        y = if(show_y_title) expression(paste(""^87, "Sr/", ""^86, "Sr")) else NULL
      ) +
      # Clean theme
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
  
  # Create all panels (NOW THREE ROWS)
  panels <- list()
  
  # Row 1: PC1 panels (legend in middle)
  panels$pc1_fish1 <- create_fish_panel(target_fish_ids[1], 1, show_legend = FALSE, show_y_title = TRUE, is_middle = FALSE)
  panels$pc1_fish2 <- create_fish_panel(target_fish_ids[2], 1, show_legend = TRUE, show_y_title = FALSE, is_middle = TRUE)
  panels$pc1_fish3 <- create_fish_panel(target_fish_ids[3], 1, show_legend = FALSE, show_y_title = FALSE, is_middle = FALSE)
  
  # Row 2: PC2 panels (legend in middle)
  panels$pc2_fish1 <- create_fish_panel(target_fish_ids[1], 2, show_legend = FALSE, show_y_title = TRUE, is_middle = FALSE)
  panels$pc2_fish2 <- create_fish_panel(target_fish_ids[2], 2, show_legend = TRUE, show_y_title = FALSE, is_middle = TRUE)
  panels$pc2_fish3 <- create_fish_panel(target_fish_ids[3], 2, show_legend = FALSE, show_y_title = FALSE, is_middle = FALSE)
  
  # Row 3: PC3 panels (legend in middle)
  panels$pc3_fish1 <- create_fish_panel(target_fish_ids[1], 3, show_legend = FALSE, show_y_title = TRUE, is_middle = FALSE)
  panels$pc3_fish2 <- create_fish_panel(target_fish_ids[2], 3, show_legend = TRUE, show_y_title = FALSE, is_middle = TRUE)
  panels$pc3_fish3 <- create_fish_panel(target_fish_ids[3], 3, show_legend = FALSE, show_y_title = FALSE, is_middle = FALSE)
  
  # Combine using patchwork (THREE ROWS)
  combined_figure <- (panels$pc1_fish1 | panels$pc1_fish2 | panels$pc1_fish3) /
    (panels$pc2_fish1 | panels$pc2_fish2 | panels$pc2_fish3) /
    (panels$pc3_fish1 | panels$pc3_fish2 | panels$pc3_fish3) +
    plot_layout(heights = c(1, 1, 1)) +  # Three equal height rows
    plot_annotation(
      title = paste0("Comparison among individuals with the same natal origin (", same_no_range$min, "-", same_no_range$max, ")"),
      subtitle = paste0("PC1 (", round(var_explained_combined[1] * 100, 1), "%), PC2 (", 
                        round(var_explained_combined[2] * 100, 1), "%), and PC3 (", 
                        round(var_explained_combined[3] * 100, 1), "%) loadings on time series"),
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
  
  # Save combined figure (adjusted height for 3 rows)
  combined_filename <- paste0("Combined_", same_no_range$name, "_Comparison.pdf")
  combined_filepath <- file.path(same_no_output_dir, combined_filename)
  
  ggsave(combined_filepath, combined_figure, 
         width = 12, height = 12,  # Changed from height = 8 to height = 12 for 3 rows
         device = cairo_pdf,
         dpi = 300,
         units = "in")
  
  cat("Saved combined figure:", combined_filename, "\n")
  
  # =============================================================================
  # CREATE SPECIFIC THREE-PANEL FIGURE WITH EXACT FISH IDs
  # =============================================================================
  
  # Create the specific figure requested with these exact fish
  specific_fish_ids <- c("2011_nk_42", "2016_yk_197", "2017_kk_134redo")
  
  # Check which of these specific fish are available
  available_specific_fish <- gam_data_same_no %>%
    filter(Fish_id %in% specific_fish_ids) %>%
    select(Fish_id, Watershed, Natal_Iso) %>%
    arrange(match(Fish_id, specific_fish_ids))
  
  cat("\nSpecific fish found for requested figure:\n")
  print(available_specific_fish)
  
  if(nrow(available_specific_fish) >= 1) {
    
    # Create panels for each available fish
    specific_panels <- list()
    
    for(fish_idx in 1:nrow(available_specific_fish)) {
      fish_id <- available_specific_fish$Fish_id[fish_idx]
      watershed <- available_specific_fish$Watershed[fish_idx]
      
      # Create the three PC panels for this fish
      specific_panels[[paste0("pc1_fish", fish_idx)]] <- create_fish_panel(
        fish_id, 1, 
        show_legend = (fish_idx == 2), # Show legend on middle fish
        show_y_title = (fish_idx == 1), # Show y-title on first fish
        is_middle = (fish_idx == 2)
      )
      
      specific_panels[[paste0("pc2_fish", fish_idx)]] <- create_fish_panel(
        fish_id, 2, 
        show_legend = (fish_idx == 2), # Show legend on middle fish
        show_y_title = (fish_idx == 1), # Show y-title on first fish
        is_middle = (fish_idx == 2)
      )
      
      specific_panels[[paste0("pc3_fish", fish_idx)]] <- create_fish_panel(
        fish_id, 3, 
        show_legend = (fish_idx == 2), # Show legend on middle fish
        show_y_title = (fish_idx == 1), # Show y-title on first fish
        is_middle = (fish_idx == 2)
      )
    }
    
    # Create the specific combined figure based on number of available fish
    if(nrow(available_specific_fish) == 3) {
      # All three fish available - create 3x3 grid
      specific_combined_figure <- (specific_panels$pc1_fish1 | specific_panels$pc1_fish2 | specific_panels$pc1_fish3) /
        (specific_panels$pc2_fish1 | specific_panels$pc2_fish2 | specific_panels$pc2_fish3) /
        (specific_panels$pc3_fish1 | specific_panels$pc3_fish2 | specific_panels$pc3_fish3) +
        plot_layout(heights = c(1, 1, 1)) +
        plot_annotation(
          title = "Comparison among specific individuals with the same natal origin",
          subtitle = paste0("PC1 (", round(var_explained_combined[1] * 100, 1), "%), PC2 (", 
                            round(var_explained_combined[2] * 100, 1), "%), and PC3 (", 
                            round(var_explained_combined[3] * 100, 1), "%) loadings on time series"),
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
      
      figure_width <- 12
      figure_height <- 12
      
    } else if(nrow(available_specific_fish) == 2) {
      # Two fish available - create 3x2 grid
      specific_combined_figure <- (specific_panels$pc1_fish1 | specific_panels$pc1_fish2) /
        (specific_panels$pc2_fish1 | specific_panels$pc2_fish2) /
        (specific_panels$pc3_fish1 | specific_panels$pc3_fish2) +
        plot_layout(heights = c(1, 1, 1)) +
        plot_annotation(
          title = "Comparison among specific individuals with the same natal origin",
          subtitle = paste0("PC1 (", round(var_explained_combined[1] * 100, 1), "%), PC2 (", 
                            round(var_explained_combined[2] * 100, 1), "%), and PC3 (", 
                            round(var_explained_combined[3] * 100, 1), "%) loadings on time series"),
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
      
      figure_width <- 8
      figure_height <- 12
      
    } else {
      # One fish available - create 3x1 grid
      specific_combined_figure <- specific_panels$pc1_fish1 /
        specific_panels$pc2_fish1 /
        specific_panels$pc3_fish1 +
        plot_layout(heights = c(1, 1, 1)) +
        plot_annotation(
          title = "Specific individual with the same natal origin",
          subtitle = paste0("PC1 (", round(var_explained_combined[1] * 100, 1), "%), PC2 (", 
                            round(var_explained_combined[2] * 100, 1), "%), and PC3 (", 
                            round(var_explained_combined[3] * 100, 1), "%) loadings on time series"),
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
      
      figure_width <- 4
      figure_height <- 12
    }
    
    # Save the specific combined figure
    specific_combined_filename <- paste0("Specific_Combined_", same_no_range$name, "_Comparison.pdf")
    specific_combined_filepath <- file.path(same_no_output_dir, specific_combined_filename)
    
    ggsave(specific_combined_filepath, specific_combined_figure, 
           width = figure_width, height = figure_height,
           device = cairo_pdf,
           dpi = 300,
           units = "in")
    
    cat("Saved specific combined figure:", specific_combined_filename, "\n")
    cat("Figure shows", nrow(available_specific_fish), "fish in", nrow(available_specific_fish), "x 3 layout\n")
    
  } else {
    cat("Warning: No specific target fish found in", same_no_range$name, "dataset\n")
  }
  
} else {
  cat("Warning: Not all target fish found in", same_no_range$name, "dataset\n")
}

# =============================================================================
# CREATE ADDITIONAL 2D PCA PLOTS FOR SAME NO DATASET
# =============================================================================

cat("\nCreating additional 2D PCA plots (PC1 vs PC2 and PC2 vs PC3) for", same_no_range$name, "dataset...\n")

# Create the additional 2D plots
same_no_additional_results <- create_additional_pca_plots(
  gam_data = gam_data_same_no,
  dataset_name = same_no_range$name,
  output_directory = additional_2d_dir
)

# =============================================================================
# SUMMARY - PCA PLOTS AND INDIVIDUAL PLOTS
# =============================================================================
cat("\n", paste(rep("=", 80), collapse = ""), "\n")
cat("PCA SUMMARY PLOTS CREATED\n")
cat(paste(rep("=", 80), collapse = ""), "\n")

cat("TOTAL Dataset PCA Summary:\n")
cat("  - Samples:", nrow(total_pca$pc_scores), "\n")
cat("  - PC1 variance explained:", round(total_pca$var_explained[1] * 100, 1), "%\n")
cat("  - PC2 variance explained:", round(total_pca$var_explained[2] * 100, 1), "%\n")
cat("  - Watershed distribution:\n")
print(table(total_pca$pc_scores$Watershed))

cat("\nOVERLAP Dataset PCA Summary:\n")
cat("  - Samples:", nrow(overlap_pca$pc_scores), "\n")
cat("  - PC1 variance explained:", round(overlap_pca$var_explained[1] * 100, 1), "%\n")
cat("  - PC2 variance explained:", round(overlap_pca$var_explained[2] * 100, 1), "%\n")
cat("  - Watershed distribution:\n")
print(table(overlap_pca$pc_scores$Watershed))

cat("\n", paste(rep("=", 80), collapse = ""), "\n")
cat("INDIVIDUAL PCA LOADINGS ANALYSIS COMPLETE\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
cat("Output Directory:", output_dir, "\n\n")

cat("Summary Files Created:\n")
cat("  -", total_pca_filename, "\n")
cat("  -", overlap_pca_filename, "\n")

cat("\nIndividual Files Created:\n")
cat("TOTAL Dataset - ", nrow(total_results$selected_samples), " individual PDFs:\n")
for(i in 1:min(5, nrow(total_results$selected_samples))) {
  fish_data <- total_results$selected_samples[i, ]
  filename <- paste0("TOTAL_", fish_data$Fish_id, "_", fish_data$Watershed, "_PCA_Loadings.pdf")
  cat("  -", filename, "\n")
}
if(nrow(total_results$selected_samples) > 5) {
  cat("  ... and", nrow(total_results$selected_samples) - 5, "more files\n")
}

cat("\nOVERLAP Dataset - ", nrow(overlap_results$selected_samples), " individual PDFs:\n")
for(i in 1:min(5, nrow(overlap_results$selected_samples))) {
  fish_data <- overlap_results$selected_samples[i, ]
  filename <- paste0("OVERLAP_", fish_data$Fish_id, "_", fish_data$Watershed, "_PCA_Loadings.pdf")
  cat("  -", filename, "\n")
}
if(nrow(overlap_results$selected_samples) > 5) {
  cat("  ... and", nrow(overlap_results$selected_samples) - 5, "more files\n")
}

cat("\n", same_no_range$name, "Dataset - ", nrow(same_no_results$selected_samples), " individual PDFs:\n")
for(i in 1:min(5, nrow(same_no_results$selected_samples))) {
  fish_data <- same_no_results$selected_samples[i, ]
  filename <- paste0(same_no_range$name, "_", fish_data$Fish_id, "_", fish_data$Watershed, "_PCA_Loadings.pdf")
  cat("  -", filename, "\n")
}
if(nrow(same_no_results$selected_samples) > 5) {
  cat("  ... and", nrow(same_no_results$selected_samples) - 5, "more files\n")
}

cat("\nOutput Directories:\n")
cat("  - TOTAL & OVERLAP:", output_dir, "\n")
cat("  -", same_no_range$name, ":", same_no_output_dir, "\n")

# =============================================================================
# 2D PCA ANALYSIS SUMMARY
# =============================================================================

cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("2D PCA ANALYSIS COMPLETE\n")
cat(paste(rep("=", 60), collapse = ""), "\n")
cat("2D Output Directory:", additional_2d_dir, "\n\n")

cat("2D PCA Results for", same_no_range$name, "Dataset:\n")
cat("  - Samples:", nrow(same_no_additional_results$pc_scores), "\n")
cat("  - PC1 variance:", round(same_no_additional_results$var_explained[1] * 100, 1), "%\n")
cat("  - PC2 variance:", round(same_no_additional_results$var_explained[2] * 100, 1), "%\n")
cat("  - PC3 variance:", round(same_no_additional_results$var_explained[3] * 100, 1), "%\n")
cat("  - Total variance (PC1-3):", round(sum(same_no_additional_results$var_explained) * 100, 1), "%\n")

cat("\nWatershed Distribution:\n")
print(table(same_no_additional_results$pc_scores$Watershed))

cat("\n2D Files Created:\n")
cat("  -", paste0(same_no_range$name, "_PC1_vs_PC2.pdf"), "(PC1 vs PC2 scatter plot)\n")
cat("  -", paste0(same_no_range$name, "_PC2_vs_PC3.pdf"), "(PC2 vs PC3 scatter plot)\n")
cat("  -", paste0(same_no_range$name, "_Combined_PCA_Views.pdf"), "(column layout comparison)\n")

cat("\nPublication-quality features:\n")
cat("  - High-resolution vector PDFs (cairo_pdf)\n")
cat("  - Clean, minimal design with proper typography\n")
cat("  - Professional color schemes (darker = higher loadings)\n")
cat("  - Scientific notation for isotope ratios\n")
cat("  - Embedded fonts for consistency\n")
cat("  - PCA summary plots with 95% confidence ellipses\n")
cat("  - Three-panel layout: PC1 (top), PC2 (middle), and PC3 (bottom)\n")
cat("  - Raw data shown as grey points\n")
cat("  - GAM-smoothed data as colored line and points\n")
cat("  - Points colored by absolute loading magnitude\n")
cat("  - 2D scatter plots with multiple viewing angles in column layout\n")
cat("  - Different colors and point shapes for each watershed\n")
cat("  - Variance explained shown on each axis\n")

cat("\n", paste(rep("=", 80), collapse = ""), "\n")
cat("MAJOR CHANGES IMPLEMENTED:\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
cat("1. ADDED PC3 TO INDIVIDUAL LOADINGS PLOTS:\n")
cat("   - Now creates three-panel plots (PC1, PC2, PC3) instead of two\n")
cat("   - Increased plot height from 11 to 14 inches for better proportions\n")
cat("   - PC3 loadings calculated and displayed with same color scheme\n")
cat("   - Only bottom panel (PC3) shows x-axis label\n")
cat("   - All three PCs shown in variance explained summary\n\n")

cat("2. CHANGED 2D PCA PLOTS TO COLUMN LAYOUT:\n")
cat("   - Combined plots now arranged vertically (PC1 vs PC2 above PC2 vs PC3)\n")
cat("   - Adjusted dimensions from 16x8 to 10x16 inches\n")
cat("   - Better use of vertical space for detailed viewing\n")
cat("   - Consistent with three-panel individual plots\n\n")

cat("3. UPDATED COMBINED COMPARISON FIGURE:\n")
cat("   - Now shows three rows (PC1, PC2, PC3) x three fish\n")
cat("   - Increased height from 8 to 12 inches for three rows\n")
cat("   - PC3 color scale calculated and applied consistently\n")
cat("   - Only bottom row shows x-axis labels\n")
cat("   - Enhanced subtitle to mention all three PCs\n\n")

cat("TO EXPERIMENT WITH DIFFERENT RANGES:\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
cat("Modify the 'same_no_range' list at the top of the script:\n\n")
cat("Examples:\n")
cat("  same_no_range <- list(min = 0.7070, max = 0.7085, name = \"SAME_NO_7070_7085\")\n")
cat("  same_no_range <- list(min = 0.7072, max = 0.7078, name = \"SAME_NO_7072_7078\")\n")
cat("  same_no_range <- list(min = 0.7074, max = 0.7082, name = \"SAME_NO_7074_7082\")\n")
cat("  same_no_range <- list(min = 0.7076, max = 0.7079, name = \"SAME_NO_7076_7079\")\n")

cat("\nThe script will automatically:\n")
cat("  - Filter data using your new range\n")
cat("  - Create appropriately named directories\n")
cat("  - Generate files with the range in the filename\n")
cat("  - Update all plot titles and descriptions\n")
cat("  - Include PC3 in all analyses\n")
cat("  - Use column layouts for better vertical space utilization\n")

cat("\n", paste(rep("=", 80), collapse = ""), "\n")
cat("ANALYSIS COMPLETE!\n")
cat("Enhanced plots with PC3 and column layouts saved successfully. Ready for publication!\n")
cat(paste(rep("=", 80), collapse = ""), "\n")