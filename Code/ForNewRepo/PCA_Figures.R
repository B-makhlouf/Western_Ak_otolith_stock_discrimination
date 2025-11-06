# PCA_Loadings_Individual_TimeSeries.R
# Creates individual three-panel figures for each selected fish showing PC1, PC2, PC3 loadings
# Also creates summary PCA plots and 2D PCA comparison plots

# =============================================================================
# SETUP
# =============================================================================
library(tidyverse)
library(ggplot2)
library(viridis)
library(cowplot)
library(patchwork)
library(scales)

set.seed(42)

# =============================================================================
# CONFIGURABLE PARAMETERS
# =============================================================================

same_no_range <- list(
  min = 0.7080,
  max = 0.7085,
  name = "SAME_NO_7080_7085"
)

# Paths and colors
gam_data_path <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/data/LA_Data/Preprocessed_ts_matrices/NatalToMarine_Processed_GAM.csv"
raw_data_path <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/data/LA_Data/Preprocessed_ts_matrices/NatalToMarine_Processed_RAW.csv"
output_dir <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/Figures/PCA Figures"
watershed_colors <- c("Kusko" = "firebrick", "Nush" = "darkgreen", "Yukon" = "dodgerblue")

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# LOAD AND PREPARE DATA
# =============================================================================
gam_data_full <- read.csv(gam_data_path) %>%
  mutate(Watershed = as.factor(Watershed))

raw_data_full <- read.csv(raw_data_path) %>%
  mutate(Watershed = as.factor(Watershed))

gam_data_filtered <- gam_data_full %>%
  filter(Natal_Iso <= 0.715)

raw_data_filtered <- raw_data_full %>%
  filter(Natal_Iso <= 0.715)

gam_data_same_no <- gam_data_full %>%
  filter(Natal_Iso >= same_no_range$min & Natal_Iso <= same_no_range$max)

raw_data_same_no <- raw_data_full %>%
  filter(Natal_Iso >= same_no_range$min & Natal_Iso <= same_no_range$max)

metadata_cols <- c("Fish_id", "Watershed", "Natal_Iso", "Year", "Natal_Start", 
                   "Marine_Start", "Marine_End", "Original_Data_Points", 
                   "Interpolated_Points", "Micron_Size")
feature_cols <- grep("^X", names(gam_data_full), value = TRUE)
time_points <- as.numeric(gsub("^X", "", feature_cols))

cat("GAM TOTAL dataset:", nrow(gam_data_full), "samples with", length(feature_cols), "time points\n")
cat("GAM OVERLAP dataset:", nrow(gam_data_filtered), "samples\n")
cat("GAM SAME NO dataset (", same_no_range$min, "-", same_no_range$max, "):", nrow(gam_data_same_no), "samples\n")

# =============================================================================
# FUNCTION TO CREATE PCA SUMMARY PLOTS
# =============================================================================
create_pca_summary_plot <- function(gam_data, dataset_name, title_suffix = "") {
  
  pca_result <- prcomp(gam_data[, feature_cols], scale. = TRUE)
  var_explained <- (pca_result$sdev^2 / sum(pca_result$sdev^2))[1:2]
  
  pc_scores <- data.frame(
    PC1 = pca_result$x[, 1],
    PC2 = pca_result$x[, 2],
    Watershed = gam_data$Watershed,
    Fish_id = gam_data$Fish_id
  )
  
  pca_plot <- ggplot(pc_scores, aes(x = PC1, y = PC2, color = Watershed)) +
    geom_point(size = 2.5, alpha = 0.8, stroke = 0.2) +
    scale_color_manual(values = watershed_colors, name = "Watershed") +
    scale_x_continuous(breaks = pretty_breaks(n = 6), expand = expansion(mult = 0.05)) +
    scale_y_continuous(breaks = pretty_breaks(n = 6), expand = expansion(mult = 0.05)) +
    labs(
      title = paste0("Principal Component Analysis", title_suffix),
      subtitle = paste0("GAM-smoothed ", nrow(gam_data), " fish by Watershed"),
      x = paste0("PC1 (", round(var_explained[1] * 100, 1), "%)"),
      y = paste0("PC2 (", round(var_explained[2] * 100, 1), "%)")
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5, color = "grey15", margin = margin(b = 5)),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "grey40", margin = margin(b = 15)),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      axis.title = element_text(size = 13, face = "bold", color = "grey20"),
      axis.title.x = element_text(margin = margin(t = 8)),
      axis.title.y = element_text(margin = margin(r = 8)),
      axis.text = element_text(size = 11, color = "grey30"),
      axis.line = element_line(color = "grey60", linewidth = 0.4),
      axis.ticks = element_line(color = "grey60", linewidth = 0.3),
      axis.ticks.length = unit(3, "pt"),
      panel.grid.major = element_line(color = "grey90", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      legend.title = element_text(size = 12, face = "bold", color = "grey20"),
      legend.text = element_text(size = 11, color = "grey30"),
      legend.key = element_blank(),
      legend.margin = margin(t = 10),
      legend.box.margin = margin(t = 5),
      plot.margin = margin(15, 15, 15, 15)
    )
  
  tryCatch({
    pca_plot <- pca_plot + 
      stat_ellipse(aes(fill = Watershed), alpha = 0.15, level = 0.95, 
                   geom = "polygon", show.legend = FALSE) +
      scale_fill_manual(values = watershed_colors, guide = "none")
  }, error = function(e) {
    cat("  Warning: Could not add ellipses\n")
  })
  
  return(list(
    plot = pca_plot,
    pc_scores = pc_scores,
    var_explained = var_explained,
    pca_result = pca_result
  ))
}

# =============================================================================
# FUNCTION TO RUN PCA AND RETURN RESULTS
# =============================================================================
run_pca_analysis <- function(gam_data) {
  pca_result <- prcomp(gam_data[, feature_cols], scale. = TRUE)
  var_explained <- (pca_result$sdev^2 / sum(pca_result$sdev^2))[1:3]
  
  loadings_df <- data.frame(
    time_point = time_points,
    PC1_loading = pca_result$rotation[, 1],
    PC2_loading = pca_result$rotation[, 2],
    PC3_loading = pca_result$rotation[, 3]
  ) %>%
    mutate(
      PC1_abs = abs(PC1_loading),
      PC2_abs = abs(PC2_loading),
      PC3_abs = abs(PC3_loading)
    )
  
  return(list(
    loadings_data = loadings_df,
    var_explained = var_explained,
    pca_result = pca_result
  ))
}

# =============================================================================
# ADDITIONAL 2D PCA PLOTS FUNCTION
# =============================================================================
create_additional_pca_plots <- function(gam_data, dataset_name, output_directory) {
  
  pca_result <- prcomp(gam_data[, feature_cols], scale. = TRUE)
  var_explained <- (pca_result$sdev^2 / sum(pca_result$sdev^2))[1:3]
  
  pc_scores <- data.frame(
    PC1 = pca_result$x[, 1],
    PC2 = pca_result$x[, 2],
    PC3 = pca_result$x[, 3],
    Watershed = gam_data$Watershed,
    Fish_id = gam_data$Fish_id,
    Natal_Iso = gam_data$Natal_Iso
  )
  
  colors <- c("Kusko" = "firebrick", "Nush" = "darkgreen", "Yukon" = "dodgerblue")
  
  # PC1 vs PC2 plot
  pc1_pc2_plot <- ggplot(pc_scores, aes(x = PC1, y = PC2, color = Watershed)) +
    geom_point(size = 5, alpha = 0.7, stroke = 0.3) +
    scale_color_manual(values = colors, name = "Watershed") +
    scale_x_continuous(breaks = pretty_breaks(n = 6), expand = expansion(mult = 0.05)) +
    scale_y_continuous(breaks = pretty_breaks(n = 6), expand = expansion(mult = 0.05)) +
    labs(
      title = NULL,
      x = paste0("PC1 (", round(var_explained[1] * 100, 2), "% variance)"),
      y = paste0("PC2 (", round(var_explained[2] * 100, 2), "% variance)")
    ) +
    theme_minimal(base_size = 24) +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      axis.title = element_text(size = 26, face = "bold", color = "grey20"),
      axis.title.x = element_text(margin = margin(t = 15)),
      axis.title.y = element_text(margin = margin(r = 15)),
      axis.text = element_text(size = 22, color = "grey30"),
      axis.line = element_line(color = "grey60", linewidth = 0.7),
      axis.ticks = element_line(color = "grey60", linewidth = 0.6),
      axis.ticks.length = unit(6, "pt"),
      panel.grid.major = element_line(color = "grey90", linewidth = 0.5),
      panel.grid.minor = element_blank(),
      legend.position = "none",
      plot.margin = margin(25, 25, 25, 25)
    )
  
  tryCatch({
    pc1_pc2_plot <- pc1_pc2_plot + 
      stat_ellipse(aes(fill = Watershed), alpha = 0.15, level = 0.95, 
                   geom = "polygon", show.legend = FALSE, linewidth = 1.2) +
      scale_fill_manual(values = colors, guide = "none")
  }, error = function(e) {
    cat("  Warning: Could not add ellipses to PC1 vs PC2 plot\n")
  })
  
  # PC2 vs PC3 plot with legend
  pc2_pc3_plot_with_legend <- ggplot(pc_scores, aes(x = PC2, y = PC3, color = Watershed)) +
    geom_point(size = 5, alpha = 0.7, stroke = 0.3) +
    scale_color_manual(values = colors, name = "Watershed") +
    scale_x_continuous(breaks = pretty_breaks(n = 6), expand = expansion(mult = 0.05)) +
    scale_y_continuous(breaks = pretty_breaks(n = 6), expand = expansion(mult = 0.05)) +
    labs(
      title = NULL,
      x = paste0("PC2 (", round(var_explained[2] * 100, 2), "% variance)"),
      y = paste0("PC3 (", round(var_explained[3] * 100, 2), "% variance)")
    ) +
    theme_minimal(base_size = 24) +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      axis.title = element_text(size = 26, face = "bold", color = "grey20"),
      axis.title.x = element_text(margin = margin(t = 15)),
      axis.title.y = element_text(margin = margin(r = 15)),
      axis.text = element_text(size = 22, color = "grey30"),
      axis.line = element_line(color = "grey60", linewidth = 0.7),
      axis.ticks = element_line(color = "grey60", linewidth = 0.6),
      axis.ticks.length = unit(6, "pt"),
      panel.grid.major = element_line(color = "grey90", linewidth = 0.5),
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      legend.title = element_text(size = 28, face = "bold", color = "grey20"),
      legend.text = element_text(size = 26, color = "grey30"),
      legend.key.size = unit(2.5, "lines"),
      legend.key = element_blank(),
      legend.spacing.x = unit(1.2, "cm"),
      legend.margin = margin(t = 20),
      legend.box.margin = margin(t = 15),
      plot.margin = margin(25, 25, 25, 25)
    )
  
  tryCatch({
    pc2_pc3_plot_with_legend <- pc2_pc3_plot_with_legend + 
      stat_ellipse(aes(fill = Watershed), alpha = 0.15, level = 0.95, 
                   geom = "polygon", show.legend = FALSE, linewidth = 1.2) +
      scale_fill_manual(values = colors, guide = "none")
  }, error = function(e) {
    cat("  Warning: Could not add ellipses to PC2 vs PC3 plot\n")
  })
  
  combined_plot_with_legend <- (pc1_pc2_plot / pc2_pc3_plot_with_legend) +
    plot_layout(nrow = 2) +
    plot_annotation(
      title = NULL,
      theme = theme_void() +
        theme(
          plot.background = element_rect(fill = "white", color = NA),
          plot.margin = margin(20, 20, 20, 20)
        )
    )
  
  combined_filename <- paste0(dataset_name, "_Combined_PCA_Views_Enhanced.pdf")
  combined_filepath <- file.path(output_directory, combined_filename)
  ggsave(combined_filepath, combined_plot_with_legend, 
         width = 11, height = 18,
         device = cairo_pdf,
         dpi = 300,
         units = "in")
  cat("Saved enhanced combined PCA views:", combined_filename, "\n")
  
  return(list(
    pc_scores = pc_scores,
    var_explained = var_explained,
    pca_result = pca_result,
    pc1_pc2_plot = pc1_pc2_plot,
    pc2_pc3_plot = pc2_pc3_plot_with_legend,
    combined_plot = combined_plot_with_legend
  ))
}

# =============================================================================
# CREATE PCA SUMMARY PLOTS
# =============================================================================

cat("Creating PCA summary plot for TOTAL dataset...\n")
total_pca <- create_pca_summary_plot(gam_data = gam_data_full, dataset_name = "TOTAL", title_suffix = "")
total_pca_filename <- "TOTAL_PCA_Summary.pdf"
total_pca_filepath <- file.path(output_dir, total_pca_filename)
ggsave(total_pca_filepath, total_pca$plot, width = 10, height = 8, device = cairo_pdf, dpi = 300, units = "in")
cat("Saved:", total_pca_filename, "\n")

cat("Creating PCA summary plot for OVERLAP dataset...\n")
overlap_pca <- create_pca_summary_plot(gam_data = gam_data_filtered, dataset_name = "OVERLAP", 
                                       title_suffix = " (Natal_Iso ≤ 0.715)")
overlap_pca_filename <- "OVERLAP_PCA_Summary.pdf"
overlap_pca_filepath <- file.path(output_dir, overlap_pca_filename)
ggsave(overlap_pca_filepath, overlap_pca$plot, width = 10, height = 8, device = cairo_pdf, dpi = 300, units = "in")
cat("Saved:", overlap_pca_filename, "\n")

# =============================================================================
# RUN PCA ANALYSIS ON SAME NO DATASET
# =============================================================================

cat("\nRunning PCA analysis for", same_no_range$name, "dataset...\n")
same_no_results <- run_pca_analysis(gam_data = gam_data_same_no)

# =============================================================================
# CREATE SPECIFIC FOUR-PANEL FIGURE WITH EXACT FISH IDs
# =============================================================================

cat("\nCreating four-panel figure with specific fish...\n")

# Create the specific figure requested with these exact fish - NOW WITH FOUR INDIVIDUALS
# Reordered as requested: Nushagak, Kuskokwim, Yukon (non-CWAK), Yukon (CWAK)
specific_fish_ids <- c("2011_nk_42", "2017_kk_134redo", "2016_yk_197", "2017_yk_201")

# Custom labels for each fish
fish_labels <- c(
  "2011_nk_42" = "Nushagak",
  "2017_kk_134redo" = "Kuskokwim", 
  "2016_yk_197" = "Yukon (non-CWAK)",
  "2017_yk_201" = "Yukon (CWAK)"
)

# Check which of these specific fish are available
available_specific_fish <- gam_data_same_no %>%
  filter(Fish_id %in% specific_fish_ids) %>%
  select(Fish_id, Watershed, Natal_Iso) %>%
  arrange(match(Fish_id, specific_fish_ids))

cat("Specific fish found for requested figure:\n")
print(available_specific_fish)

if(nrow(available_specific_fish) >= 1) {
  
  # Use PCA results from same NO dataset
  var_explained_combined <- same_no_results$var_explained
  loadings_df_combined <- same_no_results$loadings_data
  
  pc1_range <- range(loadings_df_combined$PC1_abs, na.rm = TRUE)
  pc2_range <- range(loadings_df_combined$PC2_abs, na.rm = TRUE)
  pc3_range <- range(loadings_df_combined$PC3_abs, na.rm = TRUE)
  
  create_fish_panel_custom <- function(fish_id, pc_num, show_legend = FALSE, show_y_title = FALSE, 
                                       is_middle = FALSE, custom_title = NULL, show_title = TRUE) {
    
    fish_data_gam <- gam_data_same_no %>% filter(Fish_id == fish_id)
    fish_data_raw <- raw_data_same_no %>% filter(Fish_id == fish_id)
    
    if(nrow(fish_data_gam) == 0 || nrow(fish_data_raw) == 0) {
      return(NULL)
    }
    
    watershed <- fish_data_gam$Watershed
    gam_values <- as.numeric(fish_data_gam[feature_cols])
    raw_values <- as.numeric(fish_data_raw[feature_cols])
    
    fish_ts_df <- data.frame(
      time_point = time_points,
      sr_ratio_gam = gam_values,
      sr_ratio_raw = raw_values,
      PC1_abs = loadings_df_combined$PC1_abs,
      PC2_abs = loadings_df_combined$PC2_abs,
      PC3_abs = loadings_df_combined$PC3_abs
    )
    
    if(pc_num == 1) {
      color_var <- fish_ts_df$PC1_abs
      legend_name <- "|PC1|"
      color_limits <- pc1_range
    } else if(pc_num == 2) {
      color_var <- fish_ts_df$PC2_abs
      legend_name <- "|PC2|"
      color_limits <- pc2_range
    } else {
      color_var <- fish_ts_df$PC3_abs
      legend_name <- "|PC3|"
      color_limits <- pc3_range
    }
    
    # Use custom title if provided, otherwise use fish_id
    plot_title <- if(!is.null(custom_title) && show_title) custom_title else if(show_title) paste0(fish_id, " • ", watershed) else NULL
    
    p <- ggplot(fish_ts_df, aes(x = time_point)) +
      geom_point(aes(y = sr_ratio_raw), color = "grey70", size = 1.0, alpha = 0.6) +
      geom_line(aes(y = sr_ratio_gam), color = "grey40", alpha = 0.9, size = 1.0) +
      geom_point(aes(y = sr_ratio_gam, color = color_var), size = 1.8, alpha = 1.0, stroke = 0) +
      scale_color_viridis_c(
        name = legend_name,
        option = "plasma",
        begin = 0.9,
        end = 0.1,
        direction = -1,
        limits = color_limits,
        guide = if(show_legend) {
          guide_colorbar(barwidth = 8, barheight = 0.6, title.position = "top",
                         title.hjust = 0.5, frame.colour = "grey70", frame.linewidth = 0.3)
        } else {
          "none"
        }
      ) +
      scale_x_continuous(breaks = pretty_breaks(n = 5), expand = expansion(mult = 0.01)) +
      scale_y_continuous(limits = c(0.7065, 0.713), breaks = pretty_breaks(n = 4),
                         expand = expansion(mult = 0.01), labels = label_number(accuracy = 0.001)) +
      labs(
        title = plot_title,
        x = if(pc_num == 3) "Time Point" else NULL,
        y = if(show_y_title) expression(paste(""^87, "Sr/", ""^86, "Sr")) else NULL
      ) +
      theme_minimal(base_size = 10) +
      theme(
        plot.title = element_text(size = 11, face = "bold", hjust = 0.5, color = "grey15", margin = margin(b = 8)),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        axis.title.y = element_text(size = 10, face = "bold", color = "grey20", margin = margin(r = 6)),
        axis.title.x = element_text(size = 10, face = "bold", color = "grey20", margin = margin(t = 6)),
        axis.text = element_text(size = 9, color = "grey30"),
        axis.line = element_line(color = "grey60", size = 0.4),
        axis.ticks = element_line(color = "grey60", size = 0.3),
        axis.ticks.length = unit(2, "pt"),
        panel.grid.major = element_line(color = "grey90", size = 0.25),
        panel.grid.minor = element_blank(),
        legend.position = if(show_legend) "bottom" else "none",
        legend.title = element_text(size = 9, face = "bold", color = "grey20"),
        legend.text = element_text(size = 8, color = "grey30"),
        legend.key = element_blank(),
        legend.margin = margin(t = 6),
        legend.box.margin = margin(t = 4),
        legend.justification = if(is_middle) "center" else "center",
        plot.margin = margin(6, 8, 6, 8),
        panel.spacing = unit(4, "pt")
      )
    
    return(p)
  }
  
  # Create panels for each available fish
  specific_panels <- list()
  
  for(fish_idx in 1:nrow(available_specific_fish)) {
    fish_id <- available_specific_fish$Fish_id[fish_idx]
    
    # Create the three PC panels for this fish
    # Only show title in the first row (PC1)
    specific_panels[[paste0("pc1_fish", fish_idx)]] <- create_fish_panel_custom(
      fish_id, 1, 
      show_legend = (fish_idx == 2), # Show legend on second fish
      show_y_title = (fish_idx == 1), # Show y-title on first fish
      is_middle = (fish_idx == 2),
      custom_title = fish_labels[[fish_id]],  # Use custom label
      show_title = TRUE  # Show title only in first row
    )
    
    specific_panels[[paste0("pc2_fish", fish_idx)]] <- create_fish_panel_custom(
      fish_id, 2, 
      show_legend = (fish_idx == 2), # Show legend on second fish
      show_y_title = (fish_idx == 1), # Show y-title on first fish
      is_middle = (fish_idx == 2),
      custom_title = fish_labels[[fish_id]],
      show_title = FALSE  # No title in subsequent rows
    )
    
    specific_panels[[paste0("pc3_fish", fish_idx)]] <- create_fish_panel_custom(
      fish_id, 3, 
      show_legend = (fish_idx == 2), # Show legend on second fish
      show_y_title = (fish_idx == 1), # Show y-title on first fish
      is_middle = (fish_idx == 2),
      custom_title = fish_labels[[fish_id]],
      show_title = FALSE  # No title in subsequent rows
    )
  }
  
  # Create the specific combined figure based on number of available fish
  if(nrow(available_specific_fish) == 4) {
    # All four fish available - create 3x4 grid (3 PCs x 4 fish)
    specific_combined_figure <- (specific_panels$pc1_fish1 | specific_panels$pc1_fish2 | specific_panels$pc1_fish3 | specific_panels$pc1_fish4) /
      (specific_panels$pc2_fish1 | specific_panels$pc2_fish2 | specific_panels$pc2_fish3 | specific_panels$pc2_fish4) /
      (specific_panels$pc3_fish1 | specific_panels$pc3_fish2 | specific_panels$pc3_fish3 | specific_panels$pc3_fish4) +
      plot_layout(heights = c(1, 1, 1))
    
    figure_width <- 16
    figure_height <- 12
    
  } else if(nrow(available_specific_fish) == 3) {
    # Three fish available - create 3x3 grid
    specific_combined_figure <- (specific_panels$pc1_fish1 | specific_panels$pc1_fish2 | specific_panels$pc1_fish3) /
      (specific_panels$pc2_fish1 | specific_panels$pc2_fish2 | specific_panels$pc2_fish3) /
      (specific_panels$pc3_fish1 | specific_panels$pc3_fish2 | specific_panels$pc3_fish3) +
      plot_layout(heights = c(1, 1, 1))
    
    figure_width <- 12
    figure_height <- 12
    
  } else if(nrow(available_specific_fish) == 2) {
    # Two fish available - create 3x2 grid
    specific_combined_figure <- (specific_panels$pc1_fish1 | specific_panels$pc1_fish2) /
      (specific_panels$pc2_fish1 | specific_panels$pc2_fish2) /
      (specific_panels$pc3_fish1 | specific_panels$pc3_fish2) +
      plot_layout(heights = c(1, 1, 1))
    
    figure_width <- 8
    figure_height <- 12
    
  } else {
    # One fish available - create 3x1 grid
    specific_combined_figure <- (specific_panels$pc1_fish1) /
      (specific_panels$pc2_fish1) /
      (specific_panels$pc3_fish1) +
      plot_layout(heights = c(1, 1, 1))
    
    figure_width <- 4
    figure_height <- 12
  }
  
  # Save the specific combined figure
  specific_combined_filename <- "Four_Panel_PCA_Loadings_Comparison.pdf"
  specific_combined_filepath <- file.path(output_dir, specific_combined_filename)
  
  ggsave(specific_combined_filepath, specific_combined_figure, 
         width = figure_width, height = figure_height,
         device = cairo_pdf,
         dpi = 300,
         units = "in")
  
  cat("Saved four-panel figure:", specific_combined_filename, "\n")
  
} else {
  cat("Warning: No specific fish found in same NO dataset\n")
}

# =============================================================================
# CREATE ADDITIONAL 2D PCA PLOTS
# =============================================================================

cat("\nCreating additional 2D PCA plots for", same_no_range$name, "dataset...\n")

same_no_additional_results <- create_additional_pca_plots(
  gam_data = gam_data_same_no,
  dataset_name = same_no_range$name,
  output_directory = output_dir
)

cat("\nAnalysis complete!\n")