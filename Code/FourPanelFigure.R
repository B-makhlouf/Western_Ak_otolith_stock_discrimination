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

if(nrow(available_fish) >= 1) {
  
  # Use PCA results from same_no_results
  pca_result_combined <- same_no_results$pca_result
  var_explained_combined <- same_no_results$var_explained
  loadings_df_combined <- same_no_results$loadings_data
  
  # Calculate global color scale limits for consistency across panels
  pc1_range <- range(loadings_df_combined$PC1_abs, na.rm = TRUE)
  pc2_range <- range(loadings_df_combined$PC2_abs, na.rm = TRUE)
  pc3_range <- range(loadings_df_combined$PC3_abs, na.rm = TRUE)
  
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
      PC3_abs = loadings_df_combined$PC3_abs
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
        x = if(pc_num == 3) "" else NULL,
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
        legend.title = element_blank(),
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
  
  # Custom version of create_fish_panel for the four-individual figure with custom titles
  create_fish_panel_custom <- function(fish_id, pc_num, show_legend = FALSE, show_y_title = FALSE, 
                                       is_middle = FALSE, custom_title = NULL, show_title = TRUE) {
    
    # Get fish data
    fish_data_gam <- gam_data_same_no %>% filter(Fish_id == fish_id)
    fish_data_raw <- raw_data_same_no %>% filter(Fish_id == fish_id)
    
    if(nrow(fish_data_gam) == 0 || nrow(fish_data_raw) == 0) {
      return(NULL)
    }
    
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
      PC3_abs = loadings_df_combined$PC3_abs
    )
    
    # Select the appropriate PC and color range
    if(pc_num == 1) {
      color_var <- fish_ts_df$PC1_abs
      color_limits <- pc1_range
    } else if(pc_num == 2) {
      color_var <- fish_ts_df$PC2_abs
      color_limits <- pc2_range
    } else {  # pc_num == 3
      color_var <- fish_ts_df$PC3_abs
      color_limits <- pc3_range
    }
    
    # Use custom title if provided and show_title is TRUE, otherwise no title
    plot_title <- if(show_title && !is.null(custom_title)) custom_title else NULL
    
    # Create the plot
    p <- ggplot(fish_ts_df, aes(x = time_point)) +
      # Raw data points
      geom_point(aes(y = sr_ratio_raw), color = "grey70", size = 1.5, alpha = 0.6) +
      # GAM smoothed line
      geom_line(aes(y = sr_ratio_gam), color = "grey40", alpha = 0.9, size = 1.2) +
      # GAM points colored by loadings
      geom_point(aes(y = sr_ratio_gam, color = color_var), size = 2.5, alpha = 1.0, stroke = 0) +
      # Plasma color scale - NO LEGEND TITLE
      scale_color_viridis_c(
        name = NULL,  # Remove legend title
        option = "plasma",
        begin = 0.9,
        end = 0.1,
        direction = -1,
        limits = color_limits,
        guide = if(show_legend) {
          guide_colorbar(
            barwidth = 10,
            barheight = 0.8,
            title.position = "top",
            title.hjust = 0.5,
            frame.colour = "grey70",
            frame.linewidth = 0.4
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
        title = plot_title,
        x = if(pc_num == 3) "" else NULL,
        y = if(show_y_title) expression(paste(""^87, "Sr/", ""^86, "Sr")) else NULL
      ) +
      # Clean theme with larger text for publication
      theme_minimal(base_size = 14) +
      theme(
        plot.title = if(!is.null(plot_title)) {
          element_text(size = 16, face = "bold", hjust = 0.5, 
                       color = "grey15", margin = margin(b = 10))
        } else {
          element_blank()
        },
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        
        # Axes - larger text for publication
        axis.title.y = element_text(size = 16, face = "bold", color = "grey20", 
                                    margin = margin(r = 8)),
        axis.title.x = element_text(size = 16, face = "bold", color = "grey20", 
                                    margin = margin(t = 8)),
        axis.text = element_text(size = 14, color = "grey30"),
        axis.line = element_line(color = "grey60", size = 0.5),
        axis.ticks = element_line(color = "grey60", size = 0.4),
        axis.ticks.length = unit(3, "pt"),
        
        # Grid
        panel.grid.major = element_line(color = "grey90", size = 0.3),
        panel.grid.minor = element_blank(),
        
        # Legend - NO TITLE
        legend.position = if(show_legend) "bottom" else "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 12, color = "grey30"),
        legend.key = element_blank(),
        legend.margin = margin(t = 8),
        legend.box.margin = margin(t = 6),
        legend.justification = if(is_middle) "center" else "center",
        
        # Spacing
        plot.margin = margin(8, 10, 8, 10),
        panel.spacing = unit(6, "pt")
      )
    
    return(p)
  }
  
  # Create panels based on number of available fish
  panels <- list()
  
  if(nrow(available_fish) >= 3) {
    # Create all panels for three fish (3x3 grid)
    for(fish_idx in 1:3) {
      # PC1 panels
      panels[[paste0("pc1_fish", fish_idx)]] <- create_fish_panel(
        target_fish_ids[fish_idx], 1, 
        show_legend = (fish_idx == 2), 
        show_y_title = (fish_idx == 1), 
        is_middle = (fish_idx == 2)
      )
      
      # PC2 panels
      panels[[paste0("pc2_fish", fish_idx)]] <- create_fish_panel(
        target_fish_ids[fish_idx], 2, 
        show_legend = (fish_idx == 2), 
        show_y_title = (fish_idx == 1), 
        is_middle = (fish_idx == 2)
      )
      
      # PC3 panels
      panels[[paste0("pc3_fish", fish_idx)]] <- create_fish_panel(
        target_fish_ids[fish_idx], 3, 
        show_legend = (fish_idx == 2), 
        show_y_title = (fish_idx == 1), 
        is_middle = (fish_idx == 2)
      )
    }
    
    # Combine using patchwork (3 rows x 3 columns)
    combined_figure <- (panels$pc1_fish1 | panels$pc1_fish2 | panels$pc1_fish3) /
      (panels$pc2_fish1 | panels$pc2_fish2 | panels$pc2_fish3) /
      (panels$pc3_fish1 | panels$pc3_fish2 | panels$pc3_fish3) +
      plot_layout(heights = c(1, 1, 1)) +
      plot_annotation(
        title = paste0("Comparison among three individuals with the same natal origin (", same_no_range$min, "-", same_no_range$max, ")"),
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
    
  } else if(nrow(available_fish) == 2) {
    # Create panels for two fish (3x2 grid)
    for(fish_idx in 1:2) {
      fish_id <- available_fish$Fish_id[fish_idx]
      
      panels[[paste0("pc1_fish", fish_idx)]] <- create_fish_panel(
        fish_id, 1, 
        show_legend = (fish_idx == 2), 
        show_y_title = (fish_idx == 1), 
        is_middle = (fish_idx == 2)
      )
      
      panels[[paste0("pc2_fish", fish_idx)]] <- create_fish_panel(
        fish_id, 2, 
        show_legend = (fish_idx == 2), 
        show_y_title = (fish_idx == 1), 
        is_middle = (fish_idx == 2)
      )
      
      panels[[paste0("pc3_fish", fish_idx)]] <- create_fish_panel(
        fish_id, 3, 
        show_legend = (fish_idx == 2), 
        show_y_title = (fish_idx == 1), 
        is_middle = (fish_idx == 2)
      )
    }
    
    combined_figure <- (panels$pc1_fish1 | panels$pc1_fish2) /
      (panels$pc2_fish1 | panels$pc2_fish2) /
      (panels$pc3_fish1 | panels$pc3_fish2) +
      plot_layout(heights = c(1, 1, 1)) +
      plot_annotation(
        title = paste0("Comparison among two individuals with the same natal origin (", same_no_range$min, "-", same_no_range$max, ")"),
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
    # Create panels for one fish (3x1 grid)
    fish_id <- available_fish$Fish_id[1]
    
    panels$pc1_fish1 <- create_fish_panel(fish_id, 1, show_legend = TRUE, show_y_title = TRUE)
    panels$pc2_fish1 <- create_fish_panel(fish_id, 2, show_legend = TRUE, show_y_title = TRUE)
    panels$pc3_fish1 <- create_fish_panel(fish_id, 3, show_legend = TRUE, show_y_title = TRUE)
    
    combined_figure <- (panels$pc1_fish1) /
      (panels$pc2_fish1) /
      (panels$pc3_fish1) +
      plot_layout(heights = c(1, 1, 1)) +
      plot_annotation(
        title = paste0("Individual with the same natal origin (", same_no_range$min, "-", same_no_range$max, ")"),
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
  
  # Save the combined figure
  combined_filename <- paste0("Combined_", same_no_range$name, "_Comparison.pdf")
  combined_filepath <- file.path(same_no_output_dir, combined_filename)
  
  ggsave(combined_filepath, combined_figure, 
         width = figure_width, height = figure_height,
         device = cairo_pdf,
         dpi = 300,
         units = "in")
  
  cat("Saved combined figure:", combined_filename, "\n")
  
} else {
  cat("Warning: No target fish found in same NO dataset\n")
}

# =============================================================================
# CREATE SPECIFIC FOUR-PANEL FIGURE WITH EXACT FISH IDs
# =============================================================================

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

cat("\nSpecific fish found for requested figure:\n")
print(available_specific_fish)

if(nrow(available_specific_fish) >= 1) {
  
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
    # NO MAIN TITLE OR SUBTITLE for publication-friendly version
    specific_combined_figure <- (specific_panels$pc1_fish1 | specific_panels$pc1_fish2 | specific_panels$pc1_fish3 | specific_panels$pc1_fish4) /
      (specific_panels$pc2_fish1 | specific_panels$pc2_fish2 | specific_panels$pc2_fish3 | specific_panels$pc2_fish4) /
      (specific_panels$pc3_fish1 | specific_panels$pc3_fish2 | specific_panels$pc3_fish3 | specific_panels$pc3_fish4) +
      plot_layout(heights = c(1, 1, 1))
    
    figure_width <- 16  # Increased width for 4 columns
    figure_height <- 12  # Keep same height for 3 rows
    
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
  specific_combined_filename <- paste0("Specific_Four_Individuals_", same_no_range$name, "_Comparison.pdf")
  specific_combined_filepath <- file.path(same_no_output_dir, specific_combined_filename)
  
  ggsave(specific_combined_filepath, specific_combined_figure, 
         width = figure_width, height = figure_height,
         device = cairo_pdf,
         dpi = 300,
         units = "in")
  
  cat("Saved specific four-individual figure:", specific_combined_filename, "\n")
  
} else {
  cat("Warning: No specific fish found in same NO dataset\n")
}