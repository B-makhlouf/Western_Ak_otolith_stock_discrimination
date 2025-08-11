# ModelVisualization.R
# Professional PCA visualization for GAM Sr87/86 data
# Creates both TOTAL_PCA (unfiltered) and OVERLAP_PCA (filtered) versions

# =============================================================================
# SETUP
# =============================================================================
library(tidyverse)
library(ggplot2)
library(viridis)
library(cowplot)
library(plotly)

# Paths and colors
data_path <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/data/LA_Data/Preprocessed_ts_matrices/NatalToMarine_Processed_GAM.csv"
output_dir <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/Figures/PCA"
watershed_colors <- c("Kusko" = "firebrick", "Nush" = "darkgreen", "Yukon" = "dodgerblue")

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# LOAD AND PREPARE DATA
# =============================================================================
gam_data_full <- read.csv(data_path) %>%
  mutate(Watershed = as.factor(Watershed))

# Create filtered version
gam_data_filtered <- gam_data_full %>%
  filter(Natal_Iso <= 0.715)

metadata_cols <- c("Fish_id", "Watershed", "Natal_Iso", "Year", "Natal_Start", 
                   "Marine_Start", "Marine_End", "Original_Data_Points", 
                   "Interpolated_Points", "Micron_Size")
feature_cols <- grep("^X", names(gam_data_full), value = TRUE)

cat("TOTAL dataset:", nrow(gam_data_full), "samples with", length(feature_cols), "features\n")
cat("OVERLAP dataset:", nrow(gam_data_filtered), "samples with", length(feature_cols), "features\n")

# =============================================================================
# FUNCTION TO CREATE PCA ANALYSIS AND PLOTS
# =============================================================================
create_pca_analysis <- function(data, dataset_name, title_suffix) {
  
  # PCA Analysis
  pca_result <- prcomp(data[, feature_cols], scale. = TRUE)
  pca_scores <- as.data.frame(pca_result$x[, 1:3])
  pca_data <- bind_cols(pca_scores, data[, metadata_cols])
  
  var_explained <- (pca_result$sdev^2 / sum(pca_result$sdev^2))[1:3]
  cat("\n", dataset_name, "PCA:\n")
  cat("PC1:", round(var_explained[1] * 100, 1), "% | PC2:", round(var_explained[2] * 100, 1), "% | PC3:", round(var_explained[3] * 100, 1), "%\n")
  
  # =============================================================================
  # 2D PCA PLOT
  # =============================================================================
  pca_plot_2d <- ggplot(pca_data, aes(x = PC1, y = PC2, color = Watershed, fill = Watershed)) +
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
      subtitle = paste0("GAM-Smoothed Sr 87/86 Profiles by Watershed", title_suffix),
      x = sprintf("PC1 (%s%%)", round(var_explained[1] * 100, 1)),
      y = sprintf("PC2 (%s%%)", round(var_explained[2] * 100, 1)),
      color = "Watershed",
      caption = paste0("95% confidence ellipses | n = ", nrow(pca_data), " samples")
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
  
  # =============================================================================
  # 3D PCA PLOT
  # =============================================================================
  
  # Create hover text with fish information
  pca_data$hover_text <- paste(
    "Fish ID:", pca_data$Fish_id,
    "<br>Watershed:", pca_data$Watershed,
    "<br>Year:", pca_data$Year,
    "<br>Natal Iso:", round(pca_data$Natal_Iso, 4),
    "<br>PC1:", round(pca_data$PC1, 2),
    "<br>PC2:", round(pca_data$PC2, 2),
    "<br>PC3:", round(pca_data$PC3, 2)
  )
  
  # Create the 3D scatter plot
  pca_plot_3d <- plot_ly(
    pca_data,
    x = ~PC1, 
    y = ~PC2, 
    z = ~PC3,
    color = ~Watershed,
    colors = watershed_colors,
    type = "scatter3d",
    mode = "markers",
    marker = list(
      size = 2,
      opacity = 0.5,
      line = list(width = 0)
    ),
    text = ~hover_text,
    hovertemplate = "%{text}<extra></extra>"
  ) %>%
    layout(
      title = list(
        text = paste0(
          "<b>Interactive 3D PCA Analysis - ", dataset_name, "</b><br>",
          "<sub>GAM-Smoothed Sr 87/86 Profiles by Watershed", title_suffix, "</sub>"
        ),
        font = list(size = 18, color = "rgb(50,50,50)"),
        x = 0.5
      ),
      scene = list(
        xaxis = list(
          title = paste0("PC1 (", round(var_explained[1] * 100, 1), "%)"),
          titlefont = list(size = 14, color = "rgb(70,70,70)"),
          tickfont = list(size = 12, color = "rgb(100,100,100)"),
          gridcolor = "rgb(230,230,230)",
          zerolinecolor = "rgb(200,200,200)",
          showspikes = FALSE
        ),
        yaxis = list(
          title = paste0("PC2 (", round(var_explained[2] * 100, 1), "%)"),
          titlefont = list(size = 14, color = "rgb(70,70,70)"),
          tickfont = list(size = 12, color = "rgb(100,100,100)"),
          gridcolor = "rgb(230,230,230)",
          zerolinecolor = "rgb(200,200,200)",
          showspikes = FALSE
        ),
        zaxis = list(
          title = paste0("PC3 (", round(var_explained[3] * 100, 1), "%)"),
          titlefont = list(size = 14, color = "rgb(70,70,70)"),
          tickfont = list(size = 12, color = "rgb(100,100,100)"),
          gridcolor = "rgb(230,230,230)",
          zerolinecolor = "rgb(200,200,200)",
          showspikes = FALSE
        ),
        bgcolor = "rgb(248,248,248)",
        camera = list(
          eye = list(x = 1.5, y = 1.5, z = 1.5),
          center = list(x = 0, y = 0, z = 0)
        ),
        aspectmode = "cube"
      ),
      legend = list(
        title = list(text = "<b>Watershed</b>", font = list(size = 14)),
        font = list(size = 12),
        orientation = "v",
        x = 0.02,
        y = 0.98,
        bgcolor = "rgba(255,255,255,0.8)",
        bordercolor = "rgba(0,0,0,0.2)",
        borderwidth = 1
      ),
      paper_bgcolor = "white",
      plot_bgcolor = "white",
      margin = list(l = 80, r = 80, t = 100, b = 80),
      annotations = list(
        list(
          text = paste0(
            "Total variance explained by PC1-3: ", 
            round(sum(var_explained) * 100, 1), "% | n = ", nrow(pca_data), " samples<br>",
            "Drag to rotate • Scroll to zoom • Double-click to reset"
          ),
          showarrow = FALSE,
          x = 0.5,
          y = -0.1,
          xref = "paper",
          yref = "paper",
          font = list(size = 11, color = "rgb(120,120,120)"),
          align = "center"
        )
      )
    ) %>%
    config(
      displayModeBar = TRUE,
      modeBarButtonsToRemove = c("pan2d", "select2d", "lasso2d", "autoScale2d"),
      displaylogo = FALSE,
      toImageButtonOptions = list(
        format = "png",
        filename = paste0("3D_PCA_", dataset_name),
        height = 800,
        width = 1000,
        scale = 2
      )
    )
  
  # Display and save plots
  print(pca_plot_2d)
  print(pca_plot_3d)
  
  ggsave(file.path(output_dir, paste0(dataset_name, "_PCA_2D.png")), 
         pca_plot_2d, width = 12, height = 10, dpi = 300)
  
  cat("Saved:", paste0(dataset_name, "_PCA_2D.png"), "\n")
  
  return(list(
    pca_result = pca_result,
    pca_data = pca_data,
    plot_2d = pca_plot_2d,
    plot_3d = pca_plot_3d,
    var_explained = var_explained
  ))
}

# =============================================================================
# CREATE BOTH VERSIONS
# =============================================================================

# TOTAL PCA (unfiltered)
cat("Creating TOTAL PCA (unfiltered dataset)...\n")
total_pca <- create_pca_analysis(
  data = gam_data_full,
  dataset_name = "TOTAL",
  title_suffix = ""
)

# OVERLAP PCA (filtered: Natal_Iso <= 0.715)
cat("\nCreating OVERLAP PCA (filtered dataset: Natal_Iso <= 0.715)...\n")
overlap_pca <- create_pca_analysis(
  data = gam_data_filtered,
  dataset_name = "OVERLAP",
  title_suffix = " (Natal_Iso ≤ 0.715)"
)

# =============================================================================
# SUMMARY
# =============================================================================
cat("\n", "="*60, "\n")
cat("ANALYSIS COMPLETE - FILES CREATED:\n")
cat("="*60, "\n")
cat("2D Plots:\n")
cat("  - TOTAL_PCA_2D.png\n")
cat("  - OVERLAP_PCA_2D.png\n")
cat("\nDataset Comparison:\n")
cat("  - TOTAL: ", nrow(gam_data_full), " samples\n")
cat("  - OVERLAP: ", nrow(gam_data_filtered), " samples\n")
cat("  - Difference: ", nrow(gam_data_full) - nrow(gam_data_filtered), " samples removed by filtering\n")
cat("="*60, "\n")