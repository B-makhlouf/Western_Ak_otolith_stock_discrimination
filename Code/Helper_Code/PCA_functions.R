

### This function puts the format with all of the metadata 

run_pca <- function(selected_data, selected_metadata) {
  # Run PCA
  pca_scores <- as.data.frame(PCA_raw$x)
  
  # Combine PCA scores with metadata
  pca_results <- tibble(
    PC1 = pca_scores$PC1,
    PC2 = pca_scores$PC2,
    PC3 = pca_scores$PC3,
    PC4 = pca_scores$PC4,
    Fish_id = selected_metadata$Fish_id,
    Watershed = selected_metadata$Watershed, 
    Natal_iso = selected_metadata$Natal_Iso,
    Year = selected_metadata$Year
  )
  
  # Return both the PCA results and the raw PCA object
  return(pca_results)
}





#### This function makes a figure of natal origin vs watershed plotted by PCA score 

pca_natal_plot <- function(PCA_full, pca_x = 1, pca_y = 2) {
  # Convert numeric input to column names
  pca_x_col <- paste0("PC", pca_x)
  pca_y_col <- paste0("PC", pca_y)
  
  # filter within -100 and 100 
  PCA_full <- PCA_full %>%
    filter(!!sym(pca_x_col) > -100 & !!sym(pca_x_col) < 100) %>%
    filter(!!sym(pca_y_col) > -100 & !!sym(pca_y_col) < 100)
  
  
  # PCA plot by Watershed
  pca_plot <- ggplot(PCA_full, aes_string(x = pca_x_col, y = pca_y_col, color = "Watershed")) +
    geom_point(size = 2, alpha = .2) +
    theme_classic() +
    labs(title = "PCA of Iso Values by Watershed",
         x = pca_x_col,
         y = pca_y_col) +
    theme(legend.title = element_blank())
  
  # PCA plot by Natal Iso
  pca_plot_natal_iso <- ggplot(PCA_full, aes_string(x = pca_x_col, y = pca_y_col, color = "Natal_iso")) +
    geom_point(size = 2, alpha = .9) +
    theme_classic() +
    labs(title = "PCA of Iso Values by Natal Iso",
         x = pca_x_col,
         y = pca_y_col) +
    scale_color_viridis_c(option = "C") +
    theme(legend.title = element_blank())
  
  # Combine both plots using cowplot
  combined_plot <- cowplot::plot_grid(pca_plot, pca_plot_natal_iso, labels = c("A", "B"))
  
  return(combined_plot)
}


############ PCA PLOT 

pca_plot <- function(PCA_full, pca_x = 1, pca_y = 2) {
  # Convert numeric input to column names
  pca_x_col <- paste0("PC", pca_x)
  pca_y_col <- paste0("PC", pca_y)
  
  # filter within -100 and 100 
  PCA_full <- PCA_full %>%
    filter(!!sym(pca_x_col) > -100 & !!sym(pca_x_col) < 100) %>%
    filter(!!sym(pca_y_col) > -100 & !!sym(pca_y_col) < 100)
  
  
  # PCA plot by Watershed
  pca_plot <- ggplot(PCA_full, aes_string(x = pca_x_col, y = pca_y_col, color = "Watershed")) +
    geom_point(size = 2, alpha = .5) +
    theme_classic() +
    labs(title = "PCA of Iso Values by Watershed",
         x = pca_x_col,
         y = pca_y_col) +
    theme(legend.title = element_blank())
  
  # PCA plot by Natal Iso
  pca_plot_natal_iso <- ggplot(PCA_full, aes_string(x = pca_x_col, y = pca_y_col, color = "Natal_iso")) +
    geom_point(size = 2, alpha = .9) +
    theme_classic() +
    labs(title = "PCA of Iso Values by Natal Iso",
         x = pca_x_col,
         y = pca_y_col) +
    scale_color_viridis_c(option = "C") +
    theme(legend.title = element_blank())
  
  
  return(pca_plot)
}



### This function displays the feature importance along the timeseries, either as a "line" or a " bar graph" 

plot_pca_loadings <- function(PCA_raw, num_components = 3, color_scale = "plasma") {
  # Extract the loadings data
  loadings <- as.data.frame(PCA_raw$rotation)
  
  # Limit to requested number of components
  pc_cols <- paste0("PC", 1:num_components)
  loadings <- loadings[, pc_cols, drop = FALSE]
  
  # Convert to long format for ggplot
  loadings_long <- loadings %>%
    mutate(Index = 1:nrow(loadings)) %>%
    pivot_longer(
      cols = starts_with("PC"),
      names_to = "Component",
      values_to = "Loading"
    ) %>%
    # Take absolute value for visualization
    mutate(Abs_Loading = abs(Loading))
  
  # Set the component as a factor with correct order
  loadings_long$Component <- factor(loadings_long$Component, levels = pc_cols)
  
  # Create the visualization
  feature_plot <- ggplot(loadings_long, aes(x = Index, y = 1, color = Abs_Loading)) +
    geom_point(size = 3) +
    scale_color_viridis_c(option = color_scale, direction = -1, name = "Loading\n(Abs. value)") +
    facet_wrap(~ Component, ncol = 1, strip.position = "right") +
    labs(
      title = "Timeseries Loadings onto PCA Components",
      x = "Index",
      y = NULL
    ) +
    theme_minimal() +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      strip.text = element_text(size = 12, face = "bold"),
      plot.title = element_text(hjust = 0.5, size = 14)
    )
  
  return(feature_plot)
}

plot_pca_loadings_line <- function(PCA_raw, num_components = 5, color_palette = viridis::plasma(5)) {
  # Extract the loadings data
  loadings <- as.data.frame(PCA_raw$rotation)
  
  # Limit to requested number of components
  pc_cols <- paste0("PC", 1:num_components)
  pc_cols <- pc_cols[pc_cols %in% colnames(loadings)]
  loadings <- loadings[, pc_cols, drop = FALSE]
  
  # Convert to long format for ggplot
  loadings_long <- loadings %>%
    mutate(Index = 1:nrow(loadings)) %>%
    pivot_longer(
      cols = starts_with("PC"),
      names_to = "Component",
      values_to = "Loading"
    ) %>%
    # Take absolute value for visualization
    mutate(Abs_Loading = abs(Loading))
  
  # Set the component as a factor with correct order
  loadings_long$Component <- factor(loadings_long$Component, levels = pc_cols)
  
  # Create the visualization
  feature_plot <- ggplot(loadings_long, aes(x = Index, y = Abs_Loading, color = Component)) +
    geom_line(size = 1.5, color = "black") +
    scale_color_manual(values = color_palette[1:length(pc_cols)]) +
    facet_wrap(~ Component, ncol = 1) +
    labs(
      title = "Timeseries Loadings onto PCA Components",
      x = "Index",
      y = "Absolute Loading Value"
    ) +
    theme_minimal() +
    theme(
      strip.text = element_text(size = 12, face = "bold"),
      plot.title = element_text(hjust = 0.5, size = 14),
      legend.position = "none"
    )
  
  return(feature_plot)
}

## Scree plot of PCA 
scree_plot <- function(pca_result) {
  scree_values <- PCA_raw$sdev^2
  prop_variance <- scree_values / sum(scree_values)
  
  # do the above with ggplot
  scree_df <- data.frame(PC = 1:length(scree_values), 
                         prop_variance = prop_variance)
  
  scree_gg <- ggplot(scree_df[1:10, ], aes(x = PC, y = prop_variance)) +
    geom_point() +
    geom_line() +
    labs(x = "Principal Component", 
         y = "Proportion of Variance Explained",
         title = "Scree Plot (First 10 Components)") +
    theme_grey()
  
  return(scree_gg)
}

