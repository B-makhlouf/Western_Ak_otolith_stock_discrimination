

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



### This function displays the feature importance along the timeseries, either as a "line" or a " bar graph" 

plot_pca_loadings <- function(PCA_raw, plot_type = "line") {
  # Prepare the loadings data
  loadings <- as.data.frame(PCA_raw$rotation)
  loadings$Feature <- rownames(loadings)
  
  # Create a data frame from the matrix
  feature_matrix <- matrix(1, nrow = 5, ncol = length(loadings$PC1))
  feature_matrix[1, ] <- abs(loadings$PC1)
  feature_matrix[2, ] <- abs(loadings$PC2)
  feature_matrix[3, ] <- abs(loadings$PC3)
  feature_matrix[4, ] <- abs(loadings$PC4)
  feature_matrix[5, ] <- abs(loadings$PC5)
  
  # Convert to long format for ggplot
  plot_data <- data.frame(
    Index = rep(1:length(loadings$PC1), times = 5),
    FeatureImportance = c(
      feature_matrix[1, ],
      feature_matrix[2, ],
      feature_matrix[3, ],
      feature_matrix[4, ],
      feature_matrix[5, ]
    ),
    Component = rep(c("PC1", "PC2", "PC3", "PC4", "PC5"), each = length(loadings$PC1)),
    Y = rep(1, (5 * length(loadings$PC1))) # Constant Y value for straight line
  )
  
  # Line plot
  if (plot_type == "line") {
    feature_plot <- ggplot(plot_data, aes(x = Index, y = Y, color = FeatureImportance)) +
      geom_point(size = 3) +
      scale_color_viridis(option = "plasma", direction = -1) +
      theme_grey() +
      labs(
        title = "Timeseries Loadings onto PCA variance",
        x = "Index",
        y = NULL, # Remove y-axis label
        color = "Loading (Abs. value)"
      ) +
      facet_wrap(~Component, nrow = 5) + # Separate panels for PC1 to PC5
      theme(
        axis.line.y = element_blank(), # Remove y-axis line
        axis.ticks.y = element_blank(), # Remove y-axis ticks
        axis.text.y = element_blank(), # Remove y-axis text
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8)
      )
    return(feature_plot)
  }
  
  # Bar plot
  if (plot_type == "bar") {
    feature_bar_plot <- ggplot(plot_data, aes(x = Index, y = FeatureImportance, color = FeatureImportance)) +
      geom_bar(stat = "identity", position = "identity", width = 1, alpha = 0.4) + 
      scale_color_viridis(option = "plasma", direction = -1) +
      theme_grey() +
      labs(
        title = "Timeseries Loadings onto PCA variance",
        x = "Index",
        y = NULL, # Remove y-axis label
        fill = "Loading (Abs. value)"
      ) +
      facet_wrap(~Component, nrow = 5) + # Separate panels for PC1 to PC5
      theme(
        axis.line.y = element_blank(), # Remove y-axis line
        axis.ticks.y = element_blank(), # Remove y-axis ticks
        axis.text.y = element_blank(), # Remove y-axis text
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8)
      )
    return(feature_bar_plot)
  }
  
  # Default: Return an error if an invalid plot type is specified
  stop("Invalid plot type. Please use 'line' or 'bar'.")
}
