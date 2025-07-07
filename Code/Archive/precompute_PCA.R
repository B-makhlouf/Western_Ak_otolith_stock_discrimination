# precompute_pca.R - Pre-calculates PCA for all data types and generates loadings plots
# Run this script before using the Shiny app to improve reactivity

library(tidyverse)
library(here)
library(ggplot2)
library(viridis)

# Add our new contour plot function
plot_pca_loadings_contour <- function(PCA_raw, num_components = 5, color_scale = "plasma") {
  # Extract the loadings data
  loadings <- as.data.frame(PCA_raw$rotation)
  
  # Limit to requested number of components
  pc_cols <- paste0("PC", 1:num_components)
  pc_cols <- pc_cols[pc_cols %in% colnames(loadings)]
  loadings <- loadings[, pc_cols, drop = FALSE]
  
  # Get number of data points
  n_points <- nrow(loadings)
  
  # Create a grid for the contour plot
  grid_points <- 100
  index_grid <- seq(1, n_points, length.out = grid_points)
  component_grid <- 1:length(pc_cols)
  
  # Create empty grid for interpolation
  z_grid <- matrix(NA, nrow = grid_points, ncol = length(pc_cols))
  
  # Fill the grid with absolute loading values
  for (j in 1:length(pc_cols)) {
    pc_name <- pc_cols[j]
    values <- abs(loadings[[pc_name]])
    
    # Use approx to interpolate values onto the grid
    z_grid[, j] <- approx(1:n_points, values, index_grid, rule = 2)$y
  }
  
  # Convert to long format for ggplot
  grid_df <- expand.grid(Index = index_grid, Component_Num = 1:length(pc_cols))
  grid_df$Value <- as.vector(z_grid)
  grid_df$Component <- paste0("PC", grid_df$Component_Num)
  grid_df$Component <- factor(grid_df$Component, levels = pc_cols)
  
  # Create the visualization
  feature_plot <- ggplot(grid_df, aes(x = Index, y = 1, fill = Value)) +
    geom_raster(interpolate = TRUE) +
    scale_fill_viridis_c(option = color_scale, direction = -1, name = "Loading\n(Abs. value)") +
    facet_wrap(~ Component, ncol = 1) +
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

# Function to precompute PCA for a given data type
precompute_pca <- function(data_type) {
  message("Processing ", data_type, " data...")
  
  # Construct filename based on data type
  filename <- paste0("preprocessed_", data_type, ".csv")
  
  # Try multiple possible locations
  possible_paths <- c(
    here("data/preprocessed_matrices", filename),
    here("Data/preprocessed_matrices", filename),
    here("Data/Processed/Preprocessed_ts_matrices", filename),
    here("Data/02_Preprocessed_ts_matrices", filename)
  )
  
  # Find first existing file
  filepath <- NULL
  for (path in possible_paths) {
    if (file.exists(path)) {
      filepath <- path
      break
    }
  }
  
  if (is.null(filepath)) {
    warning("Could not find data file for ", data_type, " type.")
    return(NULL)
  }
  
  # Load data
  data <- read.csv(filepath)
  
  # Separate metadata and timeseries data
  metadata_cols <- c("Fish_id", "Watershed", "Natal_Iso", "Year")
  metadata_cols <- metadata_cols[metadata_cols %in% names(data)]
  
  # Get numeric columns for PCA
  numeric_cols <- grep("^X", names(data), value = TRUE)
  
  # Run PCA
  pca <- prcomp(data[, numeric_cols], scale. = TRUE)
  
  # Create PCA data with metadata
  pca_scores <- as.data.frame(pca$x)
  pca_data <- bind_cols(
    pca_scores,
    data[, metadata_cols]
  )
  
  # Calculate explained variance
  explained_var <- pca$sdev^2 / sum(pca$sdev^2)
  cum_var <- cumsum(explained_var)
  
  # Create variance data
  var_data <- data.frame(
    PC = paste0("PC", 1:length(explained_var)),
    Variance = explained_var,
    CumulativeVariance = cum_var
  )
  
  # Return results
  list(
    pca_data = pca_data,
    var_data = var_data,
    loadings = pca$rotation
  )
}

# Define data types
data_types <- c("GAM", "MA", "RAW", "Sr88", "Combined")

# Create directories for precomputed data and plots
output_dir <- here("data/pca_precomputed")
plots_dir <- here("data/pca_plots")

for (dir in c(output_dir, plots_dir)) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }
}

# Precompute PCA for all data types
pca_results <- list()
for (data_type in data_types) {
  pca_results[[data_type]] <- precompute_pca(data_type)
  
  # Save results if computation was successful
  if (!is.null(pca_results[[data_type]])) {
    # Save the PCA results
    saveRDS(
      pca_results[[data_type]], 
      file.path(output_dir, paste0("pca_", tolower(data_type), ".rds"))
    )
    
    # Generate and save loadings plot
    # Create a PCA_raw-like object to work with the plot_pca_loadings_contour function
    PCA_raw <- list(rotation = pca_results[[data_type]]$loadings)
    
    # Generate contour plot
    contour_plot <- plot_pca_loadings_line(PCA_raw)
    ggsave(
      file.path(plots_dir, paste0("pca_loadings_contour_", tolower(data_type), ".png")),
      contour_plot,
      width = 10,
      height = 8,
      dpi = 300
    )
    
    message("  Saved PCA loadings contour plot for ", data_type)
  }
}

# Save the full results object (optional)
saveRDS(pca_results, file.path(output_dir, "all_pca_results.rds"))

message("PCA precomputation complete. Results saved to: ", output_dir)
message("PCA loadings plots saved to: ", plots_dir)
