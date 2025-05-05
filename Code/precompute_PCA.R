# precompute_pca.R - Pre-calculates PCA for all data types
# Run this script before using the Shiny app to improve reactivity

library(tidyverse)
library(here)

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

# Create directory for precomputed data
output_dir <- here("data/pca_precomputed")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Precompute PCA for all data types
pca_results <- list()
for (data_type in data_types) {
  pca_results[[data_type]] <- precompute_pca(data_type)
  
  # Save results if computation was successful
  if (!is.null(pca_results[[data_type]])) {
    saveRDS(
      pca_results[[data_type]], 
      file.path(output_dir, paste0("pca_", tolower(data_type), ".rds"))
    )
  }
}

# Save the full results object (optional)
saveRDS(pca_results, file.path(output_dir, "all_pca_results.rds"))

message("PCA precomputation complete. Results saved to: ", output_dir)
