# Packages
library(tidyverse)
library(here)

# Directories
Nush2014_directory <- here("Data/Intermediate/Cleaned and Trimmed/2014 Nush")
Yukon2015_directory <- here("Data/Intermediate/Cleaned and Trimmed/2015 Yukon")
Kusko2017_directory <- here("Data/Intermediate/Cleaned and Trimmed/2017 Kusko")

# List all files in each directory
Nush2014_files <- list.files(Nush2014_directory, full.names = TRUE)
Yukon2015_files <- list.files(Yukon2015_directory, full.names = TRUE)
Kusko2017_files <- list.files(Kusko2017_directory, full.names = TRUE)

# Function to interpolate data to 1000 points
interpolate_data <- function(data, num_points = 1000) {
  if (sum(!is.na(data$Iso)) < 2) {
    return(rep(NA, num_points)) # Return NA if not enough points for interpolation
  }
  
  data <- data %>%
    mutate(Point = seq(1, num_points, length.out = n()))
  
  interpolated <- tibble(
    Point = seq(1, num_points),
    Iso = approx(data$Point, data$Iso, xout = seq(1, num_points))$y
  )
  
  return(interpolated$Iso)
}

# Function to process all files in a directory
process_directory <- function(files) {
  files %>%
    map_df(~ {
      # Read individual file
      ind_data <- read_csv(.x) %>% select(Iso, Watershed, Fish_id)
      
      # Extract metadata
      id <- ind_data$Fish_id[1]
      watershed <- ind_data$Watershed[1]
      
      # Interpolate Iso
      interpolated <- interpolate_data(ind_data)
      
      tibble(
        Fish_id = id,
        Watershed = watershed,
        Iso = list(interpolated)
      )
    }, .id = "File_Index") # Add file index for debugging
}

# Process all three datasets
Nush2014_results <- process_directory(Nush2014_files)
Yukon2015_results <- process_directory(Yukon2015_files)
Kusko2017_results <- process_directory(Kusko2017_files)

# Combine results into a single tibble
combined_results <- bind_rows(
  Nush2014_results,
  Yukon2015_results,
  Kusko2017_results
)

# Remove rows with all NA in the Iso column
filtered_results <- combined_results %>%
  filter(map_lgl(Iso, ~ !all(is.na(.))))

# Transpose the Iso values to make each individual a row
measurement_array <- do.call(cbind, filtered_results$Iso)

# Transpose the matrix to get individuals as rows
measurement_array <- t(measurement_array)

# Diagnostic: Check for issues
cat("Checking for missing and infinite values in measurement_array...\n")
cat("Any NA values? ", anyNA(measurement_array), "\n")
cat("Any infinite values? ", any(is.infinite(measurement_array)), "\n")

# Handle missing and infinite values
if (anyNA(measurement_array) || any(is.infinite(measurement_array))) {
  measurement_array[is.infinite(measurement_array)] <- NA # Replace infinite values with NA
  measurement_array <- apply(measurement_array, 2, function(x) {
    if (any(is.na(x))) {
      x[is.na(x)] <- mean(x, na.rm = TRUE) # Replace NA with column mean
    }
    return(x)
  })
}

# Check dimensions after cleaning
cat("Dimensions of cleaned measurement_array:", dim(measurement_array), "\n")

# Extract the metadata (ID and Watershed)
ids <- filtered_results$Fish_id
watersheds <- filtered_results$Watershed

# Run PCA
results <- prcomp(measurement_array, scale. = TRUE)

# Get the PCA scores (first two principal components)
pca_scores <- as.data.frame(results$x)

# Combine PCA scores with metadata
pca_results <- tibble(
  PC1 = pca_scores$PC1,
  PC2 = pca_scores$PC2,
  Fish_id = ids,
  Watershed = watersheds
)

# Optional: Filter out extreme outliers
filtered_pca_results <- pca_results %>%
  filter(PC1 >= -150) %>%
  filter(PC2 < 40) %>%
  filter(PC2 > -10)

# Plot the filtered PCA results
ggplot(filtered_pca_results, aes(x = PC1, y = PC2, color = Watershed)) +
  geom_point(size = 1.5) +
  theme_minimal() +
  labs(title = "PCA of Iso Values by Watershed (Outlier Removed)",
       x = "Principal Component 1",
       y = "Principal Component 2") +
  theme(legend.title = element_blank()) # Optional: remove legend title
