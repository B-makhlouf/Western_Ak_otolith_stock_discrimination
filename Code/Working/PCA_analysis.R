# Packages
library(tidyverse)
library(here)

# Directories

# Nushagak
Nush2014_directory <- here("Data/Intermediate/Cleaned and Trimmed/2014 Nush")
Nush2015_directory <- here("Data/Intermediate/Cleaned and Trimmed/2015 Nush")
Nush2011_directory <- here("Data/Intermediate/Cleaned and Trimmed/2011 Nush")

# Yukon
Yukon2015_directory <- here("Data/Intermediate/Cleaned and Trimmed/2015 Yukon")
Yukon2016_directory <- here("Data/Intermediate/Cleaned and Trimmed/2016 Yukon")
Yukon2017_directory <- here("Data/Intermediate/Cleaned and Trimmed/2017 Yukon")
Yukon2019_directory <- here("Data/Intermediate/Cleaned and Trimmed/2019 Yukon")
Yukon2021_directory <- here("Data/Intermediate/Cleaned and Trimmed/2021 Yukon")

# Kusko
Kusko2015_directory <- here("Data/Intermediate/Cleaned and Trimmed/2015 Kusko")
Kusko2017_directory <- here("Data/Intermediate/Cleaned and Trimmed/2017 Kusko")
Kusko2019_directory <- here("Data/Intermediate/Cleaned and Trimmed/2019 Kusko")
kusko2020_directory <- here("Data/Intermediate/Cleaned and Trimmed/2020 Kusko")
kusko2021_directory <- here("Data/Intermediate/Cleaned and Trimmed/2021 Kusko")

# List all files in each directory
all_files <- list(
  Nush2014 = list.files(Nush2014_directory, full.names = TRUE),
  Nush2015 = list.files(Nush2015_directory, full.names = TRUE),
  Nush2011 = list.files(Nush2011_directory, full.names = TRUE),
  Yukon2015 = list.files(Yukon2015_directory, full.names = TRUE),
  Yukon2016 = list.files(Yukon2016_directory, full.names = TRUE),
  Yukon2017 = list.files(Yukon2017_directory, full.names = TRUE),
  Yukon2019 = list.files(Yukon2019_directory, full.names = TRUE),
  Yukon2021 = list.files(Yukon2021_directory, full.names = TRUE),
  Kusko2015 = list.files(Kusko2015_directory, full.names = TRUE),
  Kusko2017 = list.files(Kusko2017_directory, full.names = TRUE),
  Kusko2019 = list.files(Kusko2019_directory, full.names = TRUE),
  Kusko2020 = list.files(kusko2020_directory, full.names = TRUE),
  Kusko2021 = list.files(kusko2021_directory, full.names = TRUE)
)

# Function to interpolate data to 1000 points
interpolate_data <- function(data, num_points = 1000) {
  if (!"Iso" %in% names(data) || sum(!is.na(data$Iso)) < 2) {
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
      tryCatch({
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
      }, error = function(e) {
        message("Error processing file: ", .x, " - ", e$message)
        return(tibble(Fish_id = NA, Watershed = NA, Iso = list(rep(NA, 1000))))
      })
    }, .id = "File_Index") # Add file index for debugging
}

# Process all datasets
results_list <- map(all_files, process_directory)

# Combine results into a single tibble
combined_results <- bind_rows(results_list, .id = "Dataset")

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
      theme(legend.title = element_blank())
    