# Packages
library(tidyverse)
library(here)

# Directories
Nush2014_directory <- here("Data/Intermediate/Cleaned and Trimmed/2014 Nush")
Yukon2015_directory <- here("Data/Intermediate/Cleaned and Trimmed/2015 Yukon")
Kusko2017_directory <- here("Data/Intermediate/Cleaned and Trimmed/2017 Kusko")

# List all files in each directory
Nush2014_files <- list.files(Nush2014_directory, full.names = TRUE)
#Yukon2015_files <- list.files(Yukon2015_directory, full.names = TRUE)
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
#Yukon2015_results <- process_directory(Yukon2015_files)
Kusko2017_results <- process_directory(Kusko2017_files)

# Combine results into a single tibble
combined_results <- bind_rows(
  Nush2014_results,
  #Yukon2015_results,
  Kusko2017_results
)

# Remove rows with all NA in the Iso column
filtered_results <- combined_results %>%
  filter(map_lgl(Iso, ~ !all(is.na(.))))

# Transpose the Iso values to make each individual a row
measurement_array <- do.call(cbind, filtered_results$Iso)

# Transpose the matrix to get individuals as rows
measurement_array <- t(measurement_array)

# Extract the metadata (ID and Watershed)
ids <- filtered_results$Fish_id
watersheds <- filtered_results$Watershed

# Output results
cat("Dimensions of measurement array:", dim(measurement_array), "\n")
cat("First few IDs:", head(ids), "\n")
cat("First few watersheds:", head(watersheds), "\n")
