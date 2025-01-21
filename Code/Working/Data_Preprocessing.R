# Load necessary libraries
library(tidyverse)
library(here)
library(shiny)

if (T){
  
  # Define directories and their corresponding metadata files
  data_directories <- list(
    Yukon2015 = here("Data/Intermediate/Trimmed no core or marine/Yukon/LA Data"),
    Kusko2017 = here("Data/Intermediate/Trimmed no core or marine/Kusko/LA Data"), 
    Nush2014 = here("Data/Intermediate/Trimmed no core or marine/Nush/LA Data")
  )
  
  # Function to interpolate Iso values
  interpolate_data <- function(data) {
    if (nrow(data) < 2 || all(is.na(data$Iso))) {
      return(rep(NA, 1000))
    }
    
    approx(
      x = seq_along(data$Iso),
      y = data$Iso,
      xout = seq(1, nrow(data), length.out = 1000),
      method = "linear",
      rule = 2
    )$y
  }
  
  
  
  
  # Function to process all files in a directory and filter by Fish_id
  process_directory <- function(files, metadata) {
    files %>%
      map_df(~ {
        tryCatch({
          file_name <- basename(.x)
          
          # Read the data and select relevant columns
          ind_data <- read_csv(.x) %>% select(Iso, Sr88, Watershed, Fish_id, natal_iso)
          watershed <- ind_data$Watershed[1]
          natal_iso <- ind_data$natal_iso[1]  # Assuming this column exists in your data
          fish_id <- ind_data$Fish_id[1]
          
          # Debug message to check the assigned natal_iso
          message("Assigned natal_iso: ", natal_iso)
          
          # Interpolate Iso values
          interpolated <- interpolate_data(ind_data)
          
          # Return the result as a tibble
          tibble(
            Fish_id = fish_id,
            Watershed = watershed,
            Natal_Iso = natal_iso,  # Include Natal_Iso in the tibble
            Iso = list(interpolated)
          )
        }, error = function(e) {
          message("Error processing file: ", .x, " - ", e$message)
          tibble(Fish_id = NA, Watershed = NA, Natal_Iso = NA, Iso = list(rep(NA, 1000)))
        })
      })
  }
  
  # Process all datasets
  results_list <- map(data_directories, ~ {
    files <- list.files(.x, full.names = TRUE)
    process_directory(files)
  })
  
  # Combine results into a single tibble
  combined_results <- bind_rows(results_list, .id = "Dataset")
  
  # Debug message to check if Natal_Iso is in the final combined_results
  print(colnames(combined_results))
  print(head(combined_results))
  
  # Remove rows with all NA in the Iso column
  filtered_results <- combined_results %>%
    filter(map_lgl(Iso, ~ !all(is.na(.))))
  
  # Create a measurement array from the Iso values
  measurement_array <- do.call(cbind, filtered_results$Iso)
  measurement_array <- t(measurement_array)
  
  # Check for missing and infinite values
  if (anyNA(measurement_array) || any(is.infinite(measurement_array))) {
    measurement_array[is.infinite(measurement_array)] <- NA
    measurement_array <- apply(measurement_array, 2, function(x) {
      if (any(is.na(x))) {
        x[is.na(x)] <- mean(x, na.rm = TRUE)
      }
      return(x)
    })
  }
}



# Extract metadata
ids <- filtered_results$Fish_id
watersheds <- filtered_results$Watershed
natal_origins<- filtered_results$Natal_Iso

# Add the metadata to the front of measurement_array and save as as .csv 
all_data<- cbind(metadata_filtered, measurement_array_filtered)

write.csv(all_data, "Data/Intermediate/PCA_data.csv")
