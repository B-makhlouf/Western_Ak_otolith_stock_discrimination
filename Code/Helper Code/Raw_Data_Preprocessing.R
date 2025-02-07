





#### This function takes all the raw data files in the Tim_Locations folder (which have been given a trim location)
# And processes them to be used in Classifier, PCA, DTW, etc. 
process_trimmed_data <- function(interp_points = 1000, microns_before = 250, microns_after = 400, window_size = 60, gamma_value = 1.4) {
  library(tidyverse)
  library(here)
  library(zoo)
  library(mgcv)
  
  data_directory <- here("Data/Processed/Trim_Locations")
  files <- list.files(data_directory, full.names = TRUE)
  results_list <- list()
  
  for (file_path in files) {
    cat("\nProcessing file:", file_path, "\n")  # Debugging output
    
    ind_data <- tryCatch({
      read_csv(file_path)
    }, error = function(e) {
      cat("Error reading file:", e$message, "\n")
      return(NULL)
    })
    
    if (is.null(ind_data)) next  # Skip to the next file if read_csv fails
    
    # Extract metadata
    watershed <- ind_data$Watershed[1]
    natal_iso <- ind_data$natal_origin_iso[1]
    fish_id <- ind_data$Fish_id[1]
    natal_iso_start <- ind_data$natal_microns_start[1]
    natal_iso_end <- ind_data$natal_microns_end[1]
    marine_start <- ind_data$marine_start[1]
    Year <- ind_data$Year[1]
    
    # Trim the data
    ind_data <- ind_data %>%
      filter(Microns >= (natal_iso_start - microns_before) & Microns <= (marine_start + microns_after))
    
    # Interpolate Iso values
    interpolated <- tryCatch({
      if (nrow(ind_data) < 2 || all(is.na(ind_data$Iso))) {
        rep(NA, interp_points)
      } else {
        approx(
          x = seq_along(ind_data$Iso),
          y = ind_data$Iso,
          xout = seq(1, nrow(ind_data), length.out = interp_points),
          method = "linear",
          rule = 2
        )$y
      }
    }, error = function(e) {
      cat("Error in interpolation:", e$message, "\n")
      return(rep(NA, interp_points))
    })
    
    # Compute moving average
    moving_avg <- tryCatch({
      rollapply(
        interpolated,
        width = window_size,
        FUN = mean,
        align = "center",
        fill = NA
      )
    }, error = function(e) {
      cat("Error in moving average:", e$message, "\n")
      return(rep(NA, interp_points))
    })
    
    # Apply GAM smoothing directly on the original interpolated Iso values
    gam_smoothed <- tryCatch({
      valid_idx <- which(!is.na(interpolated))
      
      if (length(valid_idx) > 2) {  # Ensure enough data points for GAM fitting
        df <- tibble(
          Microns = seq_along(interpolated),  # x-axis values
          Iso = interpolated                  # y-axis values
        ) %>% drop_na()
        
        # Dynamic k calculation
        n <- nrow(df)
        k <- floor(15 * (n^(2/9)))
        
        # Fit the GAM model
        model <- gam(Iso ~ s(Microns, bs = "tp", k = k), gamma = gamma_value, data = df)
        
        # Predict smoothed values
        predict(model, newdata = tibble(Microns = seq_along(interpolated)))
      } else {
        rep(NA, interp_points)  # Return NA if not enough points
      }
    }, error = function(e) {
      cat("Error in GAM smoothing:", e$message, "\n")
      return(rep(NA, interp_points))
    })
    
    # Store results
    results_list[[file_path]] <- tibble(
      Fish_id = fish_id,
      Watershed = watershed,
      Iso = list(interpolated), 
      Moving_Avg = list(moving_avg),
      GAM_Smoothed = list(gam_smoothed),
      Natal_Iso = natal_iso,
      Year = Year
    )
  }
  
  # Combine results into a single tibble
  combined_results <- bind_rows(results_list, .id = "Dataset")
  
  # Remove rows with all NA in the Iso column
  filtered_results <- combined_results %>%
    filter(map_lgl(Iso, ~ !all(is.na(.))))
  
  # Create measurement arrays
  measurement_array <- do.call(cbind, filtered_results$Iso) %>% t()  # Transpose
  moving_avg_array <- do.call(cbind, filtered_results$Moving_Avg) %>% t()
  gam_smoothed_array <- do.call(cbind, filtered_results$GAM_Smoothed) %>% t()
  
  # Extract metadata
  ids <- filtered_results$Fish_id
  watersheds <- filtered_results$Watershed
  natal_origins <- filtered_results$Natal_Iso
  years <- filtered_results$Year
  
  list(
    measurement_array = measurement_array,
    moving_avg_array = moving_avg_array,
    gam_smoothed_array = gam_smoothed_array,
    ids = ids,
    watersheds = watersheds,
    natal_origins = natal_origins,
    Year = years
  )
}

