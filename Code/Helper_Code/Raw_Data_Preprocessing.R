
####
# This script processes raw otolith isotope data from the Tim_Locations folder
# and prepares it for use in classification, PCA, DTW, and other analyses.
# It filters based on landmark regions, calculates moving averages, and smooths data using GAM.
# The processed data is saved in CSV files for different filtering conditions.
####

library(data.table)
library(mgcv)
library(zoo)
library(pbapply)  # For progress bar

# Function: process_trimmed_data
# Description: Processes all raw data files in the "Tim_Locations" folder and prepares them for classification, PCA, DTW, etc.
# Arguments:
# - interp_points: Number of interpolation points (default = 1000)
# - window_size: Window size for moving average calculation (default = 60)
# - gamma_value: Gamma parameter for GAM smoothing (default = 1.4)
# - landmark_filter: Vector of landmark names to filter data (default = c("Core", "Fw", "Early Marine"))
# Returns:
# - A list containing processed measurement arrays, moving average arrays, GAM smoothed arrays, metadata, and fish IDs.

process_trimmed_data <- function(interp_points = 1000, window_size = 60, gamma_value = 1.4, landmark_filter = c("Core", "Fw")) {     
  data_directory <- "Data/Processed/Landmarks"   
  files <- list.files(data_directory, full.names = TRUE, pattern = "\\.csv$")     
  
  results_list <- pblapply(files, function(file_path) {         
    ind_data <- tryCatch({ fread(file_path) }, error = function(e) { return(NULL) })         
    if (is.null(ind_data) || nrow(ind_data) < 2) return(NULL)  
    
    # Extract metadata
    watershed <- ind_data$Watershed[1]     
    natal_iso <- ind_data$natal_origin_iso[1]     
    fish_id <- ind_data$Fish_id[1]     
    Year <- ind_data$Year[1]         
    
    # Filter data based on landmark region    
    ind_data <- ind_data %>% filter(Landmark %in% landmark_filter)
    if (nrow(ind_data) < 2) return(NULL)  
  
    
    # Interpolation
    interpolated <- tryCatch({       
      if (all(is.na(ind_data$Iso))) rep(NA, interp_points) 
      else approx(seq_len(nrow(ind_data)), ind_data$Iso, seq(1, nrow(ind_data), length.out = interp_points), method = "linear", rule = 2)$y     
    }, error = function(e) { return(rep(NA, interp_points)) })         
    
    # Moving Average Calculation     
    moving_avg <- rollapply(interpolated, width = window_size, FUN = mean, align = "center", fill = NA)         
    
    # GAM Smoothing
    gam_smoothed <- tryCatch({       
      valid_idx <- !is.na(interpolated)       
      if (sum(valid_idx) > 2) {           
        df <- data.frame(Microns = which(valid_idx), Iso = interpolated[valid_idx])                  
        k <- floor(15 * (nrow(df)^(2/9)))                  
        model <- gam(Iso ~ s(Microns, bs = "tp", k = k), gamma = gamma_value, data = df)                  
        predict(model, newdata = data.frame(Microns = seq_len(interp_points)))       
      } else rep(NA, interp_points)     
    }, error = function(e) { return(rep(NA, interp_points)) })         
    
    # Store results     
    list( Fish_id = fish_id, Watershed = watershed, Iso = interpolated, 
          Moving_Avg = moving_avg, GAM_Smoothed = gam_smoothed, 
          Natal_Iso = natal_iso, Year = Year )   
  })     
  
  results_list <- Filter(Negate(is.null), results_list)     
  
  # Convert results into matrices   
  measurement_array <- do.call(rbind, lapply(results_list, `[[`, "Iso"))   
  moving_avg_array <- do.call(rbind, lapply(results_list, `[[`, "Moving_Avg"))   
  gam_smoothed_array <- do.call(rbind, lapply(results_list, `[[`, "GAM_Smoothed"))     
  
  ids <- sapply(results_list, `[[`, "Fish_id")   
  watersheds <- sapply(results_list, `[[`, "Watershed")   
  natal_origins <- sapply(results_list, `[[`, "Natal_Iso")   
  years <- sapply(results_list, `[[`, "Year")     
  
  metadata <- tibble(Fish_id = ids, Watershed = watersheds, Natal_Iso = natal_origins, Year = years)
  
  # Define filenames dynamically based on parameters
  landmark_str <- paste(landmark_filter, collapse = "_")
  filename_base <- paste0("Processed_", landmark_str)
  
  # Save raw data
  all_data_combined_raw <- cbind(metadata, measurement_array)
  write.csv(all_data_combined_raw, file = here(paste0("Data/Processed/Preprocessed_ts_matrices/", filename_base, "_RAW.csv")), row.names = FALSE)
  
  # Save moving average data
  moving_avg_array <- moving_avg_array[, colSums(is.na(moving_avg_array)) == 0]  # Remove NA columns
  all_data_combined_MA <- cbind(metadata, moving_avg_array)
  write.csv(all_data_combined_MA, file = here(paste0("Data/Processed/Preprocessed_ts_matrices/", filename_base, "_MA.csv")), row.names = FALSE)
  
  # Save GAM smoothed data
  all_data_combined_GAM <- cbind(metadata, gam_smoothed_array)
  write.csv(all_data_combined_GAM, file = here(paste0("Data/Processed/Preprocessed_ts_matrices/", filename_base, "_GAM.csv")), row.names = FALSE)
  
  # Return processed data as a list
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

# Function: load_processed_data
# Description: Loads processed data from saved CSV files.
# Arguments:
# - data_type: One of "RAW", "MA", or "GAM" (case-insensitive)
# - landmark_filter: Vector of landmark names used in processing
# Returns:
# - A data frame containing the requested processed data.

load_processed_data <- function(data_type, landmark_filter) {
  data_type <- toupper(data_type)
  if (!data_type %in% c("RAW", "MA", "GAM")) {
    stop("Invalid data_type. Choose from 'RAW', 'MA', or 'GAM'.")
  }
  
  # Construct the filename based on parameters
  landmark_str <- paste(landmark_filter, collapse = "_")
  filename <- paste0("Processed_", landmark_str, "_", data_type, ".csv")
  file_path <- here("/Users/benjaminmakhlouf/Research_repos/Western_Ak_otolith_stock_discrimination/Data/Classification_ts_matrices/Sr8786", filename)
  
  # Check if the file exists
  if (!file.exists(file_path)) {
    stop(paste("File not found:", file_path))
  }
  
  # Read and return the data
  return(read.csv(file_path))
}



########## 
#Loop through the relevant combination of landmark filters and export a .csv 
#of all of the above for each, interpolating to 1000 microns
#########

# if (T){
#   
#   preprocessed_data_core <- process_trimmed_data(landmark_filter = c("Core"))
#   preprocessed_data_fw <- process_trimmed_data(landmark_filter = c("Fw"))
#   preprocessed_data_core_fw <- process_trimmed_data( landmark_filter = c("Core", "Fw"))
#   
# }
# 
# 



