####
# This script processes otolith isotope data from the AnalysisReady folder
# and prepares it for use in classification, PCA, and other analyses.
# It trims from natal origin start to 50 microns past marine transition,
# calculates moving averages, and smooths data using GAM.
# MODIFIED: Interpolation now uses the average length of un-interpolated time series
# The processed data is saved in CSV files for different processing methods.
####

library(data.table)
library(mgcv)
library(zoo)
library(pbapply)  # For progress bar
library(tidyverse)
library(here)

# Function: process_analysis_ready_data
# Description: Processes all data files from the AnalysisReady folder using natal/marine boundaries
# Arguments:
# - window_size: Window size for moving average calculation (default = 60)
# - gamma_value: Gamma parameter for GAM smoothing (default = 1.4)
# - marine_extension: Microns beyond marine start to include (default = 50)
# Returns:
# - A list containing processed measurement arrays, moving average arrays, GAM smoothed arrays, metadata, and fish IDs.

process_analysis_ready_data <- function(window_size = 40, gamma_value = 1.2, marine_extension = 0) {     
  
  # Update data directory to AnalysisReady folder
  data_directory <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/data/LA_Data/AnalysisReady"   
  
  # Check if directory exists
  if (!dir.exists(data_directory)) {
    stop(paste("Directory does not exist:", data_directory))
  }
  
  files <- list.files(data_directory, full.names = TRUE, pattern = "\\.csv$")     
  
  if (length(files) == 0) {
    stop(paste("No CSV files found in:", data_directory))
  }
  
  cat("Found", length(files), "files to process\n")
  
  # FIRST PASS: Calculate average length of un-interpolated time series
  cat("First pass: Calculating average time series length...\n")
  
  time_series_lengths <- sapply(files, function(file_path) {
    ind_data <- tryCatch({ 
      fread(file_path) 
    }, error = function(e) { 
      return(NULL) 
    })         
    
    if (is.null(ind_data) || nrow(ind_data) < 2) return(NA)  
    
    # Extract trimming boundaries
    natal_start <- ind_data$natal_microns_start[1]
    marine_start <- ind_data$marine_start[1]
    
    # Validate that we have the required columns and values
    if (is.na(natal_start) || is.na(marine_start)) {
      return(NA)
    }
    
    # Calculate marine end point (50 microns past marine start)
    marine_end <- marine_start + marine_extension
    
    # Filter data based on micron boundaries (natal start to marine + extension)
    ind_data_filtered <- ind_data %>% 
      filter(Microns >= natal_start & Microns <= marine_end) %>%
      arrange(Microns)  # Ensure data is ordered by microns
    
    if (nrow(ind_data_filtered) < 5) {  # Need minimum data points
      return(NA)
    }
    
    return(nrow(ind_data_filtered))
  })
  
  # Calculate average length, removing NAs
  valid_lengths <- time_series_lengths[!is.na(time_series_lengths)]
  if (length(valid_lengths) == 0) {
    stop("No valid time series lengths found")
  }
  
  average_length <- round(mean(valid_lengths))
  cat("Average time series length:", average_length, "data points\n")
  cat("Range of lengths:", min(valid_lengths), "to", max(valid_lengths), "\n")
  
  # SECOND PASS: Process all files using the calculated average length
  cat("Second pass: Processing files with interpolation points =", average_length, "...\n")
  
  results_list <- pblapply(files, function(file_path) {         
    ind_data <- tryCatch({ 
      fread(file_path) 
    }, error = function(e) { 
      cat("Error reading file:", file_path, "-", e$message, "\n")
      return(NULL) 
    })         
    
    if (is.null(ind_data) || nrow(ind_data) < 2) return(NULL)  
    
    # Extract metadata
    watershed <- ind_data$Watershed[1]     
    natal_iso <- ind_data$natal_origin_iso[1]     
    fish_id <- ind_data$Fish_id[1]     
    year <- ind_data$Year[1]
    
    # Extract trimming boundaries
    natal_start <- ind_data$natal_microns_start[1]
    marine_start <- ind_data$marine_start[1]
    
    # Validate that we have the required columns and values
    if (is.na(natal_start) || is.na(marine_start)) {
      cat("Warning: Missing natal_start or marine_start for fish:", fish_id, "\n")
      return(NULL)
    }
    
    # Calculate marine end point (50 microns past marine start)
    marine_end <- marine_start + marine_extension
    
    # Filter data based on micron boundaries (natal start to marine + extension)
    ind_data_filtered <- ind_data %>% 
      filter(Microns >= natal_start & Microns <= marine_end) %>%
      arrange(Microns)  # Ensure data is ordered by microns
    
    if (nrow(ind_data_filtered) < 5) {  # Need minimum data points
      cat("Warning: Insufficient data points after filtering for fish:", fish_id, "\n")
      return(NULL)
    }
    
    # Interpolation of Sr87/86 isotope ratios using average_length
    interpolated <- tryCatch({       
      if (all(is.na(ind_data_filtered$Iso))) {
        rep(NA, average_length)
      } else {
        # Use micron positions for interpolation instead of row indices
        micron_seq <- seq(from = min(ind_data_filtered$Microns, na.rm = TRUE),
                          to = max(ind_data_filtered$Microns, na.rm = TRUE),
                          length.out = average_length)
        
        approx(x = ind_data_filtered$Microns, 
               y = ind_data_filtered$Iso, 
               xout = micron_seq, 
               method = "linear", 
               rule = 2)$y
      }
    }, error = function(e) { 
      cat("Error in interpolation for fish:", fish_id, "-", e$message, "\n")
      return(rep(NA, average_length)) 
    })         
    
    # Moving Average Calculation     
    moving_avg <- tryCatch({
      if (all(is.na(interpolated))) {
        rep(NA, length(interpolated))
      } else {
        rollapply(interpolated, width = min(window_size, length(interpolated)), 
                  FUN = mean, align = "center", fill = NA, na.rm = TRUE)
      }
    }, error = function(e) {
      cat("Error in moving average for fish:", fish_id, "-", e$message, "\n")
      return(rep(NA, length(interpolated)))
    })
    
    # GAM Smoothing
    gam_smoothed <- tryCatch({       
      valid_idx <- !is.na(interpolated)       
      if (sum(valid_idx) > 10) {  # Need at least 10 valid points for GAM
        df <- data.frame(Index = which(valid_idx), Iso = interpolated[valid_idx])                  
        k <- min(30, floor(15 * (nrow(df)^(2/9))))  # Cap k at reasonable value
        k <- max(k, 3)  # Ensure minimum k
        
        model <- gam(Iso ~ s(Index, bs = "tp", k = k), gamma = gamma_value, data = df)                  
        
        # Predict for all interpolation points
        full_predictions <- rep(NA, average_length)
        full_predictions[valid_idx] <- predict(model, newdata = data.frame(Index = which(valid_idx)))
        
        # Interpolate to fill gaps if needed
        if (any(is.na(full_predictions)) && sum(valid_idx) > 2) {
          full_predictions <- approx(x = 1:average_length, 
                                     y = full_predictions, 
                                     xout = 1:average_length, 
                                     rule = 2)$y
        }
        
        full_predictions
      } else {
        rep(NA, average_length)
      }
    }, error = function(e) { 
      cat("Error in GAM smoothing for fish:", fish_id, "-", e$message, "\n")
      return(rep(NA, average_length)) 
    })         
    
    # Process Sr88 data similarly
    sr88_processed <- tryCatch({
      if (all(is.na(ind_data_filtered$Sr88))) {
        rep(NA, average_length)
      } else {
        micron_seq <- seq(from = min(ind_data_filtered$Microns, na.rm = TRUE),
                          to = max(ind_data_filtered$Microns, na.rm = TRUE),
                          length.out = average_length)
        
        approx(x = ind_data_filtered$Microns, 
               y = ind_data_filtered$Sr88, 
               xout = micron_seq, 
               method = "linear", 
               rule = 2)$y
      }
    }, error = function(e) {
      cat("Error processing Sr88 for fish:", fish_id, "-", e$message, "\n")
      return(rep(NA, average_length))
    })
    
    # Store results     
    list(
      Fish_id = fish_id, 
      Watershed = watershed, 
      Natal_Iso = natal_iso,
      Year = year,
      Natal_Start = natal_start,
      Marine_Start = marine_start,
      Marine_End = marine_end,
      Original_Data_Points = nrow(ind_data_filtered),
      Interpolated_Points = average_length,
      Iso_Raw = interpolated, 
      Iso_Moving_Avg = moving_avg, 
      Iso_GAM_Smoothed = gam_smoothed,
      Sr88_Raw = sr88_processed
    )   
  })     
  
  # Filter out NULL results
  results_list <- Filter(Negate(is.null), results_list)     
  
  if (length(results_list) == 0) {
    stop("No valid data processed from any files")
  }
  
  cat("Successfully processed", length(results_list), "files\n")
  
  # Convert results into matrices   
  iso_raw_array <- do.call(rbind, lapply(results_list, `[[`, "Iso_Raw"))   
  iso_moving_avg_array <- do.call(rbind, lapply(results_list, `[[`, "Iso_Moving_Avg"))   
  iso_gam_smoothed_array <- do.call(rbind, lapply(results_list, `[[`, "Iso_GAM_Smoothed"))     
  sr88_raw_array <- do.call(rbind, lapply(results_list, `[[`, "Sr88_Raw"))
  
  # Extract metadata
  ids <- sapply(results_list, `[[`, "Fish_id")   
  watersheds <- sapply(results_list, `[[`, "Watershed")   
  natal_origins <- sapply(results_list, `[[`, "Natal_Iso")   
  years <- sapply(results_list, `[[`, "Year")     
  natal_starts <- sapply(results_list, `[[`, "Natal_Start")
  marine_starts <- sapply(results_list, `[[`, "Marine_Start")
  marine_ends <- sapply(results_list, `[[`, "Marine_End")
  original_data_points <- sapply(results_list, `[[`, "Original_Data_Points")
  interpolated_points <- sapply(results_list, `[[`, "Interpolated_Points")
  
  metadata <- data.frame(
    Fish_id = ids, 
    Watershed = watersheds, 
    Natal_Iso = natal_origins, 
    Year = years,
    Natal_Start = natal_starts,
    Marine_Start = marine_starts,
    Marine_End = marine_ends,
    Original_Data_Points = original_data_points,
    Interpolated_Points = interpolated_points,
    stringsAsFactors = FALSE
  )
  
  # Add column names to arrays
  colnames(iso_raw_array) <- paste0("X", 1:ncol(iso_raw_array))
  colnames(iso_moving_avg_array) <- paste0("X", 1:ncol(iso_moving_avg_array))
  colnames(iso_gam_smoothed_array) <- paste0("X", 1:ncol(iso_gam_smoothed_array))
  colnames(sr88_raw_array) <- paste0("X", 1:ncol(sr88_raw_array))
  
  # Create output directory
  output_dir <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/data/LA_Data/Preprocessed_ts_matrices"
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Save different processing types
  filename_base <- "NatalToMarine_Processed"
  
  # Save raw isotope data
  all_data_combined_raw <- cbind(metadata, iso_raw_array)
  write.csv(all_data_combined_raw, 
            file = file.path(output_dir, paste0(filename_base, "_RAW.csv")), 
            row.names = FALSE)
  cat("Saved:", paste0(filename_base, "_RAW.csv"), "\n")
  
  # Save moving average data (remove columns that are all NA)
  non_na_cols <- colSums(is.na(iso_moving_avg_array)) < nrow(iso_moving_avg_array)
  iso_moving_avg_clean <- iso_moving_avg_array[, non_na_cols, drop = FALSE]
  all_data_combined_MA <- cbind(metadata, iso_moving_avg_clean)
  write.csv(all_data_combined_MA, 
            file = file.path(output_dir, paste0(filename_base, "_MA.csv")), 
            row.names = FALSE)
  cat("Saved:", paste0(filename_base, "_MA.csv"), "\n")
  
  # Save GAM smoothed data
  all_data_combined_GAM <- cbind(metadata, iso_gam_smoothed_array)
  write.csv(all_data_combined_GAM, 
            file = file.path(output_dir, paste0(filename_base, "_GAM.csv")), 
            row.names = FALSE)
  cat("Saved:", paste0(filename_base, "_GAM.csv"), "\n")
  
  # Save Sr88 data
  all_data_combined_Sr88 <- cbind(metadata, sr88_raw_array)
  write.csv(all_data_combined_Sr88, 
            file = file.path(output_dir, paste0(filename_base, "_Sr88.csv")), 
            row.names = FALSE)
  cat("Saved:", paste0(filename_base, "_Sr88.csv"), "\n")
  
  # Create combined feature set (GAM isotopes + Sr88)
  combined_features <- cbind(iso_gam_smoothed_array, sr88_raw_array)
  colnames(combined_features) <- paste0("X", 1:ncol(combined_features))
  all_data_combined_features <- cbind(metadata, combined_features)
  write.csv(all_data_combined_features, 
            file = file.path(output_dir, paste0(filename_base, "_Combined.csv")), 
            row.names = FALSE)
  cat("Saved:", paste0(filename_base, "_Combined.csv"), "\n")
  
  # Print summary statistics
  cat("\n=== PROCESSING SUMMARY ===\n")
  cat("Total files processed:", length(results_list), "\n")
  cat("Average original time series length:", round(mean(original_data_points), 1), "\n")
  cat("Interpolation points used:", average_length, "\n")
  cat("Marine extension:", marine_extension, "microns\n")
  cat("Watershed distribution:\n")
  print(table(watersheds))
  cat("Original data points range:", min(original_data_points), "to", max(original_data_points), "\n")
  cat("Files saved to:", output_dir, "\n")
  
  # Return processed data as a list
  list(
    iso_raw_array = iso_raw_array,     
    iso_moving_avg_array = iso_moving_avg_clean,     
    iso_gam_smoothed_array = iso_gam_smoothed_array,
    sr88_raw_array = sr88_raw_array,
    combined_features = combined_features,
    metadata = metadata,
    ids = ids,     
    watersheds = watersheds,     
    natal_origins = natal_origins,     
    years = years,
    processing_info = list(
      average_length = average_length,
      original_lengths = valid_lengths,
      window_size = window_size,
      gamma_value = gamma_value,
      marine_extension = marine_extension
    )
  )
}

# Function: load_processed_data
# Description: Loads processed data from saved CSV files.
# Arguments:
# - data_type: One of "RAW", "MA", "GAM", "Sr88", or "Combined" (case-insensitive)
# Returns:
# - A data frame containing the requested processed data.

load_processed_data <- function(data_type) {
  data_type <- toupper(data_type)
  if (!data_type %in% c("RAW", "MA", "GAM", "SR88", "COMBINED")) {
    stop("Invalid data_type. Choose from 'RAW', 'MA', 'GAM', 'Sr88', or 'Combined'.")
  }
  
  # Construct the filename
  filename <- paste0("NatalToMarine_Processed_", data_type, ".csv")
  file_path <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/data/LA_Data/Preprocessed_ts_matrices"
  file_path <- file.path(file_path, filename)
  
  # Check if the file exists
  if (!file.exists(file_path)) {
    stop(paste("File not found:", file_path))
  }
  
  # Read and return the data
  return(read.csv(file_path))
}

# Function to validate processed data
validate_processed_data <- function(processed_data) {
  cat("\n=== DATA VALIDATION ===\n")
  
  # Check metadata
  metadata <- processed_data$metadata
  cat("Metadata columns:", ncol(metadata), "\n")
  cat("Required columns present:", 
      all(c("Fish_id", "Watershed", "Natal_Start", "Marine_Start") %in% colnames(metadata)), "\n")
  
  # Check for missing values in key arrays
  iso_na_prop <- mean(is.na(processed_data$iso_gam_smoothed_array))
  sr88_na_prop <- mean(is.na(processed_data$sr88_raw_array))
  
  cat("Proportion NA in GAM smoothed isotopes:", round(iso_na_prop, 3), "\n")
  cat("Proportion NA in Sr88 data:", round(sr88_na_prop, 3), "\n")
  
  # Check dimension consistency
  cat("Array dimensions consistent:", 
      nrow(processed_data$iso_gam_smoothed_array) == nrow(metadata), "\n")
  
  # Check interpolation consistency
  if ("processing_info" %in% names(processed_data)) {
    cat("Average length used for interpolation:", processed_data$processing_info$average_length, "\n")
    cat("Original length range:", min(processed_data$processing_info$original_lengths), 
        "to", max(processed_data$processing_info$original_lengths), "\n")
  }
  
  return(invisible(TRUE))
}

##########
# Main execution example
# Uncomment to run the processing
##########

# Process the data with adaptive interpolation based on average length
processed_data <- process_analysis_ready_data()

# Validate the processed data
# validate_processed_data(processed_data)

# Example of loading specific data types
# gam_data <- load_processed_data("GAM")
# combined_data <- load_processed_data("Combined")