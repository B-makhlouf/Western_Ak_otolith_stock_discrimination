####
# This script processes otolith isotope data from the AnalysisReady folder
# and prepares it for use in classification, PCA, and other analyses.
# It trims from natal origin start to marine transition + extension,
# calculates moving averages, GAM smoothing, and Sr88 correction.
# Sr88 is corrected so lowest point = 0 and marine transition = 1.
# Combined file contains raw Sr87/86 + corrected Sr88.
####

library(data.table)
library(mgcv)
library(zoo)
library(pbapply)
library(tidyverse)
library(here)

# Main processing function
process_analysis_ready_data <- function(window_size = 40, gamma_value = 1.2, marine_extension = 100 ) {
  
  # Set data directory
  data_directory <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/data/LA_Data/AnalysisReady"
  
  # Validate directory and get files
  if (!dir.exists(data_directory)) {
    stop(paste("Directory does not exist:", data_directory))
  }
  
  files <- list.files(data_directory, full.names = TRUE, pattern = "\\.csv$")
  if (length(files) == 0) {
    stop(paste("No CSV files found in:", data_directory))
  }
  
  cat("Found", length(files), "files to process\n")
  
  # FIRST PASS: Calculate average time series length
  cat("First pass: Calculating average time series length...\n")
  
  time_series_lengths <- sapply(files, function(file_path) {
    ind_data <- tryCatch(fread(file_path), error = function(e) NULL)
    if (is.null(ind_data) || nrow(ind_data) < 2) return(NA)
    
    natal_start <- ind_data$natal_microns_start[1]
    marine_start <- ind_data$marine_start[1]
    
    if (is.na(natal_start) || is.na(marine_start)) return(NA)
    
    marine_end <- marine_start + marine_extension
    ind_data_filtered <- ind_data %>% 
      filter(Microns >= natal_start & Microns <= marine_end) %>%
      arrange(Microns)
    
    if (nrow(ind_data_filtered) < 5) return(NA)
    return(nrow(ind_data_filtered))
  })
  
  # Calculate average length
  valid_lengths <- time_series_lengths[!is.na(time_series_lengths)]
  if (length(valid_lengths) == 0) {
    stop("No valid time series lengths found")
  }
  
  average_length <- round(mean(valid_lengths))
  cat("Average time series length:", average_length, "data points\n")
  cat("Range of lengths:", min(valid_lengths), "to", max(valid_lengths), "\n")
  
  # SECOND PASS: Process all files
  cat("Second pass: Processing files with interpolation points =", average_length, "...\n")
  
  results_list <- pblapply(files, function(file_path) {
    ind_data <- tryCatch(fread(file_path), error = function(e) {
      cat("Error reading file:", file_path, "-", e$message, "\n")
      return(NULL)
    })
    
    if (is.null(ind_data) || nrow(ind_data) < 2) return(NULL)
    
    # Extract metadata
    fish_id <- ind_data$Fish_id[1]
    watershed <- ind_data$Watershed[1]
    natal_iso <- ind_data$natal_origin_iso[1]
    year <- ind_data$Year[1]
    natal_start <- ind_data$natal_microns_start[1]
    marine_start <- ind_data$marine_start[1]
    
    # Validate boundaries
    if (is.na(natal_start) || is.na(marine_start)) {
      cat("Warning: Missing boundaries for fish:", fish_id, "\n")
      return(NULL)
    }
    
    # Calculate micron size (total span of selected region)
    marine_end <- marine_start + marine_extension
    micron_size <- marine_end - natal_start
    
    # Filter data
    ind_data_filtered <- ind_data %>% 
      filter(Microns >= natal_start & Microns <= marine_end) %>%
      arrange(Microns)
    
    if (nrow(ind_data_filtered) < 5) {
      cat("Warning: Insufficient data points for fish:", fish_id, "\n")
      return(NULL)
    }
    
    # Create micron sequence for interpolation
    micron_seq <- seq(from = min(ind_data_filtered$Microns, na.rm = TRUE),
                      to = max(ind_data_filtered$Microns, na.rm = TRUE),
                      length.out = average_length)
    
    # Interpolate Sr87/86 isotope ratios
    iso_interpolated <- tryCatch({
      if (all(is.na(ind_data_filtered$Iso))) {
        rep(NA, average_length)
      } else {
        approx(x = ind_data_filtered$Microns, 
               y = ind_data_filtered$Iso, 
               xout = micron_seq, 
               method = "linear", 
               rule = 2)$y
      }
    }, error = function(e) {
      cat("Error in isotope interpolation for fish:", fish_id, "\n")
      return(rep(NA, average_length))
    })
    
    # Calculate moving average
    iso_moving_avg <- tryCatch({
      if (all(is.na(iso_interpolated))) {
        rep(NA, length(iso_interpolated))
      } else {
        rollapply(iso_interpolated, width = min(window_size, length(iso_interpolated)), 
                  FUN = mean, align = "center", fill = NA, na.rm = TRUE)
      }
    }, error = function(e) {
      cat("Error in moving average for fish:", fish_id, "\n")
      return(rep(NA, length(iso_interpolated)))
    })
    
    # GAM smoothing
    iso_gam_smoothed <- tryCatch({
      valid_idx <- !is.na(iso_interpolated)
      if (sum(valid_idx) > 10) {
        df <- data.frame(Index = which(valid_idx), Iso = iso_interpolated[valid_idx])
        k <- min(30, floor(15 * (nrow(df)^(2/9))))
        k <- max(k, 3)
        
        model <- gam(Iso ~ s(Index, bs = "tp", k = k), gamma = gamma_value, data = df)
        
        full_predictions <- rep(NA, average_length)
        full_predictions[valid_idx] <- predict(model, newdata = data.frame(Index = which(valid_idx)))
        
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
      cat("Error in GAM smoothing for fish:", fish_id, "\n")
      return(rep(NA, average_length))
    })
    
    # Interpolate Sr88 (raw)
    sr88_raw <- tryCatch({
      if (all(is.na(ind_data_filtered$Sr88))) {
        rep(NA, average_length)
      } else {
        approx(x = ind_data_filtered$Microns, 
               y = ind_data_filtered$Sr88, 
               xout = micron_seq, 
               method = "linear", 
               rule = 2)$y
      }
    }, error = function(e) {
      cat("Error in Sr88 interpolation for fish:", fish_id, "\n")
      return(rep(NA, average_length))
    })
    
    # Correct Sr88: lowest point = 0, marine transition = 1
    sr88_corrected <- tryCatch({
      if (all(is.na(sr88_raw))) {
        rep(NA, average_length)
      } else {
        # Find marine transition point in interpolated data
        marine_transition_idx <- which.min(abs(micron_seq - marine_start))
        
        # Get min value and marine transition value
        min_val <- min(sr88_raw, na.rm = TRUE)
        marine_val <- sr88_raw[marine_transition_idx]
        
        # Avoid division by zero
        if (abs(marine_val - min_val) < 1e-10) {
          rep(0, average_length)  # If no change, set all to 0
        } else {
          # Normalize: min = 0, marine transition = 1
          (sr88_raw - min_val) / (marine_val - min_val)
        }
      }
    }, error = function(e) {
      cat("Error in Sr88 correction for fish:", fish_id, "\n")
      return(rep(NA, average_length))
    })
    
    # Return results
    list(
      Fish_id = fish_id,
      Watershed = watershed,
      Natal_Iso = natal_iso,
      Year = year,
      Natal_Start = natal_start,
      Marine_Start = marine_start,
      Marine_End = marine_end,
      Micron_Size = micron_size,  # NEW FIELD: Total micron span
      Original_Data_Points = nrow(ind_data_filtered),
      Interpolated_Points = average_length,
      Iso_Raw = iso_interpolated,
      Iso_Moving_Avg = iso_moving_avg,
      Iso_GAM_Smoothed = iso_gam_smoothed,
      Sr88_Raw = sr88_raw,
      Sr88_Corrected = sr88_corrected
    )
  })
  
  # Filter out NULL results
  results_list <- Filter(Negate(is.null), results_list)
  
  if (length(results_list) == 0) {
    stop("No valid data processed from any files")
  }
  
  cat("Successfully processed", length(results_list), "files\n")
  
  # Extract data into matrices
  iso_raw_array <- do.call(rbind, lapply(results_list, `[[`, "Iso_Raw"))
  iso_moving_avg_array <- do.call(rbind, lapply(results_list, `[[`, "Iso_Moving_Avg"))
  iso_gam_smoothed_array <- do.call(rbind, lapply(results_list, `[[`, "Iso_GAM_Smoothed"))
  sr88_raw_array <- do.call(rbind, lapply(results_list, `[[`, "Sr88_Raw"))
  sr88_corrected_array <- do.call(rbind, lapply(results_list, `[[`, "Sr88_Corrected"))
  
  # Create metadata (with Micron_Size as LAST field for modeling)
  metadata <- data.frame(
    Fish_id = sapply(results_list, `[[`, "Fish_id"),
    Watershed = sapply(results_list, `[[`, "Watershed"),
    Natal_Iso = sapply(results_list, `[[`, "Natal_Iso"),
    Year = sapply(results_list, `[[`, "Year"),
    Natal_Start = sapply(results_list, `[[`, "Natal_Start"),
    Marine_Start = sapply(results_list, `[[`, "Marine_Start"),
    Marine_End = sapply(results_list, `[[`, "Marine_End"),
    Original_Data_Points = sapply(results_list, `[[`, "Original_Data_Points"),
    Interpolated_Points = sapply(results_list, `[[`, "Interpolated_Points"),
    Micron_Size = sapply(results_list, `[[`, "Micron_Size"),  # LAST FIELD - included as feature
    stringsAsFactors = FALSE
  )
  
  # Add column names
  colnames(iso_raw_array) <- paste0("X", 1:ncol(iso_raw_array))
  colnames(iso_moving_avg_array) <- paste0("X", 1:ncol(iso_moving_avg_array))
  colnames(iso_gam_smoothed_array) <- paste0("X", 1:ncol(iso_gam_smoothed_array))
  colnames(sr88_raw_array) <- paste0("X", 1:ncol(sr88_raw_array))
  colnames(sr88_corrected_array) <- paste0("X", 1:ncol(sr88_corrected_array))
  
  # Create combined feature set (raw isotopes + corrected Sr88)
  combined_features <- cbind(iso_raw_array, sr88_corrected_array)
  colnames(combined_features) <- paste0("X", 1:ncol(combined_features))
  
  # Save files
  save_processed_data(metadata, iso_raw_array, iso_moving_avg_array, 
                      iso_gam_smoothed_array, sr88_corrected_array, combined_features)
  
  # Print summary
  print_summary(results_list, metadata, average_length, marine_extension)
  
  # Return processed data
  list(
    iso_raw_array = iso_raw_array,
    iso_moving_avg_array = iso_moving_avg_array,
    iso_gam_smoothed_array = iso_gam_smoothed_array,
    sr88_raw_array = sr88_raw_array,
    sr88_corrected_array = sr88_corrected_array,
    combined_features = combined_features,
    metadata = metadata,
    processing_info = list(
      average_length = average_length,
      original_lengths = valid_lengths,
      window_size = window_size,
      gamma_value = gamma_value,
      marine_extension = marine_extension
    )
  )
}

# Function to save processed data
save_processed_data <- function(metadata, iso_raw, iso_ma, iso_gam, sr88_corrected, combined) {
  
  output_dir <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/data/LA_Data/Preprocessed_ts_matrices"
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  filename_base <- "NatalToMarine_Processed"
  
  # Save raw isotope data
  write.csv(cbind(metadata, iso_raw), 
            file = file.path(output_dir, paste0(filename_base, "_RAW.csv")), 
            row.names = FALSE)
  
  # Save moving average data (remove all-NA columns)
  non_na_cols <- colSums(is.na(iso_ma)) < nrow(iso_ma)
  iso_ma_clean <- iso_ma[, non_na_cols, drop = FALSE]
  write.csv(cbind(metadata, iso_ma_clean), 
            file = file.path(output_dir, paste0(filename_base, "_MA.csv")), 
            row.names = FALSE)
  
  # Save GAM smoothed data
  write.csv(cbind(metadata, iso_gam), 
            file = file.path(output_dir, paste0(filename_base, "_GAM.csv")), 
            row.names = FALSE)
  
  # Save corrected Sr88 data
  write.csv(cbind(metadata, sr88_corrected), 
            file = file.path(output_dir, paste0(filename_base, "_Sr88.csv")), 
            row.names = FALSE)
  
  # Save combined features (raw isotopes + corrected Sr88)
  write.csv(cbind(metadata, combined), 
            file = file.path(output_dir, paste0(filename_base, "_Combined.csv")), 
            row.names = FALSE)
  
  cat("Saved all processed files to:", output_dir, "\n")
}

# Function to print processing summary
print_summary <- function(results_list, metadata, average_length, marine_extension) {
  cat("\n=== PROCESSING SUMMARY ===\n")
  cat("Total files processed:", length(results_list), "\n")
  cat("Average original time series length:", round(mean(metadata$Original_Data_Points), 1), "\n")
  cat("Interpolation points used:", average_length, "\n")
  cat("Marine extension:", marine_extension, "microns\n")
  
  # NEW: Summary of micron sizes
  cat("Micron size statistics:\n")
  cat("  Mean micron size:", round(mean(metadata$Micron_Size), 1), "microns\n")
  cat("  Median micron size:", round(median(metadata$Micron_Size), 1), "microns\n")
  cat("  Range:", round(min(metadata$Micron_Size), 1), "to", round(max(metadata$Micron_Size), 1), "microns\n")
  
  cat("Watershed distribution:\n")
  print(table(metadata$Watershed))
  cat("Original data points range:", min(metadata$Original_Data_Points), "to", max(metadata$Original_Data_Points), "\n")
}

# Function to load processed data
load_processed_data <- function(data_type) {
  data_type <- toupper(data_type)
  if (!data_type %in% c("RAW", "MA", "GAM", "SR88", "COMBINED")) {
    stop("Invalid data_type. Choose from 'RAW', 'MA', 'GAM', 'Sr88', or 'Combined'.")
  }
  
  filename <- paste0("NatalToMarine_Processed_", data_type, ".csv")
  file_path <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/data/LA_Data/Preprocessed_ts_matrices"
  file_path <- file.path(file_path, filename)
  
  if (!file.exists(file_path)) {
    stop(paste("File not found:", file_path))
  }
  
  return(read.csv(file_path))
}

# Function to validate processed data
validate_processed_data <- function(processed_data) {
  cat("\n=== DATA VALIDATION ===\n")
  
  metadata <- processed_data$metadata
  cat("Metadata columns:", ncol(metadata), "\n")
  cat("Required columns present:", 
      all(c("Fish_id", "Watershed", "Natal_Start", "Marine_Start", "Micron_Size") %in% colnames(metadata)), "\n")
  
  iso_na_prop <- mean(is.na(processed_data$iso_gam_smoothed_array))
  sr88_na_prop <- mean(is.na(processed_data$sr88_corrected_array))
  
  cat("Proportion NA in GAM smoothed isotopes:", round(iso_na_prop, 3), "\n")
  cat("Proportion NA in corrected Sr88 data:", round(sr88_na_prop, 3), "\n")
  
  cat("Array dimensions consistent:", 
      nrow(processed_data$iso_gam_smoothed_array) == nrow(metadata), "\n")
  
  # NEW: Validate micron size calculations
  if ("Micron_Size" %in% colnames(metadata)) {
    calculated_sizes <- metadata$Marine_End - metadata$Natal_Start
    size_check <- all.equal(metadata$Micron_Size, calculated_sizes, tolerance = 1e-10)
    cat("Micron size calculations valid:", isTRUE(size_check), "\n")
    
    if (!isTRUE(size_check)) {
      cat("WARNING: Micron size calculation mismatch detected!\n")
    }
  }
  
  if ("processing_info" %in% names(processed_data)) {
    cat("Average length used for interpolation:", processed_data$processing_info$average_length, "\n")
  }
  
  return(invisible(TRUE))
}

##########
# Main execution
##########

# Process the data
processed_data <- process_analysis_ready_data()

# Validate the processed data
validate_processed_data(processed_data)

# Example of loading specific data types
# gam_data <- load_processed_data("GAM")
# combined_data <- load_processed_data("Combined")
# sr88_data <- load_processed_data("Sr88")