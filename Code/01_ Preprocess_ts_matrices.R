# 01_Preprocess_ts_matrices.R
# Processes otolith data for machine learning classification 
# Handles SR8786 and SR88 data with different smoothing methods

library(data.table)
library(mgcv)
library(zoo)
library(dplyr)
library(here)
library(progress)
library(tidyverse)

# Fixed calculation for adaptive interpolation points
process_otolith_data <- function(window_size = 60, gamma_value = 0.8, 
                                 landmark_filter = c("Core", "Fw")) {
  # List all files
  landmark_files <- list.files(here("Data/01_Processed_ts/Landmarks"), 
                               pattern = "*.csv", full.names = TRUE)
  results_list <- list()
  
  # Initialize progress bar
  pb <- progress_bar$new(
    format = "Processing [:bar] :percent (:eta remaining)",
    total = length(landmark_files),
    clear = FALSE,
    width = 60
  )
  
  # Calculate adaptive interpolation points
  valid_counts <- numeric()
  
  for (file in landmark_files) {
    tryCatch({
      ind_data <- read.csv(file)
      
      # Skip if file doesn't contain required landmarks
      if (!all(landmark_filter %in% unique(ind_data$Landmark))) {
        next
      }
      
      # Get filtered data extent
      if ("Fw" %in% landmark_filter) {
        fw_indices <- which(ind_data$Landmark == "Fw")
        if (length(fw_indices) == 0) next
        
        fw_max_microns <- max(ind_data$Microns[fw_indices], na.rm = TRUE)
        
        ind_data_filtered <- ind_data %>% 
          filter(Landmark %in% landmark_filter | 
                   (Microns > fw_max_microns & Microns <= fw_max_microns + 300))
      } else {
        ind_data_filtered <- ind_data %>% 
          filter(Landmark %in% landmark_filter)
      }
      
      # Only count if we have enough data points
      if (nrow(ind_data_filtered) >= 10) {
        valid_counts <- c(valid_counts, nrow(ind_data_filtered))
      }
    }, error = function(e) {
      # Skip files with errors
    })
  }
  
  # Calculate the average number of data points (with a fallback)
  if (length(valid_counts) > 0) {
    interp_points <- round(mean(valid_counts, na.rm = TRUE))
  } else {
    interp_points <- 1000  # Fallback if no valid counts
    warning("Could not calculate average data points, using default of 1000")
  }
  
  message(paste("Using", interp_points, "interpolation points"))
  
  # Process each file
  for (file in landmark_files) {
    pb$tick()
    
    tryCatch({
      # Read file data
      ind_data <- read.csv(file)
      
      # Extract metadata
      watershed <- ind_data$Watershed[1]
      natal_iso <- ind_data$natal_origin_iso[1]
      fish_id <- ind_data$Fish_id[1]
      year <- ind_data$Year[1]
      
      # Skip if missing landmark
      if (!all(landmark_filter %in% unique(ind_data$Landmark))) {
        next
      }
      
      # Filter data by landmarks
      fw_max_microns <- ind_data %>% 
        filter(Landmark == "Fw") %>% 
        summarise(max_microns = max(Microns, na.rm = TRUE)) %>% 
        pull(max_microns)
      
      ind_data_filtered <- ind_data %>% 
        filter(Landmark %in% landmark_filter | 
                 (Microns > fw_max_microns & Microns <= fw_max_microns + 300))
      
      # Process SR8786 (Iso)
      # Raw interpolation
      raw_iso <- approx(
        seq_len(nrow(ind_data_filtered)),
        ind_data_filtered$Iso,
        seq(1, nrow(ind_data_filtered), length.out = interp_points),
        method = "linear", rule = 2
      )$y
      
      # Moving average
      ma_iso <- rollapply(ind_data_filtered$Iso, width = window_size, 
                          FUN = mean, align = "center", fill = NA)
      
      ma_iso_interp <- approx(
        seq_len(length(ma_iso)),
        ma_iso,
        seq(1, length(ma_iso), length.out = interp_points),
        method = "linear", rule = 2
      )$y
      
      # GAM smoothing
      valid_idx <- !is.na(ind_data_filtered$Iso)
      df <- data.frame(
        Microns = which(valid_idx), 
        Iso = ind_data_filtered$Iso[valid_idx]
      )
      
      k <- min(50, floor(15 * (nrow(df)^(2/9))))
      model <- gam(Iso ~ s(Microns, bs = "tp", k = k), 
                   gamma = gamma_value, data = df)
      
      gam_iso <- predict(model, newdata = data.frame(
        Microns = seq_len(nrow(ind_data_filtered))))
      
      gam_iso_interp <- approx(
        seq_len(length(gam_iso)),
        gam_iso,
        seq(1, length(gam_iso), length.out = interp_points),
        method = "linear", rule = 2
      )$y
      
    
      # Process Sr88 section in the script
      # Find the last FW landmark index
      last_fw_idx <- which(ind_data_filtered$Landmark == "Fw")
      if(length(last_fw_idx) > 0) {
        last_fw_idx <- max(last_fw_idx)
        
        # Get the Sr88 value at the last FW landmark - this should be 1.0
        sr88_last_fw <- ind_data_filtered$Sr88[last_fw_idx]
        
        # Find the minimum Sr88 value before the last FW landmark
        # This ensures we're only looking at values before the marine transition
        min_sr88 <- min(ind_data_filtered$Sr88[1:last_fw_idx], na.rm = TRUE)
        
        # Normalize Sr88 so that min_sr88 is 0 and sr88_last_fw is 1
        norm_sr88 <- (ind_data_filtered$Sr88 - min_sr88) / (sr88_last_fw - min_sr88)
      } else {
        # Fallback if no FW landmark exists
        message("Warning: No Fw landmark found in file for fish_id: ", fish_id)
        norm_sr88 <- rep(NA, length(ind_data_filtered$Sr88))
      }
      # Interpolate normalized Sr88
      sr88_interp <- approx(
        seq_len(length(norm_sr88)),
        norm_sr88,
        seq(1, length(norm_sr88), length.out = interp_points),
        method = "linear", rule = 2
      )$y
      
      # Create combined feature set (GAM Sr8786 + Sr88)
      combined_interp <- c(gam_iso_interp, sr88_interp)
      
      # Store results
      results_list[[length(results_list) + 1]] <- list(
        Fish_id = fish_id,
        Watershed = watershed,
        Natal_Iso = natal_iso,
        Year = year,
        
        # Results by type
        Raw = raw_iso,
        GAM_Smoothed = gam_iso_interp,
        Moving_Avg = ma_iso_interp,
        Sr88 = sr88_interp, 
        Combined = combined_interp
      )
      
    }, error = function(e) {
      message("Error processing file: ", file, " - ", e$message)
    })
  }
  
  # Combine results for output
  metadata <- data.frame(
    Fish_id = sapply(results_list, `[[`, "Fish_id"),
    Watershed = sapply(results_list, `[[`, "Watershed"),
    Natal_Iso = sapply(results_list, `[[`, "Natal_Iso"),
    Year = sapply(results_list, `[[`, "Year")
  )
  
  # Create data matrices
  raw_df <- do.call(rbind, lapply(results_list, `[[`, "Raw"))
  gam_df <- do.call(rbind, lapply(results_list, `[[`, "GAM_Smoothed"))
  ma_df <- do.call(rbind, lapply(results_list, `[[`, "Moving_Avg"))
  sr88_df <- do.call(rbind, lapply(results_list, `[[`, "Sr88"))
  combined_df <- do.call(rbind, lapply(results_list, `[[`, "Combined"))
  
  # Add column names to data matrices
  colnames(raw_df) <- paste0("X", 1:ncol(raw_df))
  colnames(gam_df) <- paste0("X", 1:ncol(gam_df))
  colnames(ma_df) <- paste0("X", 1:ncol(ma_df))
  colnames(sr88_df) <- paste0("X", 1:ncol(sr88_df))
  colnames(combined_df) <- paste0("X", 1:ncol(combined_df))
  
  # Combine metadata with data matrices
  all_data_raw <- cbind(metadata, raw_df)
  all_data_gam <- cbind(metadata, gam_df)
  all_data_ma <- cbind(metadata, ma_df)
  all_data_sr88 <- cbind(metadata, sr88_df)
  all_data_combined <- cbind(metadata, combined_df)
  
  # Save to files
  landmark_str <- paste(landmark_filter, collapse = "_")
  filename_base <- paste0("Processed_", landmark_str)
  output_dir <- here("Data/02_Preprocessed_ts_matrices")
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  write.csv(all_data_raw, file = file.path(output_dir, paste0(filename_base, "_RAW.csv")), row.names = FALSE)
  write.csv(all_data_gam, file = file.path(output_dir, paste0(filename_base, "_GAM.csv")), row.names = FALSE)
  write.csv(all_data_ma, file = file.path(output_dir, paste0(filename_base, "_MA.csv")), row.names = FALSE)
  write.csv(all_data_sr88, file = file.path(output_dir, paste0(filename_base, "_Sr88.csv")), row.names = FALSE)
  write.csv(all_data_combined, file = file.path(output_dir, paste0(filename_base, "_Combined.csv")), row.names = FALSE)
  
  message("Processed data saved to: ", output_dir)
  return(invisible(results_list))
}

# Process all landmark combinations
process_all_combinations <- function() {
  # Core only
  message("Processing Core landmarks...")
  process_otolith_data(landmark_filter = c("Core"))
  
  # Fw only
  message("Processing Fw landmarks...")
  process_otolith_data(landmark_filter = c("Fw"))
  
  # Core+Fw
  message("Processing Core+Fw landmarks...")
  process_otolith_data(landmark_filter = c("Core", "Fw"))
  
  message("All preprocessing completed!")
}

process_all_combinations()
