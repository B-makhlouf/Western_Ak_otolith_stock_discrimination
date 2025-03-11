library(data.table)
library(mgcv)
library(zoo)
library(dplyr)
library(here)
library(progress)  # For progress bar

process_trimmed_data <- function(window_size = 60, gamma_value = .8, landmark_filter = c("Core", "Fw")) {
  
  # List all files in the Landmarks directory
  landmark_files <- list.files("Data/Processed/Landmarks", pattern = "*.csv", full.names = TRUE)
  
  # Initialize a list to store results for each file
  results_list <- list()
  
  # Initialize progress bar
  pb <- progress_bar$new(
    format = "Processing [:bar] :percent (:eta remaining)",
    total = length(landmark_files),  # Total number of files to process
    clear = FALSE,  # Keep the progress bar after completion
    width = 60  # Width of the progress bar
  )
  
  # Initialize a vector to store the number of reads in the filtered section for each file
  reads_count <- numeric(length(landmark_files))
  
  # Loop through each file to calculate the number of reads in the filtered section
  for (i in seq_along(landmark_files)) {
    file <- landmark_files[i]
    
    # Read the data
    ind_data <- read.csv(file)
    
    # Identify the maximum Microns value for the "Fw" landmark
    fw_max_microns <- ind_data %>% 
      filter(Landmark == "Fw") %>% 
      summarise(max_microns = max(Microns, na.rm = TRUE)) %>% 
      pull(max_microns)
    
    # Collect all reads within the specified landmarks and the next 300 microns after "Fw"
    ind_data_extended <- ind_data %>% 
      filter(Landmark %in% landmark_filter | (Microns > fw_max_microns & Microns <= fw_max_microns + 300))
    
    # Store the number of reads in the filtered section
    reads_count[i] <- nrow(ind_data_extended)
  }
  
  # Calculate the average number of reads in the filtered section across all files
  avg_reads <- round(mean(reads_count))
  
  # Loop through each file again to process the data
  for (file in landmark_files) {
    
    # Increment progress bar
    pb$tick()
    
    # Read the data
    ind_data <- read.csv(file)
    
    # Extract metadata
    watershed <- ind_data$Watershed[1]
    natal_iso <- ind_data$natal_origin_iso[1]
    fish_id <- ind_data$Fish_id[1]
    Year <- ind_data$Year[1]
    
    ################### Sr8786 MA and GAM 
    
    # Calculate a moving average 
    moving_avg <- rollapply(ind_data$Iso, width = window_size, FUN = mean, align = "center", fill = NA)
    
    # Add MA to the data frame
    ind_data$Moving_Avg <- moving_avg
    
    # Apply GAM smoothing
    df <- data.frame(Microns = which(!is.na(ind_data$Iso)), Iso = ind_data$Iso[!is.na(ind_data$Iso)])
    k <- 50#floor(10 * (nrow(df)^(2/9)))
    
    gam_value <- gamma_value
    model <- gam(Iso ~ s(Microns, bs = "tp", k = k), gamma = gam_value, data = df)
    
    ind_data$GAM_smoothed <- predict(model, newdata = data.frame(Microns = seq_len(nrow(ind_data))))
    
    ################# Sr88
    
    # Extract Sr88
    Sr88 <- ind_data$Sr88
    
    # Find the index of the last FW landmark
    last_fw <- max(which(ind_data$Landmark == "Fw"))
    
    # Get the Sr88 value at the last FW landmark
    sr88_last_fw <- ind_data$Sr88[last_fw]
    
    # Find the minimum Sr88 value
    min_sr88 <- min(ind_data$Sr88, na.rm = TRUE)
    
    # Scale Sr88 so that min_sr88 is 0 and sr88_last_fw is 1
    ind_data$Sr88 <- (ind_data$Sr88 - min_sr88) / (sr88_last_fw - min_sr88)
    
    # Run a moving average through Sr88
    moving_avg_sr88 <- rollapply(ind_data$Sr88, width = window_size, FUN = mean, align = "center", fill = NA)
    
    # Get moving average value at last FW
    sr88_last_fw_ma <- moving_avg_sr88[last_fw]
    
    # Find the minimum moving average Sr88 value
    min_sr88_ma <- min(moving_avg_sr88, na.rm = TRUE)
    
    # Scale Moving_Avg_Sr88 so that min_sr88_ma is 0 and sr88_last_fw_ma is 1
    ind_data$Moving_Avg_Sr88 <- (moving_avg_sr88 - min_sr88_ma) / (sr88_last_fw_ma - min_sr88_ma)
    
    ################################################################################ Combined Sr8786 and Sr88 
    
    # Extract the gradient values from Sr88 
    grad_sr88 <- diff(ind_data$Moving_Avg_Sr88) / diff(ind_data$Microns)
    
    Iso <- ind_data$GAM_smoothed
    
    # Remove the last read from the GAM
    Iso <- Iso[-length(Iso)]
    
    # Set any NA in the gradient to 0 
    grad_sr88[is.na(grad_sr88)] <- 0
    
    # Scale gradient to be from 1 to 5
    grad_sr88 <- (grad_sr88 - min(grad_sr88)) / (max(grad_sr88) - min(grad_sr88)) * 4 + 1
    
    # Multiply Gradient and Sr8786 
    combined <- Iso / grad_sr88
    
    # Add to dataset 
    ind_data$Combined <- c(NA, combined)
    
    ################################################################################# TRIM 
    
    # Calculate the microns at the end of the dataset 
    end_microns <- max(ind_data$Microns, na.rm = TRUE)
    
    # Calculate the microns at the end of the FW region 
    fw_end_microns <- max(ind_data$Microns[ind_data$Landmark == "Fw"], na.rm = TRUE)
    
    # Find the difference between the end of the dataset and the end of the FW region
    fw_end_diff <- end_microns - fw_end_microns
    
    
    # Identify the maximum Microns value for the "Fw" landmark
    fw_max_microns <- ind_data %>% 
      filter(Landmark == "Fw") %>% 
      summarise(max_microns = max(Microns, na.rm = TRUE)) %>% 
      pull(max_microns)
    
    # Collect all reads within the specified landmarks and the next 300 microns after "Fw"
    ind_data_extended <- ind_data %>% 
      filter(Landmark %in% landmark_filter | (Microns > fw_max_microns & Microns <= fw_max_microns + 300))
    
    # Interpolate ind_data_extended to the average number of reads 
    interpolated_GAM <- approx(
      seq_len(nrow(ind_data_extended)), 
      ind_data_extended$GAM_smoothed, 
      seq(1, nrow(ind_data_extended), length.out = avg_reads), 
      method = "linear", 
      rule = 2
    )$y
    
    # Interpolate the MA 
    interpolated_MA <- approx(
      seq_len(nrow(ind_data_extended)), 
      ind_data_extended$Moving_Avg, 
      seq(1, nrow(ind_data_extended), length.out = avg_reads), 
      method = "linear", 
      rule = 2
    )$y
    
    # Interpolate the Sr88
    interpolated_Sr88 <- approx(
      seq_len(nrow(ind_data_extended)), 
      ind_data_extended$Moving_Avg_Sr88, 
      seq(1, nrow(ind_data_extended), length.out = avg_reads), 
      method = "linear", 
      rule = 2
    )$y
    
    # Interpolate the Combined
    interpolated_Combined <- approx(
      seq_len(nrow(ind_data_extended)), 
      ind_data_extended$Combined, 
      seq(1, nrow(ind_data_extended), length.out = avg_reads), 
      method = "linear", 
      rule = 2
    )$y
    
# Store results for this file
results_list[[file]] <- list(
  Fish_id = fish_id,
  Watershed = watershed,
  Natal_Iso = natal_iso,
  Year = Year,
  GAM_Smoothed = interpolated_GAM,
  Moving_Avg = interpolated_MA,
  Sr88 = interpolated_Sr88,
  Combined = interpolated_Combined
)
  }
  
  # Combine results into data frames
  metadata <- data.frame(
    Fish_id = sapply(results_list, `[[`, "Fish_id"),
    Watershed = sapply(results_list, `[[`, "Watershed"),
    Natal_Iso = sapply(results_list, `[[`, "Natal_Iso"),
    Year = sapply(results_list, `[[`, "Year")
  )
  
  # Create data frames for each type of timeseries data
  gam_smoothed_df <- do.call(rbind, lapply(results_list, `[[`, "GAM_Smoothed"))
  moving_avg_df <- do.call(rbind, lapply(results_list, `[[`, "Moving_Avg"))
  sr88_df <- do.call(rbind, lapply(results_list, `[[`, "Sr88"))
  combined_df <- do.call(rbind, lapply(results_list, `[[`, "Combined"))
  
  # Combine metadata with timeseries data
  all_data_combined_GAM <- cbind(metadata, gam_smoothed_df)
  all_data_combined_MA <- cbind(metadata, moving_avg_df)
  all_data_combined_Sr88 <- cbind(metadata, sr88_df)
  all_data_combined_Combined <- cbind(metadata, combined_df)
  
  # Define filenames dynamically based on parameters
  landmark_str <- paste(landmark_filter, collapse = "_")
  filename_base <- paste0("Processed_", landmark_str)
  
  # Save GAM smoothed data
  write.csv(all_data_combined_GAM, file = here(paste0("Data/Processed/Preprocessed_ts_matrices/", filename_base, "_GAM.csv")), row.names = FALSE)
  
  # Save moving average data
  write.csv(all_data_combined_MA, file = here(paste0("Data/Processed/Preprocessed_ts_matrices/", filename_base, "_MA.csv")), row.names = FALSE)
  
  # Save Sr88 data
  write.csv(all_data_combined_Sr88, file = here(paste0("Data/Processed/Preprocessed_ts_matrices/", filename_base, "_Sr88.csv")), row.names = FALSE)
  
  # Save Combined data
  write.csv(all_data_combined_Combined, file = here(paste0("Data/Processed/Preprocessed_ts_matrices/", filename_base, "_Combined.csv")), row.names = FALSE)
  
}

# Run the function
process_trimmed_data()

