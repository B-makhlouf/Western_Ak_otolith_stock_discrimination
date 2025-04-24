library(data.table)
library(ggplot2)
library(mgcv)
library(zoo)
library(dplyr)
library(here)
library(progress)  # For progress bar

process_trimmed_data <- function(window_size = 50, gamma_value = .7, landmark_filter = c("Core", "Fw")) {
  
  # List all files in the Landmarks directory
  landmark_files <- list.files("Data/Processed_ts/Landmarks", pattern = "*.csv", full.names = TRUE)
  
  # Initialize a list to store results for each file
  results_list <- list()
  
  # Initialize progress bar
  pb <- progress_bar$new(
    format = "Processing [:bar] :percent (:eta remaining)",
    total = length(landmark_files),
    clear = FALSE,
    width = 60
  )
  
  # Initialize vectors to store the number of reads and valid files
  reads_count <- numeric(length(landmark_files))
  valid_files <- logical(length(landmark_files))
  
  # First pass: calculate average length of valid sections
  for (i in seq_along(landmark_files)) {
    file <- landmark_files[i]
    ind_data <- tryCatch({
      read.csv(file)
    }, error = function(e) NULL)
    
    if (is.null(ind_data)) next
    
    # Filter data based on landmarks
    if ("Fw" %in% landmark_filter) {
      fw_max_microns <- ind_data %>% 
        filter(Landmark == "Fw") %>% 
        summarise(max_microns = max(Microns, na.rm = TRUE)) %>% 
        pull(max_microns)
      
      ind_data_filtered <- ind_data %>% 
        filter(Landmark %in% landmark_filter)
      
      #extra microns 
      extra<- ind_data %>%
        filter(Microns>=fw_max_microns & Microns<= (fw_max_microns + 300))
      
      # Add to the filtered data
      ind_data_filtered <- rbind(ind_data_filtered, extra)
      
    } else {
      ind_data_filtered <- ind_data %>% 
        filter(Landmark %in% landmark_filter)
    }
    
    # Only count if we have enough data points
    if (nrow(ind_data_filtered) >= 2 && sum(!is.na(ind_data_filtered$Iso)) >= 2) {
      reads_count[i] <- nrow(ind_data_filtered)
      valid_files[i] <- TRUE
    }
  }
  
  # Filter out invalid files
  landmark_files <- landmark_files[valid_files]
  reads_count <- reads_count[valid_files]
  
  if (length(reads_count) == 0) {
    stop("No valid files found with the specified landmarks")
  }
  
  # Calculate average number of reads (minimum 2)
  avg_reads <- max(round(mean(reads_count)), 2)
  
  # Second pass: process valid files
  for (file in landmark_files) {
    pb$tick()
    
    ind_data <- read.csv(file)
    
    # Extract metadata
    metadata <- list(
      Fish_id = ind_data$Fish_id[1],
      Watershed = ind_data$Watershed[1],
      Natal_Iso = ind_data$natal_origin_iso[1],
      Year = ind_data$Year[1]
    )
    
    # Filter data based on landmarks (same as first pass)
    if ("Fw" %in% landmark_filter) {
      fw_max_microns <- ind_data %>% 
        filter(Landmark == "Fw") %>% 
        summarise(max_microns = max(Microns, na.rm = TRUE)) %>% 
        pull(max_microns)
      
      ind_data_filtered <- ind_data %>% 
        filter(Landmark %in% landmark_filter)
      
      #extra microns 
      extra<- ind_data %>%
        filter(Microns>=fw_max_microns & Microns<= (fw_max_microns + 200))
      
      # Add to the filtered data
      ind_data_filtered <- rbind(ind_data_filtered, extra)
      
    } else {
      ind_data_filtered <- ind_data %>% 
        filter(Landmark %in% landmark_filter)
    }
    
    
    # Skip if not enough data
    if (nrow(ind_data_filtered) < 2 || sum(!is.na(ind_data_filtered$Iso)) < 2) next
    
    ################### Data Processing ###################
    
    # Moving Average
    moving_avg <- rollapply(ind_data_filtered$Iso, width = window_size, FUN = mean, align = "center", fill = NA)
    
    
    ind_data_filtered$Moving_Avg <- moving_avg
    
    # GAM Smoothing
    gam_smoothed <- tryCatch({
      df <- data.frame(
        Microns = which(!is.na(ind_data_filtered$Iso)),
        Iso = ind_data_filtered$Iso[!is.na(ind_data_filtered$Iso)]
      )
      k <- 30 #min(50, floor(15 * (nrow(df)^(2/9))))
      model <- gam(Iso ~ s(Microns, bs = "tp", k = k), gamma = gamma_value, data = df)
      predict(model, newdata = data.frame(Microns = seq_len(nrow(ind_data_filtered))))
    }, error = function(e) rep(NA, nrow(ind_data_filtered)))
    
    ind_data_filtered$GAM_smoothed <- gam_smoothed
    
    # Sr88 Processing
    
    # Extract Sr88
    Sr88 <- ind_data_filtered$Sr88
    
    # Find the index of the last FW landmark
    last_fw <- max(which(ind_data_filtered$Landmark == "Fw"))
    
    # Get the Sr88 value at the last FW landmark
    sr88_last_fw <- ind_data_filtered$Sr88[last_fw]
    
    # Find the minimum Sr88 value
    min_sr88 <- min(ind_data_filtered$Sr88, na.rm = TRUE)
    
    # if Fw is one of the landmarks.. 
    if ("Fw" %in% landmark_filter) {
      
      # Scale Sr88 so that min_sr88 is 0 and sr88_last_fw is 1
      ind_data_filtered$Sr88 <- (ind_data_filtered$Sr88 - min_sr88) / (sr88_last_fw - min_sr88)
      
    } else {
      # Z normalize 
      ind_data_filtered$Sr88 <- (ind_data_filtered$Sr88 - min_sr88) / (max(ind_data_filtered$Sr88, na.rm = TRUE) - min_sr88)
      
    }
    
  
    # Run a moving average through Sr88
    moving_avg_sr88 <- rollapply(ind_data_filtered$Sr88, width = window_size, FUN = mean, align = "center", fill = NA)
    
    ind_data_filtered$Moving_Avg_Sr88 <- moving_avg_sr88
    ################### Interpolation ###################
    
    # Safe interpolation function
    safe_interpolate <- function(x, n) {
      if (sum(!is.na(x)) < 2) return(rep(NA, n))
      approx(seq_along(x), x, n = n, method = "linear", rule = 2)$y
    }
    
    # Interpolate all series
    interpolated_data <- list(
      GAM_Smoothed = safe_interpolate(ind_data_filtered$GAM_smoothed, avg_reads),
      Moving_Avg = safe_interpolate(ind_data_filtered$Moving_Avg, avg_reads),
      Sr88 = safe_interpolate(ind_data_filtered$Moving_Avg_Sr88, avg_reads), 
      Sr8786 = safe_interpolate(ind_data_filtered$Iso, avg_reads)
    )
    
    plot_df <- data.frame(
      Position = seq_len(avg_reads),  # Interpolated positions
      Raw_Sr8786 = interpolated_data$Sr8786,
      GAM_Smoothed = interpolated_data$GAM_Smoothed,
      Moving_Avg = interpolated_data$Moving_Avg
    )
    
    # Create the plot
    p <- ggplot(plot_df, aes(x = Position)) +
      geom_point(aes(y = Raw_Sr8786), color = "white", alpha = 0.5, size = 1) +
      geom_line(aes(y = GAM_Smoothed), color = "red", linewidth = 1) +
      geom_line(aes(y = Moving_Avg), color = "blue", linewidth = 1) +
      labs(title = paste("Fish ID:", metadata$Fish_id, "(Interpolated)"),
           subtitle = paste("Watershed:", metadata$Watershed),
           x = "Relative Position", y = "Sr87/86") +
      theme_minimal() +
      theme(plot.title = element_text(size = 10),
            plot.subtitle = element_text(size = 8))
    
    # Save the plot
    plot_filename <- paste0("Diagnostic Plots/Interpolated_Fish_", metadata$Fish_id, "_", 
                            paste(landmark_filter, collapse = "_"), ".png")
    ggsave(here(plot_filename), plot = p, width = 8, height = 6, dpi = 300)
    
    
    # Store results
    results_list[[file]] <- c(metadata, interpolated_data)
  }
  
  # Combine results (only if we have valid data)
  if (length(results_list) > 0) {
    metadata <- data.frame(
      Fish_id = sapply(results_list, `[[`, "Fish_id"),
      Watershed = sapply(results_list, `[[`, "Watershed"),
      Natal_Iso = sapply(results_list, `[[`, "Natal_Iso"),
      Year = sapply(results_list, `[[`, "Year")
    )
    
    # Create output matrices
    output_data <- list(
      GAM = cbind(metadata, do.call(rbind, lapply(results_list, `[[`, "GAM_Smoothed"))),
      MA = cbind(metadata, do.call(rbind, lapply(results_list, `[[`, "Moving_Avg"))),
      Sr88 = cbind(metadata, do.call(rbind, lapply(results_list, `[[`, "Sr88"))),
      RAWSr8786 = cbind(metadata, do.call(rbind, lapply(results_list, `[[`, "Sr8786")))
    )
    
    # Save outputs
    landmark_str <- paste(landmark_filter, collapse = "_")
    dir.create(here("Data/Preprocessed_ts_matrices"), showWarnings = FALSE, recursive = TRUE)
    
    for (type in names(output_data)) {
      write.csv(
        output_data[[type]],
        file = here(paste0("Data/Preprocessed_ts_matrices/Processed_", landmark_str, "_", type, ".csv")),
        row.names = FALSE
      )
    }
  } else {
    warning("No valid data to process after filtering")
  }
}

# Example usage:
process_trimmed_data(landmark_filter = "Core")
process_trimmed_data(landmark_filter = "Fw")
process_trimmed_data(landmark_filter = c("Core", "Fw"))

