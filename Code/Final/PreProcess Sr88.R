### This script will prepare Sr:Ca Transects for analysis alongside Sr8786 

process_Sr88 <- function(interp_points = 1000, landmark_filter = c("Core", "Fw")) {     
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
      if (all(is.na(ind_data$Sr88))) rep(NA, interp_points) 
      else approx(seq_len(nrow(ind_data)), ind_data$Sr88, seq(1, nrow(ind_data), length.out = interp_points), method = "linear", rule = 2)$y     
    }, error = function(e) { return(rep(NA, interp_points)) })         
    
    # Z-normalization
    z_normalized <- (interpolated - mean(interpolated, na.rm = TRUE)) / sd(interpolated, na.rm = TRUE)
    
    # Store results     
    list( Fish_id = fish_id, Watershed = watershed, Sr88_ZNorm = z_normalized, 
          Natal_Iso = natal_iso, Year = Year )   
  })     
  
  results_list <- Filter(Negate(is.null), results_list)     
  
  # Convert results into matrices   
  znorm_array <- do.call(rbind, lapply(results_list, `[[`, "Sr88_ZNorm"))     
  ids <- sapply(results_list, `[[`, "Fish_id")   
  watersheds <- sapply(results_list, `[[`, "Watershed")   
  natal_origins <- sapply(results_list, `[[`, "Natal_Iso")   
  years <- sapply(results_list, `[[`, "Year")     
  
  metadata <- tibble(Fish_id = ids, Watershed = watersheds, Natal_Iso = natal_origins, Year = years)
  
  # Define filenames dynamically based on parameters
  landmark_str <- paste(landmark_filter, collapse = "_")
  filename_base <- paste0("Processed_", landmark_str, "_Sr88_ZNorm")
  
  # Save Z-normalized data
  all_data_combined_ZNorm <- cbind(metadata, znorm_array)
  write.csv(all_data_combined_ZNorm, file = here(paste0("Data/Processed/Preprocessed_ts_matrices/", filename_base, ".csv")), row.names = FALSE)
  
  # Return processed data as a list
  list(
    znorm_array = znorm_array,     
    ids = ids,     
    watersheds = watersheds,     
    natal_origins = natal_origins,     
    Year = years   
  )
}

preprocessed_Sr88 <- process_Sr88( landmark_filter = c("Core"))
