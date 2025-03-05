






process_Sr88 <- function(interp_points = 1000, landmark_filter = c("Fw")) {  
  data_directory <- "Data/Processed/Landmarks"  
  files <- list.files(data_directory, full.names = TRUE, pattern = "\\.csv$")  
  
  results_list <- list()
  
  for (file_path in files) {
    print(paste("Processing file:", file_path))
    
    ind_data <- tryCatch({
      fread(file_path)
    }, error = function(e) {
      print(paste("Error reading file:", file_path))
      return(NULL)
    })
    
    if (is.null(ind_data) || nrow(ind_data) < 2) next  
    
    # Extract metadata
    watershed <- ind_data$Watershed[1]  
    natal_iso <- ind_data$natal_origin_iso[1]  
    fish_id <- ind_data$Fish_id[1]  
    Year <- ind_data$Year[1]  
   
    # calculate the microns at the end of the dataset 
    end_microns <- max(ind_data$Microns, na.rm = TRUE)
    
    #calculate the microns at the end of the fw region 
    fw_end_microns <- max(ind_data$Microns[ind_data$Landmark == "Fw"], na.rm = TRUE)
    
    # find the difference between the end of the dataset and the end of the fw region
    fw_end_diff <- end_microns - fw_end_microns
    
    # if the difference is less than 200, skip 
    
    if (fw_end_diff < 150) {
      next
    }
    
    
    # Identify the maximum Microns value for the "Fw" landmark
    fw_max_microns <- ind_data %>% 
      filter(Landmark == "Fw") %>% 
      summarise(max_microns = max(Microns, na.rm = TRUE)) %>% 
      pull(max_microns)
  
    # Collect all reads within the specified landmarks and the next 300 microns after "Fw"
    ind_data_extended <- ind_data %>% 
      filter(Landmark %in% landmark_filter | (Microns > fw_max_microns & Microns <= fw_max_microns + 150))
    
    
    # Find the index of the last FW landmark
    last_fw <- max(which(ind_data_extended$Landmark == "Fw"))
    
    # Get the Sr88 value at the last FW landmark
    sr88_last_fw <- ind_data_extended$Sr88[last_fw]
    
    # Find the minimum Sr88 value
    min_sr88 <- min(ind_data_extended$Sr88, na.rm = TRUE)
    
    # Scale Sr88 so that min_sr88 is 0 and sr88_last_fw is 1
    ind_data_extended$Sr88 <- (ind_data_extended$Sr88 - min_sr88) / (sr88_last_fw - min_sr88)
    
    
    # Interpolation
    interpolated <- tryCatch({
      if (all(is.na(ind_data_extended$Sr88)) || length(unique(na.omit(ind_data_extended$Sr88))) < 2) {
        rep(NA, interp_points)
      } else {
        approx(seq_len(nrow(ind_data_extended)), ind_data_extended$Sr88, 
               seq(1, nrow(ind_data_extended), length.out = interp_points), 
               method = "linear", rule = 2)$y
      }
    }, error = function(e) {
      print("Interpolation error")
      return(rep(NA, interp_points))
    })
    
    # Store results
    results_list[[length(results_list) + 1]] <- list(
      Fish_id = fish_id, Watershed = watershed, Sr88_ZNorm = interpolated,
      Natal_Iso = natal_iso, Year = Year
    )
  }
  
  # Combine results
  if (length(results_list) == 0) return(NULL)
  
  znorm_array <- do.call(rbind, lapply(results_list, `[[`, "Sr88_ZNorm"))
  ids <- sapply(results_list, `[[`, "Fish_id")
  watersheds <- sapply(results_list, `[[`, "Watershed")
  natal_origins <- sapply(results_list, `[[`, "Natal_Iso")
  years <- sapply(results_list, `[[`, "Year")
  
  metadata <- tibble(Fish_id = ids, Watershed = watersheds, Natal_Iso = natal_origins, Year = years)
 
  # Save results
  landmark_str <- paste(landmark_filter, collapse = "_")
  filename_base <- paste0("Processed_", landmark_str, "_Sr88")
  output_dir <- "Data/Processed/Preprocessed_ts_matrices"
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  all_data_combined_ZNorm <- cbind(metadata, znorm_array)
  write.csv(all_data_combined_ZNorm, file = file.path(output_dir, paste0(filename_base, ".csv")), row.names = FALSE)
  print(paste("Saved file:", file.path(output_dir, paste0(filename_base, ".csv"))))
  
  return(list(znorm_array = znorm_array, ids = ids, watersheds = watersheds, natal_origins = natal_origins, Year = years))
}
# test params
interp_points = 1000
landmark_filter = c("Fw")

process_Sr88(interp_points = interp_points, landmark_filter = landmark_filter)
