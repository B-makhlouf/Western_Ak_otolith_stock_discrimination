library(ggplot2)
library(dplyr)
library(data.table)

process_Sr88_Iso_concatenated <- function(interp_points = 1000, landmark_filter = c("Core", "Fw"), plot_fish_id = NULL) {  
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
    
    end_microns <- max(ind_data$Microns, na.rm = TRUE)
    fw_end_microns <- max(ind_data$Microns[ind_data$Landmark == "Fw"], na.rm = TRUE)
    fw_end_diff <- end_microns - fw_end_microns
    if (fw_end_diff < 100) next
    
    fw_max_microns <- ind_data %>% 
      filter(Landmark == "Fw") %>% 
      summarise(max_microns = max(Microns, na.rm = TRUE)) %>% 
      pull(max_microns)
    
    # Collect the exact same subset of data for both Sr88 and Iso
    ind_data_extended <- ind_data %>% 
      filter(Landmark %in% landmark_filter | (Microns > fw_max_microns & Microns <= fw_max_microns + 100))
    
    last_fw <- max(which(ind_data_extended$Landmark == "Fw"))
    sr88_last_fw <- ind_data_extended$Sr88[last_fw]
    min_sr88 <- min(ind_data_extended$Sr88, na.rm = TRUE)
    ind_data_extended$Sr88 <- ((ind_data_extended$Sr88 - min_sr88) / (sr88_last_fw - min_sr88))
    
    # **Interpolation using the SAME sequence**
    interp_indices <- seq(1, nrow(ind_data_extended), length.out = interp_points)
    
    interpolated_Sr88 <- tryCatch({
      if (all(is.na(ind_data_extended$Sr88)) || length(unique(na.omit(ind_data_extended$Sr88))) < 2) {
        rep(NA, interp_points)
      } else {
        approx(seq_len(nrow(ind_data_extended)), ind_data_extended$Sr88, 
               interp_indices, method = "linear", rule = 2)$y
      }
    }, error = function(e) {
      print("Interpolation error for Sr88")
      return(rep(NA, interp_points))
    })
    
    interpolated_Iso <- tryCatch({
      if (all(is.na(ind_data_extended$Iso)) || length(unique(na.omit(ind_data_extended$Iso))) < 2) {
        rep(NA, interp_points)
      } else {
        approx(seq_len(nrow(ind_data_extended)), ind_data_extended$Iso, 
               interp_indices, method = "linear", rule = 2)$y
      }
    }, error = function(e) {
      print("Interpolation error for Iso")
      return(rep(NA, interp_points))
    })
    
    # Concatenate Sr88 and Iso values
    concatenated_values <- c(interpolated_Sr88, interpolated_Iso)
    
    # Store results
    results_list[[length(results_list) + 1]] <- list(
      Fish_id = fish_id, Watershed = watershed, 
      Sr88_ZNorm = interpolated_Sr88, Iso = interpolated_Iso, 
      Concatenated = concatenated_values,
      Natal_Iso = natal_iso, Year = Year
    )
    
    # 🟢 **Plot Timeseries for Selected FishID**
    if (!is.null(plot_fish_id) && fish_id == plot_fish_id) {
      print(paste("Plotting timeseries for Fish ID:", fish_id))
      
      plot_data <- data.frame(
        Index = 1:interp_points,
        Sr88_ZNorm = interpolated_Sr88,
        Iso = interpolated_Iso,
        Concatenated = concatenated_values
      ) %>% pivot_longer(cols = c(Sr88_ZNorm, Iso, Concatenated), names_to = "Variable", values_to = "Value")
      
      ggplot(plot_data, aes(x = Index, y = Value, color = Variable)) +
        geom_line(size = 1) +
        theme_minimal() +
        scale_color_manual(values = c("steelblue2", "tomato2", "chocolate1")) +
        labs(title = paste("Fish ID:", fish_id),
             x = "Time Index",
             y = "Value",
             color = "Variable")
    }
  }
  
  if (length(results_list) == 0) return(NULL)
  
  znorm_array <- do.call(rbind, lapply(results_list, `[[`, "Sr88_ZNorm"))
  iso_array <- do.call(rbind, lapply(results_list, `[[`, "Iso"))
  concatenated_array <- do.call(rbind, lapply(results_list, `[[`, "Concatenated"))
  
  ids <- sapply(results_list, `[[`, "Fish_id")
  watersheds <- sapply(results_list, `[[`, "Watershed")
  natal_origins <- sapply(results_list, `[[`, "Natal_Iso")
  years <- sapply(results_list, `[[`, "Year")
  
  metadata <- tibble(Fish_id = ids, Watershed = watersheds, Natal_Iso = natal_origins, Year = years)
  
  # Save results
  landmark_str <- paste(landmark_filter, collapse = "_")
  filename_base <- paste0("Processed_", landmark_str, "_Sr88_Iso")
  output_dir <- "/Users/benjaminmakhlouf/Research_repos/Western_Ak_otolith_stock_discrimination/Data/Classification_ts_matrices/Sr88"
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  all_data_combined <- cbind(metadata, concatenated_array)
  write.csv(all_data_combined, file = file.path(output_dir, paste0(filename_base, ".csv")), row.names = FALSE)
  print(paste("Saved file:", file.path(output_dir, paste0(filename_base, ".csv"))))
  
  return(list(Sr88_ZNorm = znorm_array, Iso = iso_array, Concatenated = concatenated_array, 
              ids = ids, watersheds = watersheds, natal_origins = natal_origins, Year = years))
}




Processed_data<- process_Sr88_Iso_concatenated(interp_points = 1000, landmark_filter = c("Core","Fw"))
