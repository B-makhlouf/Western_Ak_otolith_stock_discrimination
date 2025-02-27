
#### This function takes all the raw data files in the Tim_Locations folder (which have been given a trim location)
# And processes them to be used in Classifier, PCA, DTW, etc. 
library(data.table)
library(mgcv)
library(zoo)
library(progressr)

library(pbapply)  # For progress bar
install.packages("pbapply")

process_trimmed_data <- function(interp_points = 1000, window_size = 60, gamma_value = 1.4) {
  
  data_directory <- "Data/Processed/Trim_Locations"
  files <- list.files(data_directory, full.names = TRUE, pattern = "\\.csv$")
  
  # Use pblapply() to track progress
  results_list <- pblapply(files, function(file_path) {
    
    ind_data <- tryCatch({
      fread(file_path)
    }, error = function(e) {
      message("Error reading file: ", e$message)
      return(NULL)
    })
    
    if (is.null(ind_data) || nrow(ind_data) < 2) return(NULL)  # Skip invalid files
    
    # Extract metadata efficiently
    watershed <- ind_data$Watershed[1]
    natal_iso <- ind_data$natal_origin_iso[1]
    fish_id <- ind_data$Fish_id[1]
    Year <- ind_data$Year[1]
    
    # Interpolation
    interpolated <- tryCatch({
      if (all(is.na(ind_data$Iso))) {
        rep(NA, interp_points)
      } else {
        approx(
          x = seq_len(nrow(ind_data)),
          y = ind_data$Iso,
          xout = seq(1, nrow(ind_data), length.out = interp_points),
          method = "linear",
          rule = 2
        )$y
      }
    }, error = function(e) {
      message("Error in interpolation: ", e$message)
      return(rep(NA, interp_points))
    })
    
    # Moving Average Calculation
    moving_avg <- rollapply(interpolated, width = window_size, FUN = mean, align = "center", fill = NA)
    
    # GAM Smoothing
    gam_smoothed <- tryCatch({
      valid_idx <- !is.na(interpolated)
      if (sum(valid_idx) > 2) {  
        df <- data.frame(Microns = which(valid_idx), Iso = interpolated[valid_idx])
        
        # Dynamic k calculation
        k <- floor(15 * (nrow(df)^(2/9)))
        
        model <- gam(Iso ~ s(Microns, bs = "tp", k = k), gamma = gamma_value, data = df)
        
        predict(model, newdata = data.frame(Microns = seq_len(interp_points)))
      } else {
        rep(NA, interp_points)
      }
    }, error = function(e) {
      message("Error in GAM smoothing: ", e$message)
      return(rep(NA, interp_points))
    })
    
    # Store results in a list
    list(
      Fish_id = fish_id,
      Watershed = watershed,
      Iso = interpolated,
      Moving_Avg = moving_avg,
      GAM_Smoothed = gam_smoothed,
      Natal_Iso = natal_iso,
      Year = Year
    )
  })
  
  # Remove NULLs
  results_list <- Filter(Negate(is.null), results_list)
  
  # Convert results into matrices for efficiency
  measurement_array <- do.call(rbind, lapply(results_list, `[[`, "Iso"))
  moving_avg_array <- do.call(rbind, lapply(results_list, `[[`, "Moving_Avg"))
  gam_smoothed_array <- do.call(rbind, lapply(results_list, `[[`, "GAM_Smoothed"))
  
  ids <- sapply(results_list, `[[`, "Fish_id")
  watersheds <- sapply(results_list, `[[`, "Watershed")
  natal_origins <- sapply(results_list, `[[`, "Natal_Iso")
  years <- sapply(results_list, `[[`, "Year")
  
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

