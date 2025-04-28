#' Preprocess Otolith Isotope Time Series Data
#' 
#' This function processes raw otolith ICPMS data files from the Landmarks directory,
#' normalizes them, and exports multiple processed datasets in consistent formats.
#' 
#' @param window_size Window size for moving average calculations (default = 60)
#' @param gamma_value Smoothing parameter for GAM fitting (default = 0.8)
#' @param landmark_filter Vector of landmark names to filter data (default = c("Core", "Fw"))
#' @param post_fw_extension Microns to include after Fw landmark (default = 300)
#' @param min_marine_size Minimum marine region size required (default = 100 microns)
#' @param output_dir Directory to save processed files (default = "Data/Processed/Preprocessed_ts_matrices")
#' 
#' @return Invisibly returns a list containing the processed data matrices and metadata
#' 
#' @examples
#' # Process with default settings
#' process_trimmed_data()
#' 
#' # Process with custom settings
#' process_trimmed_data(window_size = 40, gamma_value = 0.6, landmark_filter = c("Fw"))
process_trimmed_data <- function(window_size = 60, 
                                 gamma_value = 0.8, 
                                 landmark_filter = c("Core", "Fw"),
                                 post_fw_extension = 300,
                                 min_marine_size = 100,
                                 output_dir = "Data/Processed/Preprocessed_ts_matrices") {
  
  # Validate inputs
  validate_inputs(window_size, gamma_value, landmark_filter, post_fw_extension, min_marine_size, output_dir)
  
  # Create output directory if it doesn't exist
  ensure_output_dir(output_dir)
  
  # Process all landmark files and collect summary statistics
  landmark_files <- list_landmark_files()
  log_info(paste("Found", length(landmark_files), "landmark files to process"))
  
  # Calculate average read count for interpolation
  avg_reads <- calculate_average_reads(landmark_files, landmark_filter, post_fw_extension)
  log_info(paste("Using", avg_reads, "points for interpolation"))
  
  # Process each file
  results_list <- process_all_files(landmark_files, window_size, gamma_value, landmark_filter,
                                    post_fw_extension, min_marine_size, avg_reads)
  
  # Compile and export results
  export_processed_data(results_list, landmark_filter, output_dir)
  
  # Return results invisibly for further analysis if needed
  invisible(results_list)
}

#' Validate input parameters
#'
#' @param window_size Window size for moving average
#' @param gamma_value Smoothing parameter for GAM
#' @param landmark_filter Vector of landmark names
#' @param post_fw_extension Microns to include after Fw
#' @param min_marine_size Minimum marine region size
#' @param output_dir Output directory
validate_inputs <- function(window_size, gamma_value, landmark_filter, 
                            post_fw_extension, min_marine_size, output_dir) {
  if (!is.numeric(window_size) || window_size <= 0) {
    stop("window_size must be a positive number")
  }
  
  if (!is.numeric(gamma_value) || gamma_value <= 0) {
    stop("gamma_value must be a positive number")
  }
  
  if (!is.character(landmark_filter) || length(landmark_filter) == 0) {
    stop("landmark_filter must be a non-empty character vector")
  }
  
  if (!all(landmark_filter %in% c("Core", "Fw", "Early Marine"))) {
    warning("Some landmark filters may not exist in the data. Expected: 'Core', 'Fw', 'Early Marine'")
  }
  
  if (!is.numeric(post_fw_extension) || post_fw_extension < 0) {
    stop("post_fw_extension must be a non-negative number")
  }
  
  if (!is.numeric(min_marine_size) || min_marine_size < 0) {
    stop("min_marine_size must be a non-negative number")
  }
  
  if (!is.character(output_dir) || length(output_dir) != 1) {
    stop("output_dir must be a character string")
  }
}

#' Create output directory if it doesn't exist
#'
#' @param output_dir Output directory path
ensure_output_dir <- function(output_dir) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    log_info(paste("Created output directory:", output_dir))
  }
}

#' List all landmark files
#'
#' @return Character vector of file paths
list_landmark_files <- function() {
  files <- list.files("Data/Processed_ts/Landmarks", pattern = "*.csv", full.names = TRUE)
  if (length(files) == 0) {
    stop("No landmark files found in 'Data/Processed/Landmarks'")
  }
  return(files)
}

#' Calculate average read count for interpolation
#'
#' @param files List of file paths
#' @param landmark_filter Vector of landmark names
#' @param post_fw_extension Microns to include after Fw
#' @return Average number of reads
calculate_average_reads <- function(files, landmark_filter, post_fw_extension) {
  # Initialize progress bar
  pb <- progress_bar$new(
    format = "Calculating average reads [:bar] :percent",
    total = length(files),
    clear = FALSE,
    width = 60
  )
  
  # Initialize vector to store read counts
  reads_count <- numeric(length(files))
  
  # Process each file
  for (i in seq_along(files)) {
    pb$tick()
    
    # Read the data with error handling
    ind_data <- tryCatch({
      read.csv(files[i])
    }, error = function(e) {
      log_warning(paste("Error reading file:", files[i], "-", e$message))
      return(NULL)
    })
    
    if (is.null(ind_data)) {
      reads_count[i] <- NA
      next
    }
    
    # Extract filtered region
    filtered_region <- extract_region_of_interest(ind_data, landmark_filter, post_fw_extension)
    if (is.null(filtered_region)) {
      reads_count[i] <- NA
      next
    }
    
    # Store the number of reads
    reads_count[i] <- nrow(filtered_region)
  }
  
  # Calculate and return average
  avg_reads <- round(mean(reads_count, na.rm = TRUE))
  if (is.na(avg_reads) || avg_reads <= 0) {
    stop("Failed to calculate average read count. Check input data.")
  }
  
  return(avg_reads)
}

#' Extract region of interest from raw data
#'
#' @param ind_data Data frame containing raw otolith data
#' @param landmark_filter Vector of landmark names
#' @param post_fw_extension Microns to include after Fw
#' @return Filtered data frame or NULL if filtering failed
extract_region_of_interest <- function(ind_data, landmark_filter, post_fw_extension) {
  # Check for Fw landmark
  if (!"Fw" %in% ind_data$Landmark) {
    return(NULL)
  }
  
  # Identify the maximum Microns value for the "Fw" landmark
  fw_max_microns <- try({
    ind_data %>% 
      filter(Landmark == "Fw") %>% 
      summarise(max_microns = max(Microns, na.rm = TRUE)) %>% 
      pull(max_microns)
  }, silent = TRUE)
  
  if (inherits(fw_max_microns, "try-error") || is.na(fw_max_microns)) {
    return(NULL)
  }
  
  # Collect all reads within the specified landmarks and the extension after "Fw"
  ind_data_filtered <- ind_data %>% 
    filter(Landmark %in% landmark_filter | 
             (Microns > fw_max_microns & Microns <= fw_max_microns + post_fw_extension))
  
  return(ind_data_filtered)
}

#' Process all landmark files
#'
#' @param files List of file paths
#' @param window_size Window size for moving average
#' @param gamma_value Smoothing parameter for GAM
#' @param landmark_filter Vector of landmark names
#' @param post_fw_extension Microns to include after Fw
#' @param min_marine_size Minimum marine region size
#' @param avg_reads Average number of reads for interpolation
#' @return List of processed results
process_all_files <- function(files, window_size, gamma_value, landmark_filter,
                              post_fw_extension, min_marine_size, avg_reads) {
  # Initialize progress bar
  pb <- progress_bar$new(
    format = "Processing files [:bar] :percent (:eta remaining)",
    total = length(files),
    clear = FALSE,
    width = 60
  )
  
  # Initialize results list
  results_list <- list()
  processed_count <- 0
  skipped_count <- 0
  error_count <- 0
  
  # Process each file
  for (file in files) {
    pb$tick()
    
    result <- tryCatch({
      process_single_file(file, window_size, gamma_value, landmark_filter,
                          post_fw_extension, min_marine_size, avg_reads)
    }, error = function(e) {
      error_count <<- error_count + 1
      log_error(paste("Error processing file:", basename(file), "-", e$message))
      return(NULL)
    })
    
    if (is.null(result)) {
      next
    } else if (result$skipped) {
      skipped_count <- skipped_count + 1
    } else {
      results_list[[file]] <- result$data
      processed_count <- processed_count + 1
    }
  }
  
  # Log summary statistics
  log_info(paste("Successfully processed", processed_count, "files"))
  log_info(paste("Skipped", skipped_count, "files due to quality issues"))
  log_info(paste("Failed to process", error_count, "files due to errors"))
  
  if (length(results_list) == 0) {
    stop("No files were successfully processed. Check input data and parameters.")
  }
  
  return(results_list)
}

#' Process a single landmark file
#'
#' @param file_path Path to landmark file
#' @param window_size Window size for moving average
#' @param gamma_value Smoothing parameter for GAM
#' @param landmark_filter Vector of landmark names
#' @param post_fw_extension Microns to include after Fw
#' @param min_marine_size Minimum marine region size
#' @param avg_reads Average number of reads for interpolation
#' @return List containing processing result or NULL if processing failed
process_single_file <- function(file_path, window_size, gamma_value, landmark_filter,
                                post_fw_extension, min_marine_size, avg_reads) {
  # Read the data
  ind_data <- read.csv(file_path)
  
  # Extract metadata
  metadata <- extract_metadata(ind_data)
  if (is.null(metadata)) {
    log_warning(paste("Missing metadata in file:", basename(file_path)))
    return(list(skipped = TRUE))
  }
  
  
  
  # Calculate moving averages
  ind_data <- calculate_moving_averages(ind_data, window_size)
  
  # Apply GAM smoothing
  ind_data <- apply_gam_smoothing(ind_data, gamma_value)
  if (is.null(ind_data)) {
    log_warning(paste("GAM smoothing failed for:", metadata$fish_id))
    return(list(skipped = TRUE))
  }
  
  # Calculate scaled Sr88
  ind_data <- calculate_scaled_sr88(ind_data, window_size)
  if (is.null(ind_data)) {
    log_warning(paste("Sr88 scaling failed for:", metadata$fish_id))
    return(list(skipped = TRUE))
  }
  
  # Extract region of interest and interpolate
  interpolated_data <- interpolate_region_of_interest(
    ind_data, landmark_filter, post_fw_extension, avg_reads
  )
  
  if (is.null(interpolated_data)) {
    log_warning(paste("Interpolation failed for:", metadata$fish_id))
    return(list(skipped = TRUE))
  }
  
  # Return processed data
  return(list(
    skipped = FALSE,
    data = c(
      metadata,
      interpolated_data
    )
  ))
}

#' Extract metadata from otolith data
#'
#' @param ind_data Data frame containing raw otolith data
#' @return List of metadata fields
extract_metadata <- function(ind_data) {
  # Extract key metadata fields
  fish_id <- ind_data$Fish_id[1]
  watershed <- ind_data$Watershed[1]
  natal_iso <- ind_data$natal_origin_iso[1]
  year <- ind_data$Year[1]
  
  # Validate metadata
  if (is.na(fish_id) || is.na(watershed)) {
    return(NULL)
  }
  
  return(list(
    Fish_id = fish_id,
    Watershed = watershed,
    Natal_Iso = natal_iso,
    Year = year
  ))
}



#' Calculate moving averages for Sr8786
#'
#' @param ind_data Data frame containing raw otolith data
#' @param window_size Window size for moving average
#' @return Data frame with added moving average column
calculate_moving_averages <- function(ind_data, window_size) {
  # Calculate moving average for Iso
  moving_avg <- rollapply(ind_data$Iso, width = window_size, 
                          FUN = mean, align = "center", fill = NA)
  
  # Add to data frame
  ind_data$Moving_Avg <- moving_avg
  
  return(ind_data)
}

#' Apply GAM smoothing to isotope data
#'
#' @param ind_data Data frame containing raw otolith data
#' @param gamma_value Smoothing parameter for GAM
#' @return Data frame with added GAM smoothed column
apply_gam_smoothing <- function(ind_data, gamma_value) {
  # Prepare data for GAM
  df <- data.frame(
    Microns = which(!is.na(ind_data$Iso)), 
    Iso = ind_data$Iso[!is.na(ind_data$Iso)]
  )
  
  # Set spline complexity
  k <- 50  # Could be adjusted based on data size
  
  # Fit GAM model with error handling
  model <- try(
    gam(Iso ~ s(Microns, bs = "tp", k = k), gamma = gamma_value, data = df),
    silent = TRUE
  )
  
  if (inherits(model, "try-error")) {
    return(NULL)
  }
  
  # Predict smoothed values
  ind_data$GAM_smoothed <- predict(model, 
                                   newdata = data.frame(Microns = seq_len(nrow(ind_data))))
  
  return(ind_data)
}

#' Calculate scaled Sr88 values
#'
#' @param ind_data Data frame containing raw otolith data
#' @param window_size Window size for moving average
#' @return Data frame with added scaled Sr88 columns
calculate_scaled_sr88 <- function(ind_data, window_size) {
  # Check if Fw landmark exists
  if (!"Fw" %in% ind_data$Landmark) {
    return(NULL)
  }
  
  # Find the index of the last FW landmark
  last_fw_indices <- which(ind_data$Landmark == "Fw")
  last_fw <- max(last_fw_indices)
  
  # Get the Sr88 value at the last FW landmark (transition point)
  sr88_last_fw <- ind_data$Sr88[last_fw]
  
  # Find the minimum Sr88 value before the transition point
  min_sr88 <- min(ind_data$Sr88[1:last_fw], na.rm = TRUE)
  
  # Scale Sr88 so that min_sr88 is 0 and sr88_last_fw is 1
  # Uses precise scaling to ensure transition point is exactly 1.0
  ind_data$Sr88_scaled <- (ind_data$Sr88 - min_sr88) / (sr88_last_fw - min_sr88)
  
  # Calculate moving average of scaled Sr88
  moving_avg_sr88 <- rollapply(ind_data$Sr88_scaled, width = window_size, 
                               FUN = mean, align = "center", fill = NA)
  
  # Get moving average value at last FW landmark
  sr88_last_fw_ma <- moving_avg_sr88[last_fw]
  
  # Find the minimum moving average Sr88 value before the transition point
  min_sr88_ma <- min(moving_avg_sr88[1:last_fw], na.rm = TRUE, finite = TRUE)
  
  # Scale moving average Sr88 to ensure transition point is exactly 1.0
  ind_data$Moving_Avg_Sr88 <- (moving_avg_sr88 - min_sr88_ma) / 
    (sr88_last_fw_ma - min_sr88_ma)
  
  return(ind_data)
}

#' Interpolate region of interest to consistent length
#'
#' @param ind_data Data frame containing processed otolith data
#' @param landmark_filter Vector of landmark names
#' @param post_fw_extension Microns to include after Fw
#' @param avg_reads Average number of reads for interpolation
#' @return List of interpolated data series
interpolate_region_of_interest <- function(ind_data, landmark_filter, 
                                           post_fw_extension, avg_reads) {
  # Extract region of interest
  region <- extract_region_of_interest(ind_data, landmark_filter, post_fw_extension)
  if (is.null(region)) {
    return(NULL)
  }
  
  # Set up interpolation indices
  indices <- seq_len(nrow(region))
  target_indices <- seq(1, nrow(region), length.out = avg_reads)
  
  # Interpolate each data series
  interpolated_raw <- interpolate_series(indices, region$Iso, target_indices)
  interpolated_gam <- interpolate_series(indices, region$GAM_smoothed, target_indices)
  interpolated_ma <- interpolate_series(indices, region$Moving_Avg, target_indices)
  interpolated_sr88 <- interpolate_series(indices, region$Moving_Avg_Sr88, target_indices)
  
  # Check for interpolation failures
  if (is.null(interpolated_raw) || is.null(interpolated_gam) || 
      is.null(interpolated_ma) || is.null(interpolated_sr88)) {
    return(NULL)
  }
  
  return(list(
    RAW = interpolated_raw,
    GAM_Smoothed = interpolated_gam,
    Moving_Avg = interpolated_ma,
    Sr88 = interpolated_sr88
  ))
}

#' Interpolate a single data series
#'
#' @param indices Original indices
#' @param series Data series to interpolate
#' @param target_indices Target indices for interpolation
#' @return Interpolated series or NULL if interpolation failed
interpolate_series <- function(indices, series, target_indices) {
  # Handle missing or constant data
  if (all(is.na(series)) || length(unique(na.omit(series))) < 2) {
    return(NULL)
  }
  
  # Interpolate with error handling
  result <- try(
    approx(indices, series, target_indices, method = "linear", rule = 2)$y,
    silent = TRUE
  )
  
  if (inherits(result, "try-error")) {
    return(NULL)
  }
  
  return(result)
}

#' Export processed data to CSV files
#'
#' @param results_list List of processed results
#' @param landmark_filter Vector of landmark names
#' @param output_dir Output directory
export_processed_data <- function(results_list, landmark_filter, output_dir) {
  # Extract metadata and data matrices
  metadata <- extract_metadata_from_results(results_list)
  
  # Extract data matrices
  raw_df <- extract_data_matrix(results_list, "RAW")
  gam_df <- extract_data_matrix(results_list, "GAM_Smoothed")
  ma_df <- extract_data_matrix(results_list, "Moving_Avg")
  sr88_df <- extract_data_matrix(results_list, "Sr88")
  
  # Combine metadata with data matrices
  all_data_combined_raw <- cbind(metadata, raw_df)
  all_data_combined_gam <- cbind(metadata, gam_df)
  all_data_combined_ma <- cbind(metadata, ma_df)
  all_data_combined_sr88 <- cbind(metadata, sr88_df)
  
  # Define filenames dynamically based on landmark filter
  landmark_str <- paste(landmark_filter, collapse = "_")
  filename_base <- paste0("Processed_", landmark_str)
  
  # Save files
  write_processed_data(all_data_combined_raw, output_dir, paste0(filename_base, "_RAW.csv"))
  write_processed_data(all_data_combined_gam, output_dir, paste0(filename_base, "_GAM.csv"))
  write_processed_data(all_data_combined_ma, output_dir, paste0(filename_base, "_MA.csv"))
  write_processed_data(all_data_combined_sr88, output_dir, paste0(filename_base, "_Sr88.csv"))
}

#' Extract metadata from results list
#'
#' @param results_list List of processed results
#' @return Data frame of metadata
extract_metadata_from_results <- function(results_list) {
  data.frame(
    Fish_id = sapply(results_list, function(x) x$Fish_id),
    Watershed = sapply(results_list, function(x) x$Watershed),
    Natal_Iso = sapply(results_list, function(x) x$Natal_Iso),
    Year = sapply(results_list, function(x) x$Year)
  )
}

#' Extract data matrix from results list
#'
#' @param results_list List of processed results
#' @param data_type Type of data to extract ("RAW", "GAM_Smoothed", etc.)
#' @return Matrix of data values
extract_data_matrix <- function(results_list, data_type) {
  do.call(rbind, lapply(results_list, function(x) x[[data_type]]))
}

#' Write processed data to CSV file
#'
#' @param data_frame Data frame to write
#' @param output_dir Output directory
#' @param filename Filename
write_processed_data <- function(data_frame, output_dir, filename) {
  full_path <- file.path(output_dir, filename)
  write.csv(data_frame, file = full_path, row.names = FALSE)
  log_info(paste("Saved", nrow(data_frame), "records to", full_path))
}

#' Logging functions for consistent messaging
log_info <- function(message) message(message)
log_warning <- function(message) warning(message)
log_error <- function(message) message("ERROR:", message)


# Load required libraries
library(dplyr)
library(data.table)
library(mgcv)
library(zoo)
library(progress)
library(here)

# Source the script containing all the functions
# Assuming you've saved the script as "preprocess_timeseries.R"
#source("preprocess_timeseries.R")

# Process with default settings (Core and Fw landmarks)
process_trimmed_data()

# Process with only Core landmark
process_trimmed_data(landmark_filter = c("Core"))

# Process with only Fw landmark
process_trimmed_data(landmark_filter = c("Fw"))

# Process with custom parameters
process_trimmed_data(
  window_size = 40,            # Smaller window for moving average
  gamma_value = 0.6,           # Different smoothing parameter
  landmark_filter = c("Fw"),   # Just use Fw landmark
  post_fw_extension = 200      # Include 200 microns after Fw instead of 300
)

