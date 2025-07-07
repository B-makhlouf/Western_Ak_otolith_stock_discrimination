# ==============================================================================
# TMB KALMAN FILTER PROCESSING FOR OTOLITH TIME SERIES DATA
# ==============================================================================
# This script applies TMB Kalman filtering to interpolated raw Sr87/86 data
# Input: preprocessed_RAW.csv from the preprocessing pipeline
# Output: Kalman filtered time series data for machine learning analysis
# Author: [Your Name]
# Date: [Current Date]
# ==============================================================================

library(tidyverse)
library(here)
library(TMB)
library(Matrix)
library(ggplot2)
library(cowplot)
library(viridis)
library(progress)

# ==============================================================================
# CONFIGURATION AND SETUP
# ==============================================================================

message("=== TMB KALMAN FILTER PROCESSING PIPELINE STARTED ===")

# Configuration
config <- list(
  filter_enabled = TRUE,        # Set to FALSE for smoothing only
  initial_R = .001,            # Observation variance (NULL = auto-calculate)
  initial_Q = NULL,            # Process variance (NULL = auto-calculate)
  log_transform = FALSE        # Whether to log-transform data before filtering
)

# Create output directories
output_dirs <- list(
  matrices = here("data/preprocessed_matrices"),
  kalman_diagnostics = here("/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/Figures/Kahlman"),
  tmb_temp = here("temp_tmb")  # Temporary directory for TMB files
)

for (dir in output_dirs) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    message(paste("Created directory:", dir))
  }
}

# ==============================================================================
# STEP 1: SETUP TMB KALMAN FILTER
# ==============================================================================

message("\n=== STEP 1: SETTING UP TMB KALMAN FILTER ===")

# Store current working directory
original_wd <- getwd()

# Set temporary working directory for TMB compilation
setwd(output_dirs$tmb_temp)

# Define the Kalman filter TMB code
Univariate_Filter <- 
  "// Kalman filter for univariate
#include <TMB.hpp>
// Function for detecting NAs
template<class Type>
bool isNA(Type x){
return R_IsNA(asDouble(x));
}
template<class Type>
Type objective_function<Type>::operator() ()
{
  DATA_VECTOR(obs);
  PARAMETER(logR);
  PARAMETER(logQ);
  PARAMETER_VECTOR(u);
  
  int timeSteps=obs.size();
  
  Type R = exp(logR);
  Type Q = exp(logQ);
  
  using namespace density;
  Type ans=0;
  for(int i=1;i<timeSteps;i++){
      ans -= dnorm(u(i),u(i-1),Q,1); //PROCESS LIKELIHOOD
  }
  for(int j=0;j<timeSteps;j++){
      if(!isNA(obs(j))){
        ans -= dnorm(obs(j),u(j),R,1); //OBS LIKELIHOOD
      }
  }
  ADREPORT(R);
  ADREPORT(Q);
  ADREPORT(u);
  
  return ans;
}"
  
  # Write and compile TMB model
  tryCatch({
    write(Univariate_Filter, file = "Univariate_Filter.cpp")
    compile("Univariate_Filter.cpp")
    dyn.load(dynlib("Univariate_Filter"))
    message("✓ TMB Kalman filter compiled successfully")
  }, error = function(e) {
    stop("Failed to compile TMB model: ", e$message)
  })
  
  # Define the Kalman filter function
  KalmanTMB <- function(obs, filter = TRUE, initialR = NULL, initialQ = NULL) {
    data <- list(obs = obs)
    
    if (is.null(initialR)) {
      initialR <- log(sd(diff(obs), na.rm = TRUE) * (1/3))
    }
    if (is.null(initialQ)) {
      initialQ <- log(sd(diff(obs), na.rm = TRUE) * (2/3))
    }
    
    if (filter) {
      parameters <- list(
        logR = initialR,
        logQ = initialQ,
        u = rep(0, length(obs))
      )
      logRm <- as.factor(1)
      logQm <- as.factor(1)
    } else {
      parameters <- list(
        logR = -50,
        logQ = initialQ,
        u = rep(0, length(obs))
      )
      logRm <- as.factor(NA)
      logQm <- as.factor(1)
    }
    
    obj1 <- MakeADFun(
      data = data,
      parameters = parameters,
      random = "u",
      DLL = "Univariate_Filter",
      silent = TRUE,
      map = list(logR = logRm, logQ = logQm)
    )
    
    opt1 <- nlminb(
      obj1$par, obj1$fn, obj1$gr,
      control = list(iter.max = 2000, eval.max = 2000)
    )
    
    pl1 <- obj1$env$parList()
    
    return(list(
      u = pl1$u,
      R = exp(pl1$logR),
      Q = exp(pl1$logQ),
      convergence = opt1$convergence,
      objective = opt1$objective
    ))
  }
  
  # Restore working directory
  setwd(original_wd)
  
  # ==============================================================================
  # STEP 2: LOAD PREPROCESSED RAW DATA
  # ==============================================================================
  
  message("\n=== STEP 2: LOADING PREPROCESSED RAW DATA ===")
  
  # Load the raw interpolated data
  raw_data_file <- file.path(output_dirs$matrices, "preprocessed_RAW.csv")
  
  if (!file.exists(raw_data_file)) {
    stop("Raw preprocessed data not found. Please run the preprocessing script first.")
  }
  
  raw_data <- read.csv(raw_data_file)
  message(paste("✓ Loaded raw data:", nrow(raw_data), "samples"))
  
  # Extract metadata and time series columns
  metadata_cols <- c("Fish_id", "Watershed", "Natal_Iso", "Year")
  ts_cols <- grep("^X", names(raw_data), value = TRUE)
  
  message(paste("  - Metadata columns:", length(metadata_cols)))
  message(paste("  - Time series length:", length(ts_cols)))
  
  # ==============================================================================
  # STEP 3: APPLY KALMAN FILTER TO EACH TIME SERIES
  # ==============================================================================
  
  message("\n=== STEP 3: APPLYING KALMAN FILTER TO TIME SERIES ===")
  message(paste("Processing", nrow(raw_data), "individual time series..."))
  
  # Initialize progress bar
  pb <- progress_bar$new(
    format = "Filtering [:bar] :percent (:current/:total) :eta remaining",
    total = nrow(raw_data),
    clear = FALSE,
    width = 60
  )
  
  # Initialize storage for results
  kalman_results <- matrix(NA, nrow = nrow(raw_data), ncol = length(ts_cols))
  colnames(kalman_results) <- ts_cols
  
  # Storage for filter parameters and diagnostics
  filter_diagnostics <- data.frame(
    Fish_id = raw_data$Fish_id,
    Watershed = raw_data$Watershed,
    R_param = numeric(nrow(raw_data)),
    Q_param = numeric(nrow(raw_data)),
    Convergence = numeric(nrow(raw_data)),
    Objective = numeric(nrow(raw_data)),
    Status = character(nrow(raw_data)),
    stringsAsFactors = FALSE
  )
  
  # Process each individual time series
  for (i in 1:nrow(raw_data)) {
    pb$tick()
    
    tryCatch({
      # Extract time series for this individual
      fish_id <- raw_data$Fish_id[i]
      watershed <- raw_data$Watershed[i]
      
      # Get the raw time series
      ts_raw <- as.numeric(raw_data[i, ts_cols])
      
      # Remove any infinite or extremely large values
      ts_raw[is.infinite(ts_raw)] <- NA
      ts_raw[abs(ts_raw) > 1e6] <- NA
      
      # Apply log transformation if specified
      if (config$log_transform) {
        # Add small constant to avoid log(0) issues
        ts_raw <- log(ts_raw + abs(min(ts_raw, na.rm = TRUE)) + 1e-6)
      }
      
      # Check if we have enough valid data points
      valid_points <- sum(!is.na(ts_raw))
      if (valid_points < 10) {
        filter_diagnostics$Status[i] <- "Insufficient_Data"
        next
      }
      
      # Apply Kalman filter
      kalman_result <- KalmanTMB(
        obs = ts_raw,
        filter = config$filter_enabled,
        initialR = config$initial_R,
        initialQ = config$initial_Q
      )
      
      # Store filtered time series
      kalman_results[i, ] <- kalman_result$u
      
      # Store diagnostics
      filter_diagnostics$R_param[i] <- kalman_result$R
      filter_diagnostics$Q_param[i] <- kalman_result$Q
      filter_diagnostics$Convergence[i] <- kalman_result$convergence
      filter_diagnostics$Objective[i] <- kalman_result$objective
      filter_diagnostics$Status[i] <- ifelse(kalman_result$convergence == 0, "Success", "Convergence_Issue")
      
      # Create smoothed fit plot for ALL individuals
      if (filter_diagnostics$Status[i] == "Success") {
        
        # Prepare data for plotting
        plot_data <- data.frame(
          Index = 1:length(ts_raw),
          Raw = ts_raw,
          Kalman_Smoothed = kalman_result$u
        )
        
        # Create smoothed fit plot
        smoothed_plot <- ggplot(plot_data, aes(x = Index)) +
          geom_point(aes(y = Raw), color = "gray60", alpha = 0.4, size = 0.8) +
          geom_line(aes(y = Kalman_Smoothed), color = "red", linewidth = 1.2) +
          labs(
            title = paste0("Kalman Smoothed Sr87/86: ", fish_id, " (", watershed, ")"),
            x = "Time Index",
            y = "Sr87/86 Ratio"
          ) +
          theme_minimal() +
          theme(
            plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
            axis.title = element_text(size = 12),
            axis.text = element_text(size = 10),
            panel.grid.minor = element_blank()
          )
        
        # Save smoothed fit plot
        smoothed_filename <- file.path(
          output_dirs$kalman_diagnostics, 
          paste0(fish_id, "_kalman_smoothed.png")
        )
        ggsave(smoothed_filename, smoothed_plot, width = 10, height = 6, dpi = 300)
        
      } else {
        # Create a simple error message for failed cases
        error_plot <- ggplot() +
          annotate("text", x = 0.5, y = 0.5, 
                   label = paste0("Kalman Filtering Failed for ", fish_id, "\n", 
                                  "Status: ", filter_diagnostics$Status[i]), 
                   hjust = 0.5, vjust = 0.5, size = 12, color = "red") +
          theme_void() +
          labs(title = paste0("Error: ", fish_id)) +
          theme(
            plot.title = element_text(size = 14, hjust = 0.5, color = "red")
          )
        
        # Save error plot
        error_filename <- file.path(
          output_dirs$kalman_diagnostics, 
          paste0(fish_id, "_kalman_ERROR.png")
        )
        ggsave(error_filename, error_plot, width = 8, height = 4, dpi = 300)
      }
      
    }, error = function(e) {
      message(paste("Error processing", fish_id, ":", e$message))
      filter_diagnostics$Status[i] <- paste("Error:", substr(e$message, 1, 50))
    })
  }
  
  # ==============================================================================
  # STEP 4: COMPILE AND SAVE KALMAN FILTERED DATA
  # ==============================================================================
  
  message("\n=== STEP 4: COMPILING KALMAN FILTERED DATA ===")
  
  # Combine metadata with Kalman filtered results
  kalman_data <- cbind(raw_data[, metadata_cols], kalman_results)
  
  # Save Kalman filtered data
  kalman_filename <- file.path(output_dirs$matrices, "preprocessed_KALMAN.csv")
  write.csv(kalman_data, kalman_filename, row.names = FALSE)
  
  message(paste("✓ Kalman filtered data saved:", kalman_filename))
  message(paste("  - Dimensions:", nrow(kalman_data), "×", ncol(kalman_data)))
  
  # Save filter diagnostics
  diagnostics_filename <- file.path(output_dirs$kalman_diagnostics, "kalman_filter_diagnostics.csv")
  write.csv(filter_diagnostics, diagnostics_filename, row.names = FALSE)
  
  # ==============================================================================
  # STEP 5: GENERATE SUMMARY DIAGNOSTICS
  # ==============================================================================
  
  message("\n=== STEP 5: GENERATING SUMMARY DIAGNOSTICS ===")
  
  # Summary statistics
  success_count <- sum(filter_diagnostics$Status == "Success")
  total_count <- nrow(filter_diagnostics)
  
  message(paste("Kalman filtering completed:"))
  message(paste("  ✓ Successfully filtered:", success_count, "time series"))
  message(paste("  ✗ Failed filtering:", total_count - success_count, "time series"))
  message(paste("  📊 Success rate:", round(100 * success_count / total_count, 1), "%"))
  
  # Create summary plots
  if (success_count > 0) {
    successful_data <- filter_diagnostics[filter_diagnostics$Status == "Success", ]
    
    # Distribution of filter parameters by watershed
    param_data <- successful_data %>%
      select(Watershed, R_param, Q_param) %>%
      pivot_longer(cols = c(R_param, Q_param), names_to = "Parameter", values_to = "Value")
    
    p_params <- ggplot(param_data, aes(x = Watershed, y = Value, fill = Watershed)) +
      geom_boxplot() +
      facet_wrap(~Parameter, scales = "free_y", labeller = labeller(
        Parameter = c("R_param" = "Observation Variance (R)", "Q_param" = "Process Variance (Q)")
      )) +
      labs(
        title = "Kalman Filter Parameter Distributions by Watershed",
        x = "Watershed",
        y = "Parameter Value"
      ) +
      theme_minimal() +
      scale_fill_viridis_d() +
      theme(legend.position = "none")
    
    # Success rate by watershed
    success_summary <- filter_diagnostics %>%
      group_by(Watershed) %>%
      summarize(
        Total = n(),
        Success = sum(Status == "Success"),
        Success_Rate = Success / Total * 100,
        .groups = "drop"
      )
    
    p_success <- ggplot(success_summary, aes(x = Watershed, y = Success_Rate, fill = Watershed)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = paste0(Success, "/", Total)), vjust = -0.5) +
      labs(
        title = "Kalman Filter Success Rate by Watershed",
        x = "Watershed",
        y = "Success Rate (%)"
      ) +
      theme_minimal() +
      scale_fill_viridis_d() +
      theme(legend.position = "none")
    
    # Combine summary plots
    summary_plot <- plot_grid(p_success, p_params, ncol = 1, rel_heights = c(1, 1.5))
    
    # Save summary plot
    summary_filename <- file.path(output_dirs$kalman_diagnostics, "kalman_summary_diagnostics.png")
    ggsave(summary_filename, summary_plot, width = 12, height = 10, dpi = 300)
    
    message(paste("✓ Summary diagnostics saved:", summary_filename))
  }
  
  # ==============================================================================
  # STEP 6: CLEANUP TMB FILES
  # ==============================================================================
  
  message("\n=== STEP 6: CLEANING UP TEMPORARY FILES ===")
  
  # Clean up TMB temporary files
  tryCatch({
    setwd(output_dirs$tmb_temp)
    
    # Remove compiled files
    tmb_files <- list.files(pattern = "Univariate_Filter\\.(cpp|o|so|dll)$")
    if (length(tmb_files) > 0) {
      file.remove(tmb_files)
    }
    
    setwd(original_wd)
    
    # Remove temporary directory if empty
    if (length(list.files(output_dirs$tmb_temp)) == 0) {
      unlink(output_dirs$tmb_temp, recursive = TRUE)
    }
    
    message("✓ Temporary TMB files cleaned up")
  }, error = function(e) {
    message("Warning: Could not clean up some temporary files")
    setwd(original_wd)
  })
  
  # ==============================================================================
  # PIPELINE COMPLETION
  # ==============================================================================
  
  message("\n=== KALMAN FILTER PIPELINE COMPLETED SUCCESSFULLY ===")
  message(paste("📁 Kalman filtered data saved to:", kalman_filename))
  message(paste("📊 Diagnostic plots saved to:", output_dirs$kalman_diagnostics))
  message(paste("🔧 Configuration used:"))
  message(paste("   - Filter enabled:", config$filter_enabled))
  message(paste("   - Log transform:", config$log_transform))
  message(paste("   - Success rate:", round(100 * success_count / total_count, 1), "%"))
  message("=== END OF KALMAN FILTER PIPELINE ===")
  
  # Return results for further analysis (optional)
  invisible(list(
    kalman_data = kalman_data,
    diagnostics = filter_diagnostics,
    config = config,
    success_rate = success_count / total_count
  ))