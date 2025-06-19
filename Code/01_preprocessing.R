# ==============================================================================
# OTOLITH TIME SERIES PREPROCESSING PIPELINE
# ==============================================================================
# This script processes laser ablation time series data from otoliths
# Input: Individual CSV files with landmark annotations in data/processed/landmarks
# Output: Standardized matrices for machine learning analysis
# Author: [Your Name]
# Date: [Current Date]
# ==============================================================================

library(tidyverse)
library(here)
library(mgcv)
library(zoo)
library(progress)
library(ggplot2)
library(cowplot)
library(viridis)

# ==============================================================================
# CONFIGURATION AND SETUP
# ==============================================================================

message("=== OTOLITH PREPROCESSING PIPELINE STARTED ===")

# Fixed preprocessing parameters
config <- list(
  window_size = 60,           # For moving average smoothing
  gamma_value = 0.8,          # For GAM smoothing intensity
  landmarks = c("Core", "Fw"), # Always use both landmarks
  marine_extension = 50     # Microns beyond freshwater to include
)

# Create output directories
output_dirs <- list(
  matrices = here("data/preprocessed_matrices"),
  diagnostics = here("/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/Figures/Preprocessing_Diagnostics")
)

for (dir in output_dirs) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    message(paste("Created directory:", dir))
  }
}

# ==============================================================================
# STEP 1: DISCOVER AND VALIDATE INPUT FILES
# ==============================================================================

message("\n=== STEP 1: DISCOVERING INPUT FILES ===")

# List all landmark files
landmark_files <- list.files(
  here("data/processed/landmarks"),
  pattern = "*.csv", 
  full.names = TRUE
)

message(paste("Found", length(landmark_files), "landmark files"))

if (length(landmark_files) == 0) {
  stop("No landmark files found in data/processed/landmarks/")
}

# ==============================================================================
# STEP 2: CALCULATE ADAPTIVE INTERPOLATION POINTS
# ==============================================================================

message("\n=== STEP 2: CALCULATING OPTIMAL INTERPOLATION POINTS ===")

message("Analyzing file sizes to determine optimal interpolation length...")

valid_counts <- numeric()

pb_scan <- progress_bar$new(
  format = "Scanning files [:bar] :percent (:current/:total)",
  total = length(landmark_files),
  clear = FALSE,
  width = 60
)

for (file in landmark_files) {
  pb_scan$tick()
  
  tryCatch({
    ind_data <- read.csv(file)
    
    # Skip if file doesn't contain required landmarks
    if (!all(config$landmarks %in% unique(ind_data$Landmark))) {
      next
    }
    
    # Get filtered data extent
    fw_indices <- which(ind_data$Landmark == "Fw")
    if (length(fw_indices) == 0) next
    
    fw_max_microns <- max(ind_data$Microns[fw_indices], na.rm = TRUE)
    
    ind_data_filtered <- ind_data %>% 
      filter(Landmark %in% config$landmarks | 
               (Microns > fw_max_microns & Microns <= fw_max_microns + config$marine_extension))
    
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
  message(paste("✓ Calculated optimal interpolation points:", interp_points))
  message(paste("  - Based on", length(valid_counts), "valid files"))
  message(paste("  - Range:", min(valid_counts), "to", max(valid_counts), "points"))
} else {
  interp_points <- 1000  # Fallback if no valid counts
  warning("Could not calculate average data points, using default of 1000")
}

# ==============================================================================
# STEP 3: PROCESS INDIVIDUAL FILES AND CREATE DIAGNOSTIC PLOTS
# ==============================================================================

message("\n=== STEP 3: PROCESSING INDIVIDUAL FILES ===")
message(paste("Processing", length(landmark_files), "files with", interp_points, "interpolation points"))

# Initialize progress bar for main processing
pb_main <- progress_bar$new(
  format = "Processing [:bar] :percent (:current/:total) :eta remaining",
  total = length(landmark_files),
  clear = FALSE,
  width = 60
)

# Initialize results storage
results_list <- list()
processing_summary <- data.frame(
  Fish_id = character(),
  Watershed = character(),
  Status = character(),
  Original_Points = numeric(),
  Filtered_Points = numeric(),
  stringsAsFactors = FALSE
)

# Process each file
for (i in seq_along(landmark_files)) {
  file <- landmark_files[i]
  pb_main$tick()
  
  tryCatch({
    # Read file data
    ind_data <- read.csv(file)
    
    # Extract metadata
    watershed <- ind_data$Watershed[1]
    natal_iso <- ind_data$natal_origin_iso[1]
    fish_id <- ind_data$Fish_id[1]
    year <- ind_data$Year[1]
    
    # Skip if missing landmark
    if (!all(config$landmarks %in% unique(ind_data$Landmark))) {
      processing_summary <- rbind(processing_summary, data.frame(
        Fish_id = fish_id,
        Watershed = watershed,
        Status = "Missing_Landmarks",
        Original_Points = nrow(ind_data),
        Filtered_Points = 0
      ))
      next
    }
    
    # Filter data by landmarks
    fw_max_microns <- ind_data %>% 
      filter(Landmark == "Fw") %>% 
      summarise(max_microns = max(Microns, na.rm = TRUE)) %>% 
      pull(max_microns)
    
    ind_data_filtered <- ind_data %>% 
      filter(Landmark %in% config$landmarks | 
               (Microns > fw_max_microns & Microns <= fw_max_microns + config$marine_extension))
    
    # ===============================
    # PROCESS SR87/86 ISOTOPE RATIOS
    # ===============================
    
    # Raw interpolation
    raw_iso <- approx(
      seq_len(nrow(ind_data_filtered)),
      ind_data_filtered$Iso,
      seq(1, nrow(ind_data_filtered), length.out = interp_points),
      method = "linear", rule = 2
    )$y
    
    # Moving average
    ma_iso <- rollapply(ind_data_filtered$Iso, width = config$window_size, 
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
                 gamma = config$gamma_value, data = df)
    
    gam_iso <- predict(model, newdata = data.frame(
      Microns = seq_len(nrow(ind_data_filtered))))
    
    gam_iso_interp <- approx(
      seq_len(length(gam_iso)),
      gam_iso,
      seq(1, length(gam_iso), length.out = interp_points),
      method = "linear", rule = 2
    )$y
    
    # ===============================
    # PROCESS SR88 CONCENTRATIONS
    # ===============================
    
    # Find the last FW landmark index
    last_fw_idx <- which(ind_data_filtered$Landmark == "Fw")
    if(length(last_fw_idx) > 0) {
      last_fw_idx <- max(last_fw_idx)
      
      # Get the Sr88 value at the last FW landmark
      sr88_last_fw <- ind_data_filtered$Sr88[last_fw_idx]
      
      # Find the minimum Sr88 value before the last FW landmark
      min_sr88 <- min(ind_data_filtered$Sr88[1:last_fw_idx], na.rm = TRUE)
      
      # Normalize Sr88
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
    
    # ===============================
    # CREATE COMBINED FEATURE SET
    # ===============================
    
    # Create combined feature set (GAM Sr8786 + Sr88)
    combined_interp <- c(gam_iso_interp, sr88_interp)
    
    # ===============================
    # CREATE SIMPLIFIED DIAGNOSTIC VISUALIZATION WITH RAW POINTS
    # ===============================
    
    # Prepare data for plotting - include raw data points
    original_data <- data.frame(
      Index = 1:nrow(ind_data_filtered),
      Microns = ind_data_filtered$Microns,
      Iso_Raw = ind_data_filtered$Iso,  # Raw Sr87/86 values
      Sr88_Raw = ind_data_filtered$Sr88,
      Sr88_Normalized = norm_sr88,
      Landmark = ind_data_filtered$Landmark
    )
    
    interpolated_data <- data.frame(
      Index = 1:interp_points,
      Iso_MA = ma_iso_interp,
      Iso_GAM = gam_iso_interp,
      Sr88_Normalized = sr88_interp
    )
    
    # Add interpolated raw data to the plotting data
    interpolated_data$Iso_Raw <- raw_iso
    
    # Create plots with interpolated raw data visible and white background
    
    # Plot 1: Sr87/86 with interpolated raw, GAM and Moving Average lines
    p1 <- ggplot(data = interpolated_data, aes(x = Index)) +
      # Add all three lines
      geom_point(aes(y = Iso_Raw, color = "Raw Interpolated"), alpha = 0.8, size = 1) +
      geom_line(aes(y = Iso_MA, color = "Moving Average"), alpha = 0.9, size = 1.2) +
      geom_line(aes(y = Iso_GAM, color = "GAM Smoothed"), alpha = 0.9, size = 1.2) +
      labs(title = "Sr87/86: Raw Interpolated vs Smoothed Comparisons",
           x = "Interpolated Index", 
           y = "Sr87/86 Ratio",
           color = "Processing Method") +
      theme_classic() +  # White background theme
      theme(
        plot.title = element_text(size = 12, face = "bold"),
        axis.title = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 9),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        legend.background = element_rect(fill = "white", color = NA)
      ) +
      scale_color_manual(values = c("Raw Interpolated" = "#7F8C8D", 
                                    "Moving Average" = "#2E86C1", 
                                    "GAM Smoothed" = "#E74C3C"))
    
    # Plot 2: Sr88 with interpolated data
    p2 <- ggplot(data = interpolated_data, aes(x = Index, y = Sr88_Normalized)) +
      # Add interpolated Sr88 line
      geom_line(color = "#27AE60", alpha = 0.9, size = 1.2) +
      labs(title = "Sr88: Normalized and Interpolated",
           x = "Interpolated Index", 
           y = "Normalized Sr88") +
      theme_classic() +  # White background theme
      theme(
        plot.title = element_text(size = 12, face = "bold"),
        axis.title = element_text(size = 10),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA)
      )
    
    # Combine plots side by side
    diagnostic_plot <- plot_grid(
      p1, p2,
      ncol = 2, 
      labels = c("A", "B"),
      label_size = 12
    )
    
    # Add title with white background
    title <- ggdraw() + 
      draw_label(
        paste0("Preprocessing Diagnostics: ", fish_id, " (", watershed, ")"),
        fontface = 'bold',
        size = 14
      ) +
      theme(plot.background = element_rect(fill = "white", color = NA))
    
    final_plot <- plot_grid(
      title, diagnostic_plot,
      ncol = 1,
      rel_heights = c(0.1, 1)
    ) +
      theme(plot.background = element_rect(fill = "white", color = NA))
    
    # Save diagnostic plot with white background
    diagnostic_filename <- file.path(output_dirs$diagnostics, paste0(fish_id, "_preprocessing_diagnostic.png"))
    ggsave(diagnostic_filename, final_plot, width = 12, height = 6, dpi = 300, bg = "white")
    
    # ===============================
    # STORE RESULTS
    # ===============================
    
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
    
    # Update processing summary
    processing_summary <- rbind(processing_summary, data.frame(
      Fish_id = fish_id,
      Watershed = watershed,
      Status = "Success",
      Original_Points = nrow(ind_data),
      Filtered_Points = nrow(ind_data_filtered)
    ))
    
  }, error = function(e) {
    message(paste("Error processing file:", basename(file), "-", e$message))
    
    # Try to extract fish_id for error logging
    fish_id_error <- tryCatch({
      temp_data <- read.csv(file)
      temp_data$Fish_id[1]
    }, error = function(e2) "Unknown")
    
    processing_summary <- rbind(processing_summary, data.frame(
      Fish_id = fish_id_error,
      Watershed = "Unknown",
      Status = paste("Error:", e$message),
      Original_Points = 0,
      Filtered_Points = 0
    ))
  })
}

# ==============================================================================
# STEP 4: COMPILE AND SAVE PROCESSED MATRICES
# ==============================================================================

message("\n=== STEP 4: COMPILING PROCESSED MATRICES ===")

if (length(results_list) == 0) {
  stop("No files were successfully processed!")
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

# Save processed matrices
data_types <- list(
  "RAW" = all_data_raw,
  "GAM" = all_data_gam,
  "MA" = all_data_ma,
  "Sr88" = all_data_sr88,
  "Combined" = all_data_combined
)

message("Saving processed data matrices:")
for (data_type in names(data_types)) {
  filename <- file.path(output_dirs$matrices, paste0("preprocessed_", data_type, ".csv"))
  write.csv(data_types[[data_type]], filename, row.names = FALSE)
  message(paste("  ✓", data_type, "matrix saved:", nrow(data_types[[data_type]]), "samples ×", ncol(data_types[[data_type]]), "features"))
}

# ==============================================================================
# STEP 5: GENERATE PROCESSING SUMMARY AND DIAGNOSTICS
# ==============================================================================

message("\n=== STEP 5: GENERATING PROCESSING SUMMARY ===")

# Save processing summary
summary_filename <- file.path(output_dirs$diagnostics, "processing_summary.csv")
write.csv(processing_summary, summary_filename, row.names = FALSE)

# Print summary statistics
success_count <- sum(processing_summary$Status == "Success")
total_count <- nrow(processing_summary)

message(paste("Processing completed:"))
message(paste("  ✓ Successfully processed:", success_count, "files"))
message(paste("  ✗ Failed processing:", total_count - success_count, "files"))
message(paste("  📊 Success rate:", round(100 * success_count / total_count, 1), "%"))

# Generate summary plots with white background
if (success_count > 0) {
  successful_data <- processing_summary[processing_summary$Status == "Success", ]
  
  # Summary by watershed
  watershed_summary <- successful_data %>%
    group_by(Watershed) %>%
    summarize(
      Count = n(),
      Avg_Original_Points = mean(Original_Points),
      Avg_Filtered_Points = mean(Filtered_Points),
      .groups = "drop"
    )
  
  p_summary <- ggplot(watershed_summary, aes(x = Watershed, y = Count, fill = Watershed)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = Count), vjust = -0.5) +
    labs(title = "Successfully Processed Files by Watershed",
         x = "Watershed", y = "Number of Files") +
    theme_classic() +  # White background theme
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      legend.background = element_rect(fill = "white", color = NA)
    ) +
    scale_fill_viridis_d()
  
  summary_plot_filename <- file.path(output_dirs$diagnostics, "processing_summary_plot.png")
  ggsave(summary_plot_filename, p_summary, width = 8, height = 6, dpi = 300, bg = "white")
  
  message(paste("Summary visualizations saved to:", output_dirs$diagnostics))
}

# ==============================================================================
# PIPELINE COMPLETION
# ==============================================================================

message("\n=== PREPROCESSING PIPELINE COMPLETED SUCCESSFULLY ===")
message(paste("📁 Processed matrices saved to:", output_dirs$matrices))
message(paste("📊 Diagnostic plots saved to:", output_dirs$diagnostics))
message(paste("🔧 Configuration used:"))
message(paste("   - Window size:", config$window_size))
message(paste("   - GAM gamma:", config$gamma_value))
message(paste("   - Landmarks:", paste(config$landmarks, collapse = ", ")))
message(paste("   - Interpolation points:", interp_points))
message("=== END OF PIPELINE ===")

# Return results for further analysis (optional)
invisible(list(
  results = results_list,
  summary = processing_summary,
  config = config,
  interpolation_points = interp_points
))