library(here)
library(tidyverse)
library(ggplot2)
library(mgcv)

# Define directories
original_dir <- "/Users/benjaminmakhlouf/Research_repos/Western_Ak_otolith_stock_discrimination/Data/Processed/Trim_Locations"
processed_dir <- "/Users/benjaminmakhlouf/Research_repos/Western_Ak_otolith_stock_discrimination/Data/Processed/Trim_Locations_w_Core"

# List files in both directories
original_files <- list.files(original_dir, full.names = TRUE)
processed_files <- list.files(processed_dir, full.names = TRUE)

# Extract Fish_IDs from filenames
original_ids <- gsub("_trimLocations.csv", "", basename(original_files))
processed_ids <- gsub("_Trimmed.csv", "", basename(processed_files))


# Find missing Fish_IDs (those in original but not in processed)
missing_ids <- setdiff(original_ids, processed_ids)
missing_files <- original_files[basename(original_files) %in% paste0(missing_ids, "_trimLocations.csv")]

# Read QC data
QC_data <- read_csv("/Users/benjaminmakhlouf/Research_repos/Western_Ak_otolith_stock_discrimination/Data/qc_results.csv")
QC_data$Fish_ID <- gsub("_trimLocations", "", QC_data$Fish_ID)

# Process only missing files
for (file in missing_files) {
  
  # Read in the individual otolith ablation data
  current_read <- read_csv(file)
  
  Fish_ID <- current_read$Fish_id[1]
  
  # Add another column called Core
  current_read$Core <- NA
  
  # Find the associated index for Fish_ID in QC
  index <- which(QC_data$Fish_ID == Fish_ID)
  
  # Find the associated index for Fish_ID in QC
  index <- which(QC_data$Fish_ID == Fish_ID)
  
  # If Fish_ID is not found, skip to the next file
  if (length(index) == 0) {
    next
  }
  
  # If there are multiple matches, choose the last one
  if (length(index) > 1) {
    index <- index[length(index)]
  }
  
  # If QC_Grade or Core_Status are missing (NA), or don't meet the criteria, skip the file
  if (is.na(QC_data$QC_Grade[index]) || is.na(QC_data$Core_Status[index]) || 
      QC_data$QC_Grade[index] != "Yes" || QC_data$Core_Status[index] == "No") {
    
    file_path <- file.path("/Users/benjaminmakhlouf/Research_repos/Western_Ak_otolith_stock_discrimination/Data/Processed/Trim_Locations_w_Core", paste0(Fish_ID, "_Trimmed.csv"))
    write_csv(current_read, file_path)
    next
  }
  
  # Run a GAM
  n <- length(current_read$Iso)
  k <- floor(15 * (n^(2/9)))
  model <- gam(Iso ~ s(Microns, bs="tp", k=k), gamma=0.9, data=current_read)
  
  # Get model predictions and confidence intervals
  predictions <- predict(model, se=TRUE)
  fit <- predictions$fit
  se <- predictions$se.fit
  lcl <- fit - 1.96 * se
  ucl <- fit + 1.96 * se
  
  # Prepare data for plotting
  i.for <- order(current_read$Microns)
  i.back <- order(current_read$Microns, decreasing=TRUE)
  x.polygon <- c(current_read$Microns[i.for], current_read$Microns[i.back])
  y.polygon <- c(ucl[i.for], lcl[i.back])
  
  # Open new plotting device
  dev.new()
  par(mfrow=c(2, 1))
  
  # Plot Sr88
  plot(current_read$Sr88 ~ current_read$Microns, axes=FALSE, pch=16, cex=0.5)
  axis(1, at=seq(50, 50 * max(current_read$Microns) %/% 50, by=50))
  axis(2)
  box()
  
  # Plot Sr8786 with GAM
  plot(current_read$Iso ~ current_read$Microns, main= Fish_ID,  xlab="microns", ylab="87Sr/86Sr", axes=FALSE)
  polygon(x.polygon, y.polygon, col="gold", border=NA)
  lines(current_read$Microns[i.for], fit[i.for], col="black", lwd=2)
  axis(1, at=seq(50, 50 * max(current_read$Microns) %/% 50, by=50))
  axis(2)
  
  # Add the locations already selected as vertical lines 
  abline(v = current_read$natal_microns_start, col="red")
  abline(v = current_read$natal_microns_end, col="red")
  abline(v = current_read$marine_start, col="blue")
  
  # Horizontal line at .7092
  abline(h = .7092, col="blue")
  
  # Use locator to get one point
  locations <- locator(1)
  Core_start <- locations$x
  current_read$Core <- Core_start
  
  # Save the updated data in new location
  file_path <- file.path(processed_dir, paste0(Fish_ID, "_Trimmed.csv"))
  write_csv(current_read, file_path)
  
  dev.off()
}
