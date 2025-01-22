library(here)
library(dplyr)
library(ggplot2)
library(zoo)
library(cowplot)

# Define directories


### 2015 Yukon 
data_dir <- here("Data/Raw/LA Data/2015 Yukon")
metadata_path <- here("Data/Final/Extracted Natal Origins/ALL_DATA_2015_Yukon_Natal_Origins.csv")
output_dir <- here("/Users/benjaminmakhlouf/Research_repos/Western_Ak_otolith_stock_discrimination/Data/Processed/Trim_Locations/Diagnostic_Plots")
la_data_dir <- here("Data/Processed/Trim_Locations")

# 2017 Kuskokwim
data_dir <- here("Data/Raw/LA Data/2017 Kusko")
metadata_path <- here("Data/Final/Extracted Natal Origins/ALL_DATA_2017_Kusko_Natal_Origins.csv")
output_dir <- here("Data/Processed/Trim_Locations/Diagnostic_Plots")
la_data_dir <- here("Data/Processed/Trim_Locations")

# 2017 Kuskokwim 
#data_dir <- here("Data/Intermediate/Cleaned but not trimmed/2017 Kusko")
#metadata_path <- here("Dat


# 2019 Kuskokwim
#data_dir <- here("Data/Intermediate/Cleaned but not trimmed/2019 Kusko")
#metadata_path <- here("Data/Processed/Extracted Natal Origins/ALL_DATA_2019_Kusko_Natal_Origins.csv")
#output_dir <- here("Data/Intermediate/Trimmed no core/Kusko/Diagnostic Plots")
#la_data_dir <- here("Data/Intermediate/Trimmed no core/Kusko/LA Data")

# 2014 Nushagak 
#data_dir <- here("Data/Intermediate/Cleaned but not trimmed/2014 Nush")
#metadata_path <- here("/Users/benjaminmakhlouf/Research_repos/Western_Ak_otolith_stock_discrimination/Data/Processed/Extracted Natal Origins/2014 Nushagak_Cleaned_Natal_Origins.csv")
#output_dir <- here("Data/Intermediate/Trimmed no core/Nush/Diagnostic Plots")
#la_data_dir <- here("Data/Intermediate/Trimmed no core/Nush/LA Data")

# 2016 Yukon 
#data_dir <- here("Data/Intermediate/Cleaned but not trimmed/2016 Yukon")
#metadata_path <- here("Data/Processed/Extracted Natal Origins/ALL_DATA_2016_Yukon_Natal_Origins.csv")
#output_dir <- here("Data/Intermediate/Trimmed no core/Yukon/Diagnostic Plots")
#la_data_dir <- here("Data/Intermediate/Trimmed no core/Yukon/LA Data")



# Load metadata
metadata <- read.csv(metadata_path)

# Load all individual files
files <- list.files(data_dir, full.names = TRUE)

# Process each file
for (file_path in files) {
  # Error handling for each file
  tryCatch({
    # Load individual data
    individual_data <- read.csv(file_path)
    fish_id <- individual_data$Fish_id[1]
    Watershed <- individual_data$Watershed[1]
    
    # Match metadata
    fish_metadata <- metadata %>% filter(Fish_id == fish_id)
    
    if (nrow(fish_metadata) == 0) {
      warning(paste("No metadata found for Fish ID:", fish_id))
      next  # Skip this iteration and continue with the next file
    }
    
    natal_iso <- fish_metadata$natal_iso
    natal_iso_start <- fish_metadata$natal_start
    natal_iso_end <- fish_metadata$natal_end
    
    # Plot Iso vs Microns
    individual_data$Iso_MA <- zoo::rollmean(individual_data$Iso, 60, fill = NA)
    
    
    # Default natal thresholds and logic based on Watershed
    if (Watershed == "Yukon") {
      
      # Define Sr88 threshold for Yukon
      Sr88_threshold <- 0.90
      
      # Set the natal threshold range based on natal_iso value
      if (natal_iso > 0.720) {
        natal_threshold <- c(0.7091, 0.7097)
      } else {
        natal_threshold <- c(0.7091, 0.7094)
      }
      
      # Find the index for Yukon Watershed
      Sr88_threshold_index <- which(
        individual_data$Sr88 > Sr88_threshold & 
          individual_data$Iso_MA > natal_threshold[1] & 
          individual_data$Iso_MA < natal_threshold[2] & 
          individual_data$Microns > natal_iso_end
      )[1]
      
    } else if (Watershed == "Kusko") {
      
      # Define Sr88 threshold for Kuskokwim
      Sr88_threshold <- max(individual_data$Sr88)  # Example: adjust as needed
      
      # Define natal thresholds for Kuskokwim
      natal_threshold <- c(0.7088, 0.7095)  # Example: customize for Kuskokwim
      
      # Find the index for Kuskokwim Watershed
      Sr88_threshold_index <- which(
        individual_data$Sr88 == Sr88_threshold & 
          individual_data$Iso_MA > natal_threshold[1] & 
          individual_data$Iso_MA < natal_threshold[2] & 
          individual_data$Microns > natal_iso_end
      )[1]
      
    } else if (Watershed == "OtherWatershed") {
      
      # Define Sr88 threshold for another watershed
      Sr88_threshold <- 0.88  # Example threshold
      
      # Define natal thresholds for this watershed
      natal_threshold <- c(0.7090, 0.7095)  # Example: adjust as needed
      
      # Find the index for this watershed
      Sr88_threshold_index <- which(
        individual_data$Sr88 > Sr88_threshold & 
          individual_data$Iso_MA > natal_threshold[1] & 
          individual_data$Iso_MA < natal_threshold[2] & 
          individual_data$Microns > natal_iso_end
      )[1]
      
    } else {
      
      # Handle other watersheds or default behavior
      stop("Watershed not recognized. Please define thresholds for this watershed.")
    }
    
    # Output the index
    print(Sr88_threshold_index)
    
    
    
    
    # find the microns at the Sr88 threshold
    Sr88_threshold_microns <- individual_data$Microns[Sr88_threshold_index]
    
   # ggplot iso vs microns and add three lines for the Sr88 threshold, natal start and natal end
    plot <- ggplot(individual_data, aes(x = Microns, y = Iso)) +
      geom_point( color = "gray30", alpha = .5) +
      geom_line(aes(y = Iso_MA), color = "red") +
      geom_vline(xintercept = Sr88_threshold_microns, linetype = "dashed", color = "red", size = 1) +
      geom_vline(xintercept = natal_iso_start, linetype = "dashed", color = "blue", size = 1) +
      geom_vline(xintercept = natal_iso_end, linetype = "dashed", color = "blue", size = 1) +
      labs(title = paste("Iso vs Microns for Fish ID:", fish_id))
    
    
    # Same thing with Sr88 
    individual_data$Sr88_MA <- zoo::rollmean(individual_data$Sr88, 60, fill = NA)
    
    plot2 <- ggplot(individual_data, aes(x = Microns, y = Sr88)) +
      geom_point() +
      geom_line(aes(y = Sr88_MA), color = "red") +
      geom_vline(xintercept = Sr88_threshold_microns, linetype = "dashed", color = "red", size = 1) +
      geom_vline(xintercept = natal_iso_start, linetype = "dashed", color = "blue", size = 1) +
      geom_vline(xintercept = natal_iso_end, linetype = "dashed", color = "blue", size = 1) +
      labs(title = paste("Sr88 vs Microns"))
    
    
    # Add the trimming locations to the individual data 
    individual_data$natal_microns_start<- natal_iso_start
    individual_data$natal_microns_end<- natal_iso_end
    individual_data$marine_start <- Sr88_threshold_microns
    
    # Add natal origin to the individual data
    individual_data$natal_origin_iso <- natal_iso
    
    # Show plot 1 and 2 together 
    combined_plot <- plot_grid(plot, plot2, ncol = 1)

    # Save the combined plot
    output_file <- file.path(output_dir, paste0(fish_id, ".pdf"))
    
    ggsave(
      filename = output_file,
      plot = combined_plot,
      width = 25,          # Width in cm
      height = 15,         # Height in cm
      units = "cm",
      dpi = 300            # Resolution in dots per inch
    )
    
    # Save trimmed data to CSV if Watershed is Yukon
    if (Watershed == "Yukon") {
      trimmed_output_file <- file.path(la_data_dir, paste0(fish_id, "_trimLocations.csv"))
      write.csv(individual_data_trimmed, trimmed_output_file, row.names = FALSE)
    }
    
    if (Watershed == "Kusko") {
      trimmed_output_file <- file.path(la_data_dir, paste0(fish_id, "_trimLocations.csv"))
      write.csv(individual_data_trimmed, trimmed_output_file, row.names = FALSE)
    }
    
    if (Watershed == "Nush") {
      trimmed_output_file <- file.path(la_data_dir, paste0(fish_id, "_trimLocations.csv"))
      write.csv(individual_data_trimmed, trimmed_output_file, row.names = FALSE)
    }
    
  }, error = function(e) {
    # If an error occurs, print the error message and skip the current iteration
    message("Error processing file: ", file_path, "\nError message: ", e$message)
  })
}
