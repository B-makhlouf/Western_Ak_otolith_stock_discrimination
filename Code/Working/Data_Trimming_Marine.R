## This script is to trim the marine environment to a consistant value 

library(here)
library(dplyr)
library(ggplot2)
library(zoo)
library(cowplot)

# Define directories


### 2015 Yukon 
data_dir <- here("/Users/benjaminmakhlouf/Research_repos/Western_Ak_otolith_stock_discrimination/Data/Intermediate/Trimmed no core/Yukon/LA Data")
metadata_path <- here("Data/Processed/Extracted Natal Origins/ALL_DATA_2015_Yukon_Natal_Origins.csv")
output_dir <- here("Data/Intermediate/Trimmed no core or marine/Yukon/Diagnostic Plots")
la_data_dir <- here("Data/Intermediate/Trimmed no core or marine/Yukon/LA Data")


# 2017 Kuskokwim 
data_dir <- here("/Users/benjaminmakhlouf/Research_repos/Western_Ak_otolith_stock_discrimination/Data/Intermediate/Trimmed no core/Kusko/LA Data")
metadata_path <- here("Data/Processed/Extracted Natal Origins/ALL_DATA_2017_Kusko_Natal_Origins.csv")
output_dir <- here("Data/Intermediate/Trimmed no core or marine/Kusko/Diagnostic Plots")
la_data_dir <- here("Data/Intermediate/Trimmed no core or marine/Kusko/LA Data")

# 2019 Kuskokwim
data_dir <- here("/Users/benjaminmakhlouf/Research_repos/Western_Ak_otolith_stock_discrimination/Data/Intermediate/Trimmed no core/Kusko/LA Data")
metadata_path <- here("Data/Processed/Extracted Natal Origins/ALL_DATA_2019_Kusko_Natal_Origins.csv")
output_dir <- here("Data/Intermediate/Trimmed no core or marine/Kusko/Diagnostic Plots")
la_data_dir <- here("Data/Intermediate/Trimmed no core or marine/Kusko/LA Data")


#2014 Nush
data_dir <- here("/Users/benjaminmakhlouf/Research_repos/Western_Ak_otolith_stock_discrimination/Data/Intermediate/Trimmed no core/Nush/LA Data") 
metadata_path <- here("/Users/benjaminmakhlouf/Research_repos/Western_Ak_otolith_stock_discrimination/Data/Processed/Extracted Natal Origins/2014 Nushagak_Cleaned_Natal_Origins.csv")
output_dir <- here("Data/Intermediate/Trimmed no core or marine/Nush/Diagnostic Plots")
la_data_dir <- here("Data/Intermediate/Trimmed no core or marine/Nush/LA Data")




# 2016 Yukon 
data_dir <- here("/Users/benjaminmakhlouf/Research_repos/Western_Ak_otolith_stock_discrimination/Data/Intermediate/Trimmed no core/Yukon/LA Data/2016 Yukon")
metadata_path <- here("Data/Processed/Extracted Natal Origins/ALL_DATA_2016_Yukon_Natal_Origins.csv")
output_dir <- here("Data/Intermediate/Trimmed no core or marine/Yukon/Diagnostic Plots")
la_data_dir <- here("Data/Intermediate/Trimmed no core or marine/Yukon/LA Data")


# Load metadata
metadata <- read.csv(metadata_path)

# Load all individual files
files <- list.files(data_dir, full.names = TRUE)

# Process each file
for (i in 1:length(files)) {
  
  file_path <- files[i]
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
    
    # Identify the index where the MA iso line is within .0001 of .7092 AND Sr88 is greater than 1.2
    # This is the start of the marine environment
    
    #marine_start_index <- which(individual_data$Iso_MA > 0.7090 & individual_data$Iso_MA < 0.7094 & individual_data$Sr88 > 1.0)[1]
   
    marine_start_index<- which(individual_data$Sr88 > 1.6 & individual_data$Iso_MA> .7085)[1]
    
    # Add this value as another column
    individual_data$marine_start_index <- marine_start_index
    
    # Trim the data to before the marine environment
    individual_data_trimmed <- individual_data[1:marine_start_index, ]
    
    
    # from the metadata, add natal origin start and end locations 
    individual_data_trimmed$natal_start_index <- fish_metadata$natal_start
    individual_data_trimmed$natal_end_index <- fish_metadata$natal_end
    individual_data_trimmed$natal_iso <- natal_iso
    
    # Create ggplot
    plot <-  ggplot(individual_data_trimmed, aes(x = Microns, y = Iso)) + 
      geom_point(color = "gray20", alpha = .7) + 
      geom_line(aes(y = Iso_MA), color = "red", size = 2) + 
      geom_vline(xintercept = natal_iso_start, color = "blue") + 
      geom_vline(xintercept = natal_iso_end, color = "blue") + 
      geom_hline(yintercept = 0.7092, color = "orange", size =1.6) +
      labs(x = "Microns", y = "Isotope") + 
      theme_grey()
    
    
    # Create another ggplot of the original non-trimmed data
    plot2 <- ggplot(individual_data, aes(x = Microns, y = Iso)) + 
      geom_point(color = "gray20", alpha = .7) + 
      geom_line(aes(y = Iso_MA), color = "red", size = 2) + 
      geom_vline(xintercept = natal_iso_start, color = "blue") + 
      geom_vline(xintercept = natal_iso_end, color = "blue") + 
      labs(x = "Microns", y = "Isotope") + 
      geom_hline(yintercept = 0.7092, color = "orange", size =1.6) +
      theme_grey()
    
    # Create a third plot of Sr88
    plot3 <- ggplot(individual_data, aes(x = Microns, y = Sr88)) + 
      geom_point(alpha = .6) + 
      labs(x = "Microns", y = "Sr88") + 
      theme_grey()
    
    # Create a fourth plot of trimmed Sr88
    plot4 <- ggplot(individual_data_trimmed, aes(x = Microns, y = Sr88)) + 
      geom_point(alpha = .6) + 
      labs(x = "Microns", y = "Sr88") + 
      theme_grey()
    
    # Combine the plots
    combined_plot <- plot_grid(plot2, plot3, plot, plot4, ncol = 1, labels = c("Raw", "RawSr88", "Trimmed", "Trimmed Sr88"))
    
    # Save the combined plot
    output_file <- file.path(output_dir, paste0(fish_id, ".pdf"))
    ggsave(filename = output_file, plot = combined_plot)
    
    # Save trimmed data to CSV if Watershed is Yukon
    if (Watershed == "Yukon") {
      trimmed_output_file <- file.path(la_data_dir, paste0(fish_id, "_trimmed.csv"))
      write.csv(individual_data_trimmed, trimmed_output_file, row.names = FALSE)
    }
    
    if (Watershed == "Kusko") {
      trimmed_output_file <- file.path(la_data_dir, paste0(fish_id, "_trimmed.csv"))
      write.csv(individual_data_trimmed, trimmed_output_file, row.names = FALSE)
    }
    
    if (Watershed == "Nush") {
      trimmed_output_file <- file.path(la_data_dir, paste0(fish_id, "_trimmed.csv"))
      write.csv(individual_data_trimmed, trimmed_output_file, row.names = FALSE)
    }
    
  }, error = function(e) {
    # If an error occurs, print the error message and skip the current iteration
    message("Error processing file: ", file_path, "\nError message: ", e$message)
  })
}
