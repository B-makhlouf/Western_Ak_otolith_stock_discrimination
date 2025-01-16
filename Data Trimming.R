library(here)
library(dplyr)
library(ggplot2)
library(zoo)
library(cowplot)

# Define directories


### 2015 Yukon 
data_dir <- here("Data/Intermediate/Cleaned but not trimmed/2015 Yukon")
metadata_path <- here("Data/Processed/Extracted Natal Origins/ALL_DATA_2015_Yukon_Natal_Origins.csv")
output_dir <- here("Data/Intermediate/Trimmed no core/Yukon/Diagnostic Plots")
la_data_dir <- here("Data/Intermediate/Trimmed no core/Yukon/LA Data")


# 2017 Kuskokwim 
data_dir <- here("Data/Intermediate/Cleaned but not trimmed/2017 Kusko")








# Load metadata
yk_2015_metadata <- read.csv(metadata_path)

# Load all individual files
Yk_2015 <- list.files(data_dir, full.names = TRUE)

# Process each file
for (file_path in Yk_2015) {
  # Load individual data
  individual_data <- read.csv(file_path)
  fish_id <- individual_data$Fish_id[1]
  Watershed <- individual_data$Watershed[1]
  
  # Match metadata
  fish_metadata <- yk_2015_metadata %>% filter(Fish_id == fish_id)
  
  if (nrow(fish_metadata) == 0) {
    warning(paste("No metadata found for Fish ID:", fish_id))
    next
  }
  
  natal_iso <- fish_metadata$natal_iso
  natal_iso_start <- fish_metadata$natal_start
  natal_iso_end <- fish_metadata$natal_end
  
  # Plot Iso vs Microns
  individual_data$Iso_MA <- zoo::rollmean(individual_data$Iso, 60, fill = NA)
  
  # Trim data
  individual_data_trimmed <- individual_data %>% filter(Microns > natal_iso_start)
  individual_data_trimmed$natal_iso <- natal_iso
  
  # Create ggplot
  plot <- ggplot(individual_data_trimmed, aes(x = Microns, y = Iso)) + 
    geom_point() + 
    geom_line(aes(y = Iso_MA), color = "red") + 
    geom_vline(xintercept = natal_iso_start, color = "blue") + 
    geom_vline(xintercept = natal_iso_end, color = "blue") + 
    labs(x = "Microns", y = "Isotope") + 
    theme_classic()
  
  # Create another ggplot of the original non-trimmed data
  plot2 <- ggplot(individual_data, aes(x = Microns, y = Iso)) + 
    geom_point() + 
    geom_line(aes(y = Iso_MA), color = "red") + 
    geom_vline(xintercept = natal_iso_start, color = "blue") + 
    geom_vline(xintercept = natal_iso_end, color = "blue") + 
    labs(x = "Microns", y = "Isotope") + 
    theme_classic()
  
  # Create a third plot of Sr88
  plot3 <- ggplot(individual_data, aes(x = Microns, y = Sr88)) + 
    geom_point() + 
    geom_line(aes(y = Sr88), color = "red") + 
    labs(x = "Microns", y = "Sr88") + 
    theme_classic()
  
  # Create a fourth plot of trimmed Sr88
  plot4 <- ggplot(individual_data_trimmed, aes(x = Microns, y = Sr88)) + 
    geom_point() + 
    geom_line(aes(y = Sr88), color = "red") + 
    labs(x = "Microns", y = "Sr88") + 
    theme_classic()
  
  # Combine the plots
  combined_plot <- plot_grid(plot2, plot3, plot, plot4, ncol = 1, labels = c("Raw", "RawSr88", "Trimmed", "Trimmed Sr88"))
  
  # Save the combined plot
  output_file <- file.path(output_dir, paste0(fish_id, ".png"))
  ggsave(filename = output_file, plot = combined_plot)
  
  # Save trimmed data to CSV if Watershed is Yukon
  if (Watershed == "Yukon") {
    trimmed_output_file <- file.path(la_data_dir, paste0(fish_id, "_trimmed.csv"))
    write.csv(individual_data_trimmed, trimmed_output_file, row.names = FALSE)
  }
}
