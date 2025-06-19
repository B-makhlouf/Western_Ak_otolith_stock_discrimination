library(here)
library(dplyr)
library(ggplot2)
library(zoo)
library(cowplot)

# Define directories

#### Yukon 
### 2015 Yukon 
data_dir <- here("Data/Raw/LA Data/2015 Yukon")
metadata_path <- here("Data/Final/Extracted Natal Origins/ALL_DATA_2015_Yukon_Natal_Origins.csv")
output_dir <- here("/Users/benjaminmakhlouf/Research_repos/Western_Ak_otolith_stock_discrimination/Data/Processed/Trim_Locations/Diagnostic_Plots")
la_data_dir <- here("Data/Processed/Trim_Locations")

# 2016 Yukon 
data_dir <- here("Data/Raw/LA Data/2016 Yukon")
metadata_path <- here("Data/Final/Extracted Natal Origins/ALL_DATA_2016_Yukon_Natal_Origins.csv")
output_dir <- here("Data/Processed/Trim_Locations/Diagnostic_Plots")
la_data_dir <- here("Data/Processed/Trim_Locations")

# 2017 Yukon
data_dir <- here("Data/Raw/LA Data/2017 Yukon")
metadata_path <- here("Data/Final/Extracted Natal Origins/2017 Yukon_Cleaned_Natal_Origins.csv")
output_dir <- here("Data/Processed/Trim_Locations/Diagnostic_Plots")
la_data_dir <- here("Data/Processed/Trim_Locations")

# No 2018 

# 2019 Yukon
data_dir <- here("Data/Raw/LA Data/2019 Yukon")
metadata_path <- here("Data/Final/Extracted Natal Origins/2019 Yukon_Cleaned_Natal_Origins.csv")
output_dir <- here("Data/Processed/Trim_Locations/Diagnostic_Plots")
la_data_dir <- here("Data/Processed/Trim_Locations")

# No 2020

# 2021 Yukon 
data_dir <- here("Data/Raw/LA Data/2021 Yukon")
metadata_path <- here("Data/Final/Extracted Natal Origins/ALL_DATA_2021_Yukon_Natal_Origins.csv")
output_dir <- here("Data/Processed/Trim_Locations/Diagnostic_Plots")
la_data_dir <- here("Data/Processed/Trim_Locations")

#
# Kuskokwim

# # 2017 Kuskokwim
data_dir <- here("Data/Raw/LA Data/2017 Kusko")
metadata_path <- here("Data/Final/Extracted Natal Origins/ALL_DATA_2017_Kusko_Natal_Origins.csv")
output_dir <- here("Data/Processed/Trim_Locations/Diagnostic_Plots")
la_data_dir <- here("Data/Processed/Trim_Locations")

# # 2019 Kuskokwim 
data_dir <- here("Data/Raw/LA Data/2019 Kusko")
metadata_path <- here("Data/Final/Extracted Natal Origins/ALL_DATA_2019_Kusko_Natal_Origins.csv")
output_dir <- here("Data/Processed/Trim_Locations/Diagnostic_Plots")
la_data_dir <- here("Data/Processed/Trim_Locations")
# 

# 2020 Kuskokwim 
data_dir <- here("Data/Raw/LA Data/2020 Kusko")
metadata_path <- here("Data/Final/Extracted Natal Origins/ALL_DATA_2020_Kusko_Natal_Origins.csv")
output_dir <- here("Data/Processed/Trim_Locations/Diagnostic_Plots")
la_data_dir <- here("Data/Processed/Trim_Locations")

# 2021 Kuskokwim
data_dir <- here("Data/Raw/LA Data/2021 Kusko")
metadata_path <- here("/Users/benjaminmakhlouf/Research_repos/Western_Ak_otolith_stock_discrimination/Data/Final/Extracted Natal Origins/ALL_DATA_2021_Kusko_Natal_Origins.csv")
output_dir <- here("Data/Processed/Trim_Locations/Diagnostic_Plots")
la_data_dir <- here("Data/Processed/Trim_Locations")


# # 2014 Nushagak 
data_dir <- here("Data/Raw/LA Data/2014 Nush")
metadata_path <- here("/Users/benjaminmakhlouf/Research_repos/Western_Ak_otolith_stock_discrimination/Data/Final/Extracted Natal Origins/2014 Nushagak_Cleaned_Natal_Origins.csv")
output_dir <- here("Data/Processed/Trim_Locations/Diagnostic_Plots")
la_data_dir <- here("Data/Processed/Trim_Locations")

# 2015 Nushagak 
data_dir <- here("Data/Raw/LA Data/2015 Nush")
metadata_path <- here("/Users/benjaminmakhlouf/Research_repos/Western_Ak_otolith_stock_discrimination/Data/Final/Extracted Natal Origins/2015 Nushagak_Cleaned_Natal_Origins.csv")
output_dir <- here("Data/Processed/Trim_Locations/Diagnostic_Plots")
la_data_dir <- here("Data/Processed/Trim_Locations")

#2011 Nushagak
data_dir <- here("Data/Raw/LA Data/2011 Nush")
metadata_path <- here("Data/Final/Extracted Natal Origins/2011 Nushagak_Cleaned_Natal_Origins.csv")
output_dir <- here("Data/Processed/Trim_Locations/Diagnostic_Plots")
la_data_dir <- here("Data/Processed/Trim_Locations")

# Load metadata
metadata <- read.csv(metadata_path)

# Load all individual files
files <- list.files(data_dir, full.names = TRUE)

# 
file_path <- files[5]


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
    
    natal_iso <- fish_metadata$natal_iso
    natal_iso_start <- fish_metadata$natal_start
    natal_iso_end <- fish_metadata$natal_end
    
    #IF the first four characters are 2011, reverse all the data 
    if (substr(fish_id, 1, 4) == "2011") {
      individual_data <- individual_data %>% mutate_at(vars(Iso:Sr88), funs(rev(.)))
    }


    # Plot Iso vs Microns
    individual_data$Iso_MA <- zoo::rollmean(individual_data$Iso, 60, fill = NA)
    
    # plot MA Sr88
    individual_data$Sr88_MA <- zoo::rollmean(individual_data$Sr88, 60, fill = NA)
    
    # Calculate the gradient of Sr88_MA
    individual_data$Sr88_MA_Gradient <- c(NA, diff(individual_data$Sr88_MA))
    
    # Define the starting index of the final half of the dataset
    start_index <- ceiling(nrow(individual_data) / 2)
    
    # Subset the data to the final half
    final_half_gradient <- individual_data$Sr88_MA_Gradient[start_index:nrow(individual_data)]
    
    # Calculate the maximum gradient value in the final half
    max_gradient <- max(final_half_gradient, na.rm = TRUE)
    
    # Find the index of the maximum gradient within the subset
    max_gradient_index_within_subset <- which(final_half_gradient == max_gradient)[1]
    
    # Convert the subset index back to the full dataset index
    max_gradient_index <- start_index + max_gradient_index_within_subset - 1
    
    # Subset the gradient values from the max gradient index to the end of the timeseries
    gradient_subset <- individual_data$Sr88_MA_Gradient[max_gradient_index:length(individual_data$Sr88_MA_Gradient)]
    
    # Find the next point where the gradient falls below 0.001
    next_below_threshold_index <- which(gradient_subset < 0.001)[1]
    
    if (!is.na(next_below_threshold_index)) {
      full_index <- max_gradient_index + next_below_threshold_index - 1
      # Save the micron value as final_index
      marine_cut <- individual_data$Microns[full_index]
    } else {
      # If no such point exists, set final_index to NA
      marine_cut <- NA
    }
    
    # 
    # 
    # 
    # Create Sr88_MA plot
    sr88_ma_plot <- ggplot(individual_data, aes(x = Microns, y = Sr88)) +
      geom_point(color = "blue") +
      geom_vline(xintercept = marine_cut, color = "red") +
      labs(
        title = paste("Sr88 Moving Average:", fish_id),
        x = "Microns",
        y = "Sr88_MA"
      ) +
      theme_minimal()

    # Create Gradient plot
    gradient_plot <- ggplot(individual_data, aes(x = Microns, y = Sr88_MA_Gradient)) +
      geom_line(color = "red") +
      geom_vline(xintercept = marine_cut, color = "red") +
      labs(
        title = paste("Sr88 Gradient:", fish_id),
        x = "Microns",
        y = "Gradient"
      ) +
      theme_minimal()

    sr8786_ma_plot <- ggplot(individual_data, aes(x = Microns, y = Iso)) +
      geom_point(color = "grey60", alpha = .3) +
      geom_vline(xintercept = marine_cut, color = "red") +
      geom_line(aes(y = Iso_MA), color = "red") +
      labs(
        title = paste("Sr87/86 Moving Average:", fish_id),
        x = "Microns",
        y = "Sr8786_MA"
      ) +
      theme_minimal()

    # Combine the three plots
    combined_plot <- plot_grid(
      sr88_ma_plot, gradient_plot, sr8786_ma_plot,
      ncol = 1, align = "v", labels = c("A", "B", "C")
    )
    # 
    # 
# 
#   
   # ggplot iso vs microns and add three lines for the Sr88 threshold, natal start and natal end
    plot <- ggplot(individual_data, aes(x = Microns, y = Iso)) +
      geom_point( color = "gray30", alpha = .3) +
      geom_line(aes(y = Iso_MA), color = "firebrick") +
      geom_vline(xintercept = marine_cut , linetype = "dashed", color = "dodgerblue", size = 1) +
      geom_vline(xintercept = natal_iso_start, linetype = "dashed", color = "dodgerblue", size = 1) +
      geom_vline(xintercept = natal_iso_end, linetype = "dashed", color = "dodgerblue", size = 1) +
      labs(title = paste("Sr87/86 vs Microns:", fish_id))


    # Same thing with Sr88
    individual_data$Sr88_MA <- zoo::rollmean(individual_data$Sr88, 60, fill = NA)

    plot2 <- ggplot(individual_data, aes(x = Microns, y = Sr88)) +
      geom_point(alpha = .2) +
      geom_line(aes(y = Sr88_MA), color = "firebrick") +
      geom_vline(xintercept = marine_cut, linetype = "dashed", color = "dodgerblue", size = 1) +
      geom_vline(xintercept = natal_iso_start, linetype = "dashed", color = "dodgerblue", size = 1) +
      geom_vline(xintercept = natal_iso_end, linetype = "dashed", color = "dodgerblue", size = 1) +
      labs(title = paste("Sr88 vs Microns"))

    
    # # Add the trimming locations to the individual data 
    individual_data$natal_microns_start<- natal_iso_start
    individual_data$natal_microns_end<- natal_iso_end
    individual_data$marine_start <- marine_cut
    
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
    
    trimmed_output_file <- file.path(la_data_dir, paste0(fish_id, "_trimLocations.csv"))
    write.csv(individual_data, trimmed_output_file, row.names = FALSE)
    
    # # Save trimmed data to CSV if Watershed is Yukon
    # if (Watershed == "Yukon") {
    #   trimmed_output_file <- file.path(la_data_dir, paste0(fish_id, "_trimLocations.csv"))
    #   write.csv(individual_data_trimmed, trimmed_output_file, row.names = FALSE)
    # }
    # 
    # if (Watershed == "Kusko") {
    #   trimmed_output_file <- file.path(la_data_dir, paste0(fish_id, "_trimLocations.csv"))
    #   write.csv(individual_data_trimmed, trimmed_output_file, row.names = FALSE)
    # }
    # 
    # if (Watershed == "Nush") {
    #   trimmed_output_file <- file.path(la_data_dir, paste0(fish_id, "_trimLocations.csv"))
    #   write.csv(individual_data_trimmed, trimmed_output_file, row.names = FALSE)
    # }
    # 
  }, error = function(e) {
    # If an error occurs, print the error message and skip the current iteration
    message("Error processing file: ", file_path, "\nError message: ", e$message)
  })
}

