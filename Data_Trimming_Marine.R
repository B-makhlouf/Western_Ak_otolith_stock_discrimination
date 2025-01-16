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


#2014 Nush
data_dir <- here("/Users/benjaminmakhlouf/Research_repos/Western_Ak_otolith_stock_discrimination/Data/Intermediate/Trimmed no core/Nush/LA Data") 
metadata_path <- here("/Users/benjaminmakhlouf/Research_repos/Western_Ak_otolith_stock_discrimination/Data/Processed/Extracted Natal Origins/2014 Nushagak_Cleaned_Natal_Origins.csv")
output_dir <- here("Data/Intermediate/Trimmed no core or marine/Nush/Diagnostic Plots")
la_data_dir <- here("Data/Intermediate/Trimmed no core or marine/Nush/LA Data")


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
    
    # Trim data
  
    dev.new()
    
    # Plot Sr88 vs Cycle (will be microns later)
    par(mfrow=c(2,1))
    plot(individual_data$Sr88 ~ Cycle, data=individual_data, axes=F, pch=16, cex=0.5)
    axis(1, at=seq(50, 50 * max(individual_data$Cycle) %/% 50, by=50))
    axis(2)
    box()
    
    # Plot Sr8786 vs Cycle with rolling average line through it 
    plot(individual_data$Iso ~ individual_data$Cycle,  xlab = "microns" , ylab = "87Sr/86Sr "  ,  main = fish_id,axes=F)
    axis(1,at=seq(50,50*max(individual_data$Cycle) %/% 50,by=50))
    axis(2)
    box()
    abline(h=0.70862, col="orange", lwd=3)
    
    #Rolling average calculation, change k to be window size 
    MA<-zoo::rollmean(individual_data$Iso,k=50,align='center',na.pad=T)
    lines(MA~individual_data$Cycle, col="red", lwd=3)# add rolling average line 
    
    locations <- locator(1) #manually pick where the start and end of the 
    dev.off()
    
    
    # Trim the data to only those BEFORE the selection 
    individual_data_trimmed <- individual_data %>% filter(Microns < locations$x)
  
    
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
