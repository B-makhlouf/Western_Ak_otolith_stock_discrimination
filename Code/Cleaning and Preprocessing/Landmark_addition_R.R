### This script will manually add landmarks 
library(here)
library(tidyverse)
library(cowplot)

# List all the files 
trimlocfiles<- list.files("/Users/benjaminmakhlouf/Research_repos/Western_Ak_otolith_stock_discrimination/Data/Processed/Trim_Locations_w_Core", full.names = TRUE)
library(ggplot2)
library(cowplot)
library(readr)
library(dplyr)
library(here)

for (i in 1:length(trimlocfiles)) {
  
  tryCatch({
    ind_file <- read_csv(trimlocfiles[i])
    ind_file <- ind_file %>% mutate(Landmark = NA)
    
    # Extract key values
    natal_start <- ind_file$natal_microns_start[1]
    core <- ind_file$Core[1]
    marine_start <- ind_file$marine_start[1]
    Fish_ID <- ind_file$Fish_id[1]
    
    # Find the closest indices
    core_index <- which.min(abs(ind_file$Microns - core))
    natal_start_index <- which.min(abs(ind_file$Microns - natal_start))
    
    # Assign "Core" to the range
    ind_file$Landmark[core_index:natal_start_index] <- "Core"
    
    # Find marine start index
    marine_index <- which.min(abs(ind_file$Microns - marine_start))
    
    # Assign "Freshwater" to the range
    ind_file$Landmark[natal_start_index:marine_index] <- "Fw"
    
    # Define early marine range
    early_marine_index <- which.min(abs(ind_file$Microns - (marine_start + 200)))
    
    # Assign "Early Marine" to the range
    ind_file$Landmark[marine_index:early_marine_index] <- "Early Marine"
    
    # Generate plots
    isoplot <- ggplot(ind_file, aes(x = Microns, y = Iso, color = Landmark)) +
      geom_point() +
      geom_line(aes(x = Microns, y = Iso_MA), color = "gray20", linewidth = 1.2) +
      theme_grey() +
      labs(x = "Microns", y = "Iso") +
      scale_color_manual(values = c("dodgerblue", "firebrick", "darkorange")) +
      theme(legend.position = "none")
    
    Sr88plot <- ggplot(ind_file, aes(x = Microns, y = Sr88, color = Landmark)) +
      geom_point() +
      theme_grey() +
      labs(x = "Microns", y = "Sr_Ca") +
      scale_color_manual(values = c("dodgerblue", "firebrick", "darkorange")) +
      theme(legend.position = "none")
    
    # Combine plots
    combined_plot <- plot_grid(isoplot, Sr88plot, labels = c("A", "B"), ncol = 1)
    
    # Add fish ID title
    final_plot <- plot_grid(
      ggdraw() + draw_label(Fish_ID, fontface = "bold", size = 16),
      combined_plot,
      ncol = 1,
      rel_heights = c(0.1, 1)
    )
    
    # Save the plot
    ggsave(here("Data/Processed/Landmarks/Diagnostic Plots", paste0(Fish_ID, "_Landmark.png")), final_plot)
    
    # Save the CSV file
    write_csv(ind_file, here("Data/Processed/Landmarks", basename(trimlocfiles[i])))
    
  }, error = function(e) {
    message(paste("Error processing file:", trimlocfiles[i], "- Skipping to next."))
    message(e$message)  # Print the error message for debugging
  })
  
}
