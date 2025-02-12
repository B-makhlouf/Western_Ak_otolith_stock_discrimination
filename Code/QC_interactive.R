library(ggplot2)
library(dplyr)
library(cowplot)
library(zoo)
library(tools)
library(here)

# Define directories and files
la_data_dir <- here("Data/Processed/Trim_Locations")  # Directory with trimmed files
files <- list.files(la_data_dir, pattern = "_trimLocations.csv", full.names = TRUE)

# Initialize a data frame to store QC grades
qc_results <- data.frame(
  File = character(),
  Fish_ID = character(),
  QC_Grade = character(),
  stringsAsFactors = FALSE
)

# Function to display plots and prompt for QC grade
qc_grade_plot <- function(file_path) {
  # Load data
  individual_data <- read.csv(file_path)
  fish_id <- individual_data$Fish_id[1]
  
  # Generate plots
  sr88_ma_plot <- ggplot(individual_data, aes(x = Microns, y = Sr88)) +
    geom_point(color = "blue") +
    geom_vline(xintercept = individual_data$marine_start[1], color = "red") +
    labs(title = paste("Sr88 Moving Average:", fish_id))
  
  sr8786_ma_plot <- ggplot(individual_data, aes(x = Microns, y = Iso)) +
    geom_point(color = "grey60", alpha = 0.3) +
    geom_vline(xintercept = individual_data$marine_start[1], color = "red") +
    geom_vline(xintercept = individual_data$natal_microns_start[1], color = "blue") +
    geom_vline(xintercept = individual_data$natal_microns_end[1], color = "blue") +
    geom_line(aes(y = Iso_MA), color = "red") +
    labs(title = paste("Sr87/86 Moving Average:", fish_id))
  
  # Combine plots
  combined_plot <- plot_grid(sr88_ma_plot, sr8786_ma_plot, ncol = 1, align = "v", labels = c("A", "B"))
  
  # Display combined plot
  print(combined_plot)
  
  # Prompt for QC grade using single key press
  cat("QC Grade for", fish_id, ":\n")
  cat("Press 'p' for PERFECT, 'r' for REDO, 'd' for DELETE, or 's' to SKIP.\n")
  qc_grade <- readline(prompt = "Enter grade (p/r/d/s): ")
  
  # Map input to grade
  qc_grade <- switch(
    tolower(qc_grade),
    "p" = "Perfect",
    "r" = "Redo",
    "d" = "Delete",
    "s" = "Skip",
    NA
  )
  
  return(qc_grade)
}

# Loop through files
for (file_path in files) {
  tryCatch({
    # Get QC grade
    qc_grade <- qc_grade_plot(file_path)
    
    # Store results if not skipped
    if (!is.na(qc_grade) && qc_grade != "Skip") {
      fish_id <- tools::file_path_sans_ext(basename(file_path))
      qc_results <- rbind(qc_results, data.frame(
        File = file_path,
        Fish_ID = fish_id,
        QC_Grade = qc_grade,
        stringsAsFactors = FALSE
      ))
      
      # Save results after each file (in case of interruption)
      write.csv(qc_results, file.path(la_data_dir, "qc_results.csv"), row.names = FALSE)
    }
    
    # Optional: Add a delay or confirmation to proceed
    proceed <- readline(prompt = "Press Enter to continue or 'q' to quit: ")
    if (tolower(proceed) == "q") {
      break
    }
  }, error = function(e) {
    message("Error processing file: ", file_path, "\nError message: ", e$message)
  })
}

# Save final QC results
write.csv(qc_results, file.path(la_data_dir, "qc_results.csv"), row.names = FALSE)
