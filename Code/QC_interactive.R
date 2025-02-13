library(ggplot2)
library(dplyr)
library(cowplot)
library(zoo)
library(tools)
library(here)

# Define directories and files
la_data_dir <- here("Data/Processed/Trim_Locations")
files <- list.files(la_data_dir, pattern = "_trimLocations.csv", full.names = TRUE)

# Load previous QC results if they exist
qc_results_file <- file.path(la_data_dir, "qc_results.csv")

if (file.exists(qc_results_file) && file.info(qc_results_file)$size > 0) {
  qc_results <- read.csv(qc_results_file, stringsAsFactors = FALSE)
  
  # Get the last processed file name
  last_processed_file <- tail(qc_results$File, 1)
  
  # Find the index of the last processed file
  last_index <- match(last_processed_file, files)
  
  # Start from the next file
  start_index <- ifelse(is.na(last_index), 1, last_index + 1)
} else {
  qc_results <- data.frame(File = character(), Fish_ID = character(), QC_Grade = character(), stringsAsFactors = FALSE)
  start_index <- 1  # Start from the beginning if no records exist
}

# Function to display plots and prompt for QC grade
qc_grade_plot <- function(file_path) {
  individual_data <- read.csv(file_path)
  fish_id <- individual_data$Fish_id[1]
  
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
  
  combined_plot <- plot_grid(sr88_ma_plot, sr8786_ma_plot, ncol = 1, align = "v", labels = c("A", "B"))
  print(combined_plot)
  
  cat("QC Grade for", fish_id, ":\n")
  cat("Press 'p' for PERFECT, 'r' for REDO, 'd' for DELETE, or 's' to SKIP.\n")
  qc_grade <- readline(prompt = "Enter grade (p/r/d/s): ")
  
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

# Loop through files starting from the correct index
for (i in start_index:length(files)) {
  file_path <- files[i]
  
  tryCatch({
    qc_grade <- qc_grade_plot(file_path)
    
    if (!is.na(qc_grade) && qc_grade != "Skip") {
      fish_id <- tools::file_path_sans_ext(basename(file_path))
      qc_results <- qc_results %>%
        bind_rows(data.frame(File = file_path, Fish_ID = fish_id, QC_Grade = qc_grade, stringsAsFactors = FALSE))
      
      write.csv(qc_results, qc_results_file, row.names = FALSE)
    }
    
    proceed <- readline(prompt = "Press Enter to continue or 'q' to quit: ")
    if (tolower(proceed) == "q") {
      break
    }
  }, error = function(e) {
    message("Error processing file: ", file_path, "\nError message: ", e$message)
  })
}

write.csv(qc_results, qc_results_file, row.names = FALSE)
