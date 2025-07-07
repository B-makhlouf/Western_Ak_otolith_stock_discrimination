library(ggplot2)
library(dplyr)
library(cowplot)
library(zoo)
library(tools)
library(here)

# Define directories and files
la_data_dir <- here("Data/Processed/Trim_Locations")
files <- list.files(la_data_dir, pattern = "_trimLocations.csv", full.names = TRUE)

# Get all Fish IDs from trim locations files
all_fish_ids <- sapply(files, function(file) {
  tryCatch({
    data <- read.csv(file)
    return(data$Fish_id[1])
  }, error = function(e) {
    return(NA)
  })
})

# Remove any NA values (files that couldn't be read)
valid_files <- files[!is.na(all_fish_ids)]
valid_fish_ids <- all_fish_ids[!is.na(all_fish_ids)]

cat("Found", length(valid_fish_ids), "valid trim location files\n")

# Load previous QC results if they exist
qc_results_file <- file.path(la_data_dir, "qc_results.csv")
if (file.exists(qc_results_file) && file.info(qc_results_file)$size > 0) {
  qc_results <- read.csv(qc_results_file, stringsAsFactors = FALSE)
  
  # Get list of already processed Fish IDs
  processed_fish_ids <- qc_results$Fish_ID
  
  # Find Fish IDs that haven't been processed yet
  unprocessed_fish_ids <- valid_fish_ids[!valid_fish_ids %in% processed_fish_ids]
  
  # Get the corresponding files for unprocessed Fish IDs
  unprocessed_files <- valid_files[valid_fish_ids %in% unprocessed_fish_ids]
  
  cat("Found", length(processed_fish_ids), "already processed Fish IDs\n")
  cat("Found", length(unprocessed_fish_ids), "unprocessed Fish IDs\n")
  
} else {
  # No previous results exist
  qc_results <- data.frame(File = character(), Fish_ID = character(), 
                           QC_Grade = character(), Core_Status = character(), 
                           stringsAsFactors = FALSE)
  unprocessed_files <- valid_files
  unprocessed_fish_ids <- valid_fish_ids
  cat("No previous QC results found. Processing all", length(valid_fish_ids), "Fish IDs\n")
}

# Check if there are any unprocessed Fish IDs
if (length(unprocessed_fish_ids) == 0) {
  cat("All Fish IDs have already been processed!\n")
  cat("Current QC results summary:\n")
  print(table(qc_results$QC_Grade))
  
  # Show any Fish IDs that might be in QC but not in trim locations
  if (nrow(qc_results) > 0) {
    orphaned_qc <- qc_results$Fish_ID[!qc_results$Fish_ID %in% valid_fish_ids]
    if (length(orphaned_qc) > 0) {
      cat("\nWarning: Found", length(orphaned_qc), "Fish IDs in QC file but not in trim locations:\n")
      cat(paste(orphaned_qc, collapse = ", "), "\n")
    }
  }
} else {
  cat("Starting QC process for", length(unprocessed_fish_ids), "unprocessed Fish IDs...\n")
  cat("Fish IDs to process:", paste(unprocessed_fish_ids, collapse = ", "), "\n")
}

# Function to display plots and prompt for QC grade and core status
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
  cat("Press 'y' for GOOD, 'r' for REDO, 'd' for DELETE\n")
  qc_grade <- readline(prompt = "Enter grade (y/r/d): ")
  
  qc_grade <- switch(
    tolower(qc_grade),
    "y" = "Yes",
    "r" = "Redo",
    "d" = "Delete",
    NA
  )
  
  if (!is.na(qc_grade)) {
    cat("Core Status for", fish_id, ":\n")
    cat("Press 'y' for YES, 'p' for PARTIAL, or 'n' for NO.\n")
    core_status <- readline(prompt = "Enter core status (y/p/n): ")
    
    core_status <- switch(
      tolower(core_status),
      "y" = "Yes",
      "p" = "Partial",
      "n" = "No",
      NA
    )
  } else {
    core_status <- NA
  }
  
  return(list(qc_grade = qc_grade, core_status = core_status))
}

# Process only unprocessed Fish IDs
if (length(unprocessed_fish_ids) > 0) {
  for (i in 1:length(unprocessed_files)) {
    file_path <- unprocessed_files[i]
    expected_fish_id <- unprocessed_fish_ids[i]
    
    cat("\n--- Processing Fish ID", i, "of", length(unprocessed_fish_ids), "---\n")
    cat("Fish ID:", expected_fish_id, "\n")
    cat("File:", basename(file_path), "\n")
    
    tryCatch({
      result <- qc_grade_plot(file_path)
      qc_grade <- result$qc_grade
      core_status <- result$core_status
      
      if (!is.na(qc_grade)) {
        fish_id <- tools::file_path_sans_ext(basename(file_path))
        
        # Add new result to dataframe
        new_row <- data.frame(File = file_path, Fish_ID = fish_id, 
                              QC_Grade = qc_grade, Core_Status = core_status, 
                              stringsAsFactors = FALSE)
        qc_results <- bind_rows(qc_results, new_row)
        
        # Save after each entry (in case of interruption)
        write.csv(qc_results, qc_results_file, row.names = FALSE)
        cat("Saved QC result for", fish_id, "\n")
      }
      
      # Ask to continue
      proceed <- readline(prompt = "Press Enter to continue or 'q' to quit: ")
      if (tolower(proceed) == "q") {
        cat("Quitting... Progress has been saved.\n")
        break
      }
      
    }, error = function(e) {
      cat("Error processing file:", file_path, "\n")
      cat("Error message:", e$message, "\n")
      proceed <- readline(prompt = "Press Enter to continue with next file or 'q' to quit: ")
      if (tolower(proceed) == "q") {
        break
      }
    })
  }
}

# Final save and summary
write.csv(qc_results, qc_results_file, row.names = FALSE)
cat("\n--- QC Process Complete ---\n")
cat("Total Fish IDs processed:", nrow(qc_results), "\n")
if (nrow(qc_results) > 0) {
  cat("QC Grade summary:\n")
  print(table(qc_results$QC_Grade))
  cat("Core Status summary:\n")
  print(table(qc_results$Core_Status))
}