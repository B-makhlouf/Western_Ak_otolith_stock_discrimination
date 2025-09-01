# Duplicate_CWAK_Figures.R
# Script to identify and duplicate figures for fish that are both "Yukon" watershed and "Lower" genetic group

library(tidyverse)

# =============================================================================
# CONFIGURATION - Update these paths to match your system
# =============================================================================

# Path to the metadata file with genetic information
metadata_path <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/Data/Final/Metadata_and_QC.csv"

# Source directory for PCA figures
figure_source_dir <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/Figures/PCA/SAME_NO_7080_7085_ts_loadings"

# Output directory for CWAK figures
cwak_output_dir <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/Figures/PCA/CWAK"

# =============================================================================
# LOAD AND FILTER DATA
# =============================================================================

cat("Loading metadata...\n")
metadata <- read_csv(metadata_path)

# Filter for Yukon watershed fish with Lower genetic group
cwak_fish <- metadata %>%
  filter(
    str_detect(Fish_ID, "yk_"),  # Yukon watershed (based on ID pattern)
    likely_gen == "Lower_gen"     # Lower genetic group
  ) %>%
  select(Fish_ID, likely_gen, Lower_gen, Middle_gen, Upper_gen) %>%
  arrange(Fish_ID)

cat("Found", nrow(cwak_fish), "fish that are Yukon watershed + Lower genetic group:\n")
print(cwak_fish)

if (nrow(cwak_fish) == 0) {
  cat("No fish found matching criteria. Exiting.\n")
  quit()
}

# =============================================================================
# FIND AND DUPLICATE RELEVANT FIGURES
# =============================================================================

# Create output directory
dir.create(cwak_output_dir, recursive = TRUE, showWarnings = FALSE)

# Function to find and copy figures for specific fish IDs
copy_cwak_figures <- function(fish_ids, source_dir, dest_dir) {
  
  files_copied <- 0
  files_not_found <- character()
  
  if (!dir.exists(source_dir)) {
    cat("ERROR: Source directory does not exist:", source_dir, "\n")
    return(list(copied = 0, not_found = fish_ids))
  }
  
  for (fish_id in fish_ids) {
    cat("Looking for figures for", fish_id, "...\n")
    
    # Look for files containing this fish ID
    all_files <- list.files(source_dir, pattern = "\\.pdf$", full.names = TRUE)
    matching_files <- all_files[str_detect(basename(all_files), fixed(fish_id))]
    
    if (length(matching_files) > 0) {
      # Copy each found file to CWAK directory
      for (source_file in matching_files) {
        dest_file <- file.path(dest_dir, paste0("CWAK_", basename(source_file)))
        
        # Copy file
        file.copy(source_file, dest_file, overwrite = TRUE)
        cat("  Copied:", basename(source_file), "-> CWAK_", basename(source_file), "\n")
        files_copied <- files_copied + 1
      }
    } else {
      files_not_found <- c(files_not_found, fish_id)
      cat("  No figures found for", fish_id, "\n")
    }
  }
  
  return(list(
    copied = files_copied,
    not_found = files_not_found
  ))
}

# Execute the copying
result <- copy_cwak_figures(cwak_fish$Fish_ID, figure_source_dir, cwak_output_dir)

# =============================================================================
# SUMMARY REPORT
# =============================================================================

cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("CWAK FIGURE DUPLICATION COMPLETE\n")
cat(paste(rep("=", 60), collapse = ""), "\n")

cat("Criteria: Yukon watershed (yk_ in Fish_ID) + Lower genetic group\n")
cat("Target fish identified:", nrow(cwak_fish), "\n")
cat("Figures copied:", result$copied, "\n")
cat("Output directory:", cwak_output_dir, "\n\n")

cat("CWAK Fish List:\n")
for (i in 1:nrow(cwak_fish)) {
  fish <- cwak_fish[i, ]
  cat("  ", fish$Fish_ID, "- Lower:", round(fish$Lower_gen, 3), 
      "Middle:", round(fish$Middle_gen, 3), 
      "Upper:", round(fish$Upper_gen, 3), "\n")
}

if (length(result$not_found) > 0) {
  cat("\nFish with no figures found:\n")
  for (missing_fish in result$not_found) {
    cat("  ", missing_fish, "\n")
  }
}

cat("\nSource directories searched:\n")
for (dir in figure_base_dirs) {
  if (dir.exists(dir)) {
    n_files <- length(list.files(dir, pattern = "\\.pdf$"))
    cat("  ", dir, "(", n_files, "PDF files)\n")
  } else {
    cat("  ", dir, "(DIRECTORY NOT FOUND)\n")
  }
}

cat("\nFiles in CWAK directory:\n")
cwak_files <- list.files(cwak_output_dir, pattern = "\\.pdf$")
if (length(cwak_files) > 0) {
  for (file in sort(cwak_files)) {
    cat("  ", file, "\n")
  }
} else {
  cat("  No PDF files in CWAK directory\n")
}

cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("ANALYSIS READY!\n")
cat("All figures for Yukon + Lower genetic group fish are now in the CWAK folder.\n")
cat("These represent the Central/Western Alaska Kenai (CWAK) genetic lineage.\n")
cat(paste(rep("=", 60), collapse = ""), "\n")