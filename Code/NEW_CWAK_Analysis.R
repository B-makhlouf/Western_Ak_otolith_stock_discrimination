# ==============================================================================
# CWAK ANALYSIS: GENETIC GROUPINGS AND RANDOM FOREST MODEL PERFORMANCE
# ==============================================================================
# This script combines genetic groupings with Random Forest GAM model predictions
# to analyze performance by CWAK (Central Western Alaska) vs non-CWAK groups
# 
# CWAK Definition:
# - Nushagak (Nush) and Kuskokwim (Kusko) watersheds = CWAK
# - Yukon Lower genetic group = CWAK  
# - Yukon Middle/Upper genetic groups = non-CWAK
#
# Author: [Your Name]
# Date: [Current Date]
# ==============================================================================

library(tidyverse)

# ==============================================================================
# CONFIGURATION AND PATHS
# ==============================================================================

# Base paths
BASE_REPO_PATH <- "/Users/benjaminmakhlouf/Research_repos"
DATA_PATH <- file.path(BASE_REPO_PATH, "04_Western_Ak_otolith_stock_discrimination/data")
OUTPUT_PATH <- file.path(BASE_REPO_PATH, "04_Western_Ak_otolith_stock_discrimination/Output")

# Input file paths
GAM_DATA_PATH <- file.path(DATA_PATH, "LA_Data/Preprocessed_ts_matrices/NatalToMarine_Processed_GAM.csv")
FISH_SPLITS_PATH <- file.path(DATA_PATH, "LA_Data/TrainingTesting/Fish_ID_Splits.csv")
RF_PREDICTIONS_PATH <- file.path(OUTPUT_PATH, "ModelResultsPreCal/Total/GAM_RF_TOTAL_predictions.csv")
MASTER_GENETICS_PATH <- file.path(DATA_PATH, "All_Yukon_Genetics.csv")

# Genetic data file paths (for creating master file if needed)
GENETIC_FILES <- c(
  file.path(BASE_REPO_PATH, "Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/Data/Natal Origin Analysis Data/02_Natal Origins and Genetics/2015_Yukon_Natal_Origins_Genetics.csv"),
  file.path(BASE_REPO_PATH, "Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/Data/Natal Origin Analysis Data/02_Natal Origins and Genetics/2016_Yukon_Natal_Origins_Genetics.csv"),
  file.path(BASE_REPO_PATH, "Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/Data/Natal Origin Analysis Data/02_Natal Origins and Genetics/2017_Yukon_Natal_Origins_Genetics.csv"),
  file.path(BASE_REPO_PATH, "Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/Data/Natal Origin Analysis Data/02_Natal Origins and Genetics/2019_Yukon_Natal_Origins_Genetics.csv"),
  file.path(BASE_REPO_PATH, "Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/Data/Natal Origin Analysis Data/02_Natal Origins and Genetics/2021_Yukon_Natal_Origins_Genetics.csv")
)

# Output file names
OUTPUT_FILES <- list(
  performance_summary = "CWAK_RF_Performance_Summary.csv",
  detailed_performance = "CWAK_RF_Detailed_Performance.csv",
  yukon_genetic_breakdown = "CWAK_RF_Yukon_Genetic_Breakdown.csv",
  full_dataset = "RF_Predictions_with_CWAK_Groups.csv"
)

# Ensure output directory exists
dir.create(OUTPUT_PATH, recursive = TRUE, showWarnings = FALSE)

# ==============================================================================
# FUNCTIONS
# ==============================================================================

#' Determine genetic group based on highest probability
#' @param lower Lower Yukon probability
#' @param middle Middle Yukon probability  
#' @param upper Upper Yukon probability
#' @return Character indicating highest probability group
determine_genetic_group <- function(lower, middle, upper) {
  if (all(is.na(c(lower, middle, upper)))) {
    return(NA)
  }
  
  values <- c(Lower = lower, Middle = middle, Upper = upper)
  max_group <- names(values)[which.max(values)]
  return(max_group)
}

#' Load and combine genetic data from multiple years
#' @param file_paths Vector of file paths to genetic data
#' @return Combined genetic data frame with genetic_group column
load_and_combine_genetic_data <- function(file_paths) {
  cat("=== Loading and combining genetic data ===\n")
  
  genetic_dataframes <- list()
  
  for (file_path in file_paths) {
    if (file.exists(file_path)) {
      year <- str_extract(file_path, "\\d{4}")
      genetic_data <- read.csv(file_path)
      
      # Add year column if not present
      if (!"Year" %in% colnames(genetic_data)) {
        genetic_data$Year <- year
      }
      
      # Remove problematic columns from 2017 data
      if (year == "2017") {
        genetic_data <- genetic_data %>% select(-starts_with("X"))
      }
      
      genetic_dataframes[[year]] <- genetic_data
      cat("Loaded genetic data for", year, "- rows:", nrow(genetic_data), "\n")
    } else {
      cat("Warning: File not found:", file_path, "\n")
    }
  }
  
  # Combine all genetic data
  all_genetic_data <- do.call(rbind, genetic_dataframes)
  
  # Determine genetic groups
  all_genetic_data$genetic_group <- mapply(
    determine_genetic_group,
    all_genetic_data$Lower,
    all_genetic_data$Middle,
    all_genetic_data$Upper
  )
  
  cat("Combined genetic data: ", nrow(all_genetic_data), " rows\n")
  return(all_genetic_data)
}

#' Create CWAK groupings based on watershed and genetic assignments
#' @param data Data frame with Watershed and genetic_group columns
#' @return Data frame with added cwak_group column
create_cwak_groupings <- function(data) {
  data %>%
    mutate(
      cwak_group = case_when(
        Watershed %in% c("Nush", "Kusko") ~ "CWAK",
        Watershed == "Yukon" & genetic_group == "Lower" ~ "CWAK", 
        Watershed == "Yukon" & genetic_group %in% c("Middle", "Upper") ~ "non-CWAK",
        TRUE ~ NA_character_
      )
    )
}

# ==============================================================================
# MAIN ANALYSIS
# ==============================================================================

cat("=== CWAK ANALYSIS: GENETIC GROUPINGS AND MODEL PERFORMANCE ===\n\n")

# ------------------------------------------------------------------------------
# 1. LOAD CORE DATA FILES
# ------------------------------------------------------------------------------

cat("1. Loading core data files...\n")

# Load GAM data
if (!file.exists(GAM_DATA_PATH)) stop("GAM data file not found: ", GAM_DATA_PATH)
gam_data <- read.csv(GAM_DATA_PATH)
cat("   Loaded GAM data:", nrow(gam_data), "rows\n")

# Load test fish IDs
if (!file.exists(FISH_SPLITS_PATH)) stop("Fish splits file not found: ", FISH_SPLITS_PATH)
fish_splits <- read.csv(FISH_SPLITS_PATH)
test_fish_ids <- fish_splits$Fish_id[fish_splits$Split == "Test"]
cat("   Loaded test fish IDs:", length(test_fish_ids), "fish\n")

# Filter GAM data to test fish only
gam_test <- gam_data %>% filter(Fish_id %in% test_fish_ids)
cat("   Filtered GAM test data:", nrow(gam_test), "rows\n")

# Load Random Forest predictions
if (!file.exists(RF_PREDICTIONS_PATH)) stop("RF predictions file not found: ", RF_PREDICTIONS_PATH)
rf_predictions <- read.csv(RF_PREDICTIONS_PATH)
cat("   Loaded RF predictions:", nrow(rf_predictions), "rows\n")

# Verify data alignment
if (nrow(rf_predictions) != nrow(gam_test)) {
  warning("Row count mismatch: RF predictions (", nrow(rf_predictions), 
          ") vs GAM test data (", nrow(gam_test), ")")
}

# ------------------------------------------------------------------------------
# 2. LOAD OR CREATE GENETIC DATA
# ------------------------------------------------------------------------------

cat("\n2. Loading genetic data...\n")

if (file.exists(MASTER_GENETICS_PATH)) {
  cat("   Loading existing master genetics file...\n")
  all_genetic_data <- read.csv(MASTER_GENETICS_PATH)
} else {
  cat("   Master genetics file not found. Creating from individual files...\n")
  all_genetic_data <- load_and_combine_genetic_data(GENETIC_FILES)
  
  # Save master genetics file for future use
  write.csv(all_genetic_data, MASTER_GENETICS_PATH, row.names = FALSE)
  cat("   Saved master genetics file to:", MASTER_GENETICS_PATH, "\n")
}

cat("   Genetic data:", nrow(all_genetic_data), "rows\n")

# ------------------------------------------------------------------------------
# 3. MERGE RF PREDICTIONS WITH METADATA
# ------------------------------------------------------------------------------

cat("\n3. Merging RF predictions with metadata...\n")

# Add Fish_id and watershed information to RF predictions
rf_with_metadata <- rf_predictions %>%
  mutate(
    Fish_id = gam_test$Fish_id,
    Watershed = if ("Watershed" %in% colnames(rf_predictions)) Watershed else gam_test$Watershed,
    predicted_watershed = .pred_class,
    correct_prediction = (Watershed == .pred_class)
  )

cat("   RF with metadata:", nrow(rf_with_metadata), "rows\n")

# ------------------------------------------------------------------------------
# 4. MERGE WITH GENETIC DATA AND CREATE CWAK GROUPINGS
# ------------------------------------------------------------------------------

cat("\n4. Merging with genetic data and creating CWAK groupings...\n")

# Remove Watershed column from genetic data to avoid duplication
genetic_data_clean <- all_genetic_data %>% 
  select(-any_of("Watershed"))

# Merge with genetic data
rf_with_genetics <- rf_with_metadata %>%
  left_join(genetic_data_clean, by = "Fish_id")

# Create CWAK groupings
rf_with_cwak <- create_cwak_groupings(rf_with_genetics)

# Filter to fish with clear CWAK classification
rf_final <- rf_with_cwak %>% filter(!is.na(cwak_group))

cat("   Fish with genetic data:", sum(!is.na(rf_with_genetics$genetic_group)), "\n")
cat("   Fish with CWAK classification:", nrow(rf_final), "\n")
cat("   CWAK fish:", sum(rf_final$cwak_group == "CWAK"), "\n")
cat("   non-CWAK fish:", sum(rf_final$cwak_group == "non-CWAK"), "\n")

# ------------------------------------------------------------------------------
# 5. ANALYZE PERFORMANCE BY CWAK GROUP
# ------------------------------------------------------------------------------

cat("\n5. Analyzing performance by CWAK group...\n")

# Overall performance by CWAK group
cwak_performance <- rf_final %>%
  group_by(cwak_group) %>%
  summarise(
    n_fish = n(),
    correct_predictions = sum(correct_prediction),
    accuracy = correct_predictions / n_fish,
    .groups = "drop"
  )

# Detailed breakdown by watershed within each group
detailed_performance <- rf_final %>%
  group_by(cwak_group, Watershed) %>%
  summarise(
    n_fish = n(),
    correct_predictions = sum(correct_prediction),
    accuracy = correct_predictions / n_fish,
    .groups = "drop"
  )

# Yukon fish breakdown by genetic group
yukon_genetic_breakdown <- rf_final %>%
  filter(Watershed == "Yukon") %>%
  group_by(genetic_group, cwak_group) %>%
  summarise(
    n_fish = n(),
    correct_predictions = sum(correct_prediction),
    accuracy = correct_predictions / n_fish,
    .groups = "drop"
  )

# ------------------------------------------------------------------------------
# 6. DISPLAY RESULTS
# ------------------------------------------------------------------------------

cat("\n=== RESULTS ===\n")

cat("\n--- RF GAM MODEL: CWAK vs NON-CWAK PERFORMANCE ---\n")
print(cwak_performance)

cat("\n--- DETAILED BREAKDOWN BY WATERSHED ---\n")
print(detailed_performance)

cat("\n--- YUKON FISH BY GENETIC GROUP ---\n")
print(yukon_genetic_breakdown)

