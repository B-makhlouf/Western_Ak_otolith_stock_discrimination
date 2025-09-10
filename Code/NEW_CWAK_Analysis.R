# CWAK Analysis - Step by Step Debug Version
# Run each section separately to identify where the issue occurs

library(tidyverse)

# =============================================================================
# STEP 1: LOAD AND CHECK BASIC DATA FILES
# =============================================================================

cat("=== STEP 1: Loading basic data files ===\n")

# Load the GAM test data
gam_test_path <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/data/LA_Data/Preprocessed_ts_matrices/NatalToMarine_Processed_GAM.csv"
cat("Loading GAM data from:", gam_test_path, "\n")
gam_data <- read.csv(gam_test_path)

cat("GAM data dimensions:", nrow(gam_data), "rows x", ncol(gam_data), "columns\n")
cat("GAM data column names (first 10):", paste(head(colnames(gam_data), 10), collapse = ", "), "\n")
cat("Does GAM data have Fish_id column?", "Fish_id" %in% colnames(gam_data), "\n")
cat("Does GAM data have Watershed column?", "Watershed" %in% colnames(gam_data), "\n")

# Load test fish IDs
test_fish_ids_path <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/data/LA_Data/TrainingTesting/Fish_ID_Splits.csv"
cat("\nLoading fish ID splits from:", test_fish_ids_path, "\n")
fish_splits <- read.csv(test_fish_ids_path)
test_fish_ids <- fish_splits$Fish_id[fish_splits$Split == "Test"]

cat("Number of test fish IDs:", length(test_fish_ids), "\n")
cat("First 5 test fish IDs:", paste(head(test_fish_ids, 5), collapse = ", "), "\n")

# =============================================================================
# STEP 2: CHECK RF PREDICTIONS FILE
# =============================================================================

cat("\n=== STEP 2: Loading RF predictions ===\n")

rf_predictions_path <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/Output/ModelResultsPreCal/Total/GAM_RF_TOTAL_predictions.csv"
cat("RF predictions path:", rf_predictions_path, "\n")
cat("RF predictions file exists?", file.exists(rf_predictions_path), "\n")

if (file.exists(rf_predictions_path)) {
  rf_predictions <- read.csv(rf_predictions_path)
  cat("RF predictions dimensions:", nrow(rf_predictions), "rows x", ncol(rf_predictions), "columns\n")
  cat("RF predictions column names:", paste(colnames(rf_predictions), collapse = ", "), "\n")
} else {
  cat("RF predictions file not found! Stopping here.\n")
  stop("Cannot proceed without RF predictions")
}

# =============================================================================
# STEP 3: FILTER GAM DATA TO TEST FISH ONLY
# =============================================================================

cat("\n=== STEP 3: Filtering GAM data to test fish ===\n")

# Check if we can filter GAM data
cat("Checking Fish_id column in GAM data...\n")
if ("Fish_id" %in% colnames(gam_data)) {
  gam_test <- gam_data %>%
    filter(Fish_id %in% test_fish_ids)
  
  cat("Filtered GAM test data dimensions:", nrow(gam_test), "rows x", ncol(gam_test), "columns\n")
  cat("Filtered GAM test column names (first 10):", paste(head(colnames(gam_test), 10), collapse = ", "), "\n")
  
  if ("Watershed" %in% colnames(gam_test)) {
    cat("Watershed column exists in filtered data\n")
    cat("Watershed values:", paste(unique(gam_test$Watershed), collapse = ", "), "\n")
  } else {
    cat("ERROR: Watershed column missing from filtered GAM data\n")
  }
} else {
  cat("ERROR: Fish_id column missing from GAM data\n")
  cat("Available columns:", paste(colnames(gam_data), collapse = ", "), "\n")
  stop("Cannot proceed without Fish_id column")
}

# =============================================================================
# STEP 4: CHECK ROW COUNTS MATCH
# =============================================================================

cat("\n=== STEP 4: Checking if row counts match ===\n")

cat("RF predictions rows:", nrow(rf_predictions), "\n")
cat("Filtered GAM test rows:", nrow(gam_test), "\n")
cat("Row counts match?", nrow(rf_predictions) == nrow(gam_test), "\n")

if (nrow(rf_predictions) != nrow(gam_test)) {
  cat("WARNING: Row counts don't match!\n")
  cat("This suggests the filtering or ordering is different\n")
}

# =============================================================================
# STEP 5: LOAD GENETIC DATA (simplified)
# =============================================================================

cat("\n=== STEP 5: Loading genetic data files ===\n")

# Check if the master genetics file already exists
master_genetics_path <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/data/All_Yukon_Genetics.csv"

if (file.exists(master_genetics_path)) {
  cat("Loading existing master genetics file...\n")
  all_genetic_data <- read.csv(master_genetics_path)
  cat("Genetic data dimensions:", nrow(all_genetic_data), "rows x", ncol(all_genetic_data), "columns\n")
  cat("Genetic data column names:", paste(colnames(all_genetic_data), collapse = ", "), "\n")
} else {
  cat("Master genetics file not found. Please run the genetics loading part first.\n")
  cat("Expected location:", master_genetics_path, "\n")
}

cat("\n=== STEP 5 COMPLETE ===\n")
cat("If all steps above worked, we can proceed to the next phase.\n")
cat("If there were errors, please share the output so we can fix them.\n")

# CWAK Analysis - Phase 2: Merge and Analyze
# Run this after the debug script shows everything is working

# CWAK Analysis - Phase 2: Merge and Analyze
# Run this after the debug script shows everything is working

# =============================================================================
# STEP 6: MERGE RF PREDICTIONS WITH GAM TEST DATA (FIXED)
# =============================================================================

cat("=== STEP 6: Merging RF predictions with GAM test data ===\n")

# First check what columns are already in RF predictions
cat("RF predictions columns:", paste(colnames(rf_predictions), collapse = ", "), "\n")

# Check if RF predictions already has Watershed column
if ("Watershed" %in% colnames(rf_predictions)) {
  cat("RF predictions already has Watershed column - using that\n")
  rf_with_metadata <- rf_predictions %>%
    mutate(
      Fish_id = gam_test$Fish_id,
      predicted_watershed = .pred_class,
      correct_prediction = (Watershed == .pred_class)
    )
} else {
  cat("Adding Watershed column from GAM test data\n")
  rf_with_metadata <- rf_predictions %>%
    mutate(
      Fish_id = gam_test$Fish_id,
      Watershed = gam_test$Watershed,
      predicted_watershed = .pred_class,
      correct_prediction = (gam_test$Watershed == .pred_class)
    )
}

cat("RF with metadata dimensions:", nrow(rf_with_metadata), "rows x", ncol(rf_with_metadata), "columns\n")
cat("RF with metadata columns:", paste(colnames(rf_with_metadata), collapse = ", "), "\n")
cat("Sample of merged data:\n")
print(head(rf_with_metadata[, c("Fish_id", "Watershed", "predicted_watershed", "correct_prediction")]))


# =============================================================================
# STEP 7: MERGE WITH GENETIC DATA (FIXED)
# =============================================================================

cat("\n=== STEP 7: Merging with genetic data ===\n")

# Check what columns are in genetic data before merging
cat("Genetic data columns:", paste(colnames(all_genetic_data), collapse = ", "), "\n")

# Remove the Watershed column from genetic data if it exists to avoid duplication
# We want to keep the Watershed column from rf_with_metadata (which comes from GAM data)
if ("Watershed" %in% colnames(all_genetic_data)) {
  cat("Removing Watershed column from genetic data to avoid duplication\n")
  genetic_data_clean <- all_genetic_data %>%
    select(-Watershed)
} else {
  genetic_data_clean <- all_genetic_data
}

# Now merge without duplication
rf_with_genetics <- rf_with_metadata %>%
  left_join(genetic_data_clean, by = "Fish_id")

cat("RF with genetics dimensions:", nrow(rf_with_genetics), "rows x", ncol(rf_with_genetics), "columns\n")
cat("RF with genetics columns:", paste(colnames(rf_with_genetics), collapse = ", "), "\n")

# Check how many fish have genetic data
fish_with_genetics <- sum(!is.na(rf_with_genetics$genetic_group))
cat("Fish with genetic assignments:", fish_with_genetics, "out of", nrow(rf_with_genetics), "\n")
# =============================================================================
# STEP 8: CREATE CWAK GROUPINGS
# =============================================================================

cat("\n=== STEP 8: Creating CWAK groupings ===\n")

rf_with_cwak <- rf_with_genetics %>%
  mutate(
    cwak_group = case_when(
      Watershed %in% c("Nush", "Kusko") ~ "CWAK",
      Watershed == "Yukon" & genetic_group == "Lower" ~ "CWAK", 
      Watershed == "Yukon" & genetic_group %in% c("Middle", "Upper") ~ "non-CWAK",
      TRUE ~ NA_character_
    )
  )

# Check CWAK groupings
cat("CWAK grouping summary:\n")
print(table(rf_with_cwak$cwak_group, useNA = "always"))

# Filter to only fish with clear CWAK classification
rf_final <- rf_with_cwak %>%
  filter(!is.na(cwak_group))

cat("Final dataset dimensions (with CWAK groups):", nrow(rf_final), "rows\n")

# =============================================================================
# STEP 9: ANALYZE PERFORMANCE BY CWAK GROUP
# =============================================================================

cat("\n=== STEP 9: Analyzing performance by CWAK group ===\n")

# Overall performance by CWAK group
cwak_performance <- rf_final %>%
  group_by(cwak_group) %>%
  summarise(
    n_fish = n(),
    correct_predictions = sum(correct_prediction),
    accuracy = correct_predictions / n_fish,
    .groups = "drop"
  )

cat("=== RF GAM MODEL: CWAK vs NON-CWAK PERFORMANCE ===\n")
print(cwak_performance)

# Detailed breakdown by watershed within each group
detailed_performance <- rf_final %>%
  group_by(cwak_group, Watershed) %>%
  summarise(
    n_fish = n(),
    correct_predictions = sum(correct_prediction),
    accuracy = correct_predictions / n_fish,
    .groups = "drop"
  )

cat("\n=== DETAILED BREAKDOWN BY WATERSHED ===\n")
print(detailed_performance)

# Show the genetic breakdown for Yukon fish specifically
yukon_genetic_breakdown <- rf_final %>%
  filter(Watershed == "Yukon") %>%
  group_by(genetic_group, cwak_group) %>%
  summarise(
    n_fish = n(),
    correct_predictions = sum(correct_prediction),
    accuracy = correct_predictions / n_fish,
    .groups = "drop"
  )

cat("\n=== YUKON FISH BY GENETIC GROUP ===\n")
print(yukon_genetic_breakdown)

# =============================================================================
# STEP 10: SAVE RESULTS
# =============================================================================

cat("\n=== STEP 10: Saving results ===\n")

# Create output directory if it doesn't exist
output_dir <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/Output"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Save performance summaries
write.csv(cwak_performance, 
          file.path(output_dir, "CWAK_RF_Performance_Summary.csv"), 
          row.names = FALSE)

write.csv(detailed_performance, 
          file.path(output_dir, "CWAK_RF_Detailed_Performance.csv"), 
          row.names = FALSE)

write.csv(yukon_genetic_breakdown,
          file.path(output_dir, "CWAK_RF_Yukon_Genetic_Breakdown.csv"),
          row.names = FALSE)

# Save full dataset with predictions and CWAK groups
write.csv(rf_final,
          file.path(output_dir, "RF_Predictions_with_CWAK_Groups.csv"), 
          row.names = FALSE)

cat("✓ Results saved to:", output_dir, "\n")
cat("✓ Files saved:\n")
cat("  - CWAK_RF_Performance_Summary.csv\n")
cat("  - CWAK_RF_Detailed_Performance.csv\n")
cat("  - CWAK_RF_Yukon_Genetic_Breakdown.csv\n") 
cat("  - RF_Predictions_with_CWAK_Groups.csv\n")

cat("\n=== ANALYSIS COMPLETE ===\n")