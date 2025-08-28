# Add Genetic Groupings to GAM Testing Set
# Step-by-step approach to merge genetic data with testing set

library(tidyverse)

# Step 1: Load the GAM data BEFORE Fish_id is removed (from original processed file)
gam_test_data <- read.csv("/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/data/LA_Data/Preprocessed_ts_matrices/NatalToMarine_Processed_GAM.csv")

# Load the Fish_ID splits to identify test fish
fish_id_splits <- read.csv("/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/data/LA_Data/TrainingTesting/Fish_ID_Splits.csv")

# Filter to get only test fish from the original GAM data
test_fish_ids <- fish_id_splits$Fish_id[fish_id_splits$Split == "Test"]
gam_test_data <- gam_test_data %>% 
  filter(Fish_id %in% test_fish_ids)

# Step 2: Load all genetics data with proper column standardization
# First check which files exist and their structures
gen_files <- c(
  "2015" = "/Users/benjaminmakhlouf/Research_repos/Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/Data/Natal Origin Analysis Data/02_Natal Origins and Genetics/2015_Yukon_Natal_Origins_Genetics.csv",
  "2016" = "/Users/benjaminmakhlouf/Research_repos/Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/Data/Natal Origin Analysis Data/02_Natal Origins and Genetics/2016_Yukon_Natal_Origins_Genetics.csv",
  "2017" = "/Users/benjaminmakhlouf/Research_repos/Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/Data/Natal Origin Analysis Data/02_Natal Origins and Genetics/2017_Yukon_Natal_Origins_Genetics.csv",
  "2019" = "/Users/benjaminmakhlouf/Research_repos/Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/Data/Natal Origin Analysis Data/02_Natal Origins and Genetics/2019_Yukon_Natal_Origins_Genetics.csv",
  "2021" = "/Users/benjaminmakhlouf/Research_repos/Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/Data/Natal Origin Analysis Data/02_Natal Origins and Genetics/2021_Yukon_Natal_Origins_Genetics.csv"
)

# Load genetics files and check their structure
gen_list <- list()
for(year in names(gen_files)) {
  if(file.exists(gen_files[year])) {
    cat("Loading genetics for", year, "\n")
    temp_data <- read.csv(gen_files[year])
    cat("Columns in", year, ":", paste(names(temp_data), collapse = ", "), "\n")
    gen_list[[year]] <- temp_data
  } else {
    cat("File not found for", year, "\n")
  }
}

# Step 2b: Standardize and combine genetics data
# Ensure all files have the same essential columns: Fish_id, Lower, Middle, Upper
gen_all <- bind_rows(gen_list)

# Step 3: Determine genetic group for each fish
# The genetic group is the column (Lower, Middle, Upper) with the highest value
genetics_with_group <- gen_all %>%
  rowwise() %>%
  mutate(
    # Find the column name with the maximum value among Lower, Middle, Upper
    genetic_group = case_when(
      is.na(Lower) & is.na(Middle) & is.na(Upper) ~ NA_character_,
      Lower == max(c(Lower, Middle, Upper), na.rm = TRUE) ~ "Lower",
      Middle == max(c(Lower, Middle, Upper), na.rm = TRUE) ~ "Middle", 
      Upper == max(c(Lower, Middle, Upper), na.rm = TRUE) ~ "Upper",
      TRUE ~ NA_character_
    )
  ) %>%
  ungroup() %>%
  select(Fish_id, genetic_group, Lower, Middle, Upper)  # Fish_id should already match GAM data

# Step 4: Merge genetics with testing set by Fish_id
gam_test_with_genetics <- gam_test_data %>%
  left_join(genetics_with_group, by = "Fish_id")

# Step 5: Check the merge results
cat("Original testing set rows:", nrow(gam_test_data), "\n")
cat("Testing set with genetics rows:", nrow(gam_test_with_genetics), "\n")
cat("Fish with genetic assignments:", sum(!is.na(gam_test_with_genetics$genetic_group)), "\n")
cat("Fish missing genetic assignments:", sum(is.na(gam_test_with_genetics$genetic_group)), "\n")

# Step 6: View genetic group distribution
cat("\nGenetic group distribution in testing set:\n")
print(table(gam_test_with_genetics$genetic_group, useNA = "ifany"))

# Step 7: View combined data structure
cat("\nFirst few rows of merged data:\n")
print(head(gam_test_with_genetics[, c("Fish_id", "Watershed", "genetic_group", "Lower", "Middle", "Upper")]))

# Optional: Save the merged dataset
# write.csv(gam_test_with_genetics, "Test_GAM_with_genetics.csv", row.names = FALSE)