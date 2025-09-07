# Add Genetic Groupings to GAM Test Data
# Creates a dataframe with genetic group assignments for testing set fish

library(tidyverse)

# =============================================================================
# LOAD DATA
# =============================================================================

# Load the GAM test data
gam_test_path <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/data/LA_Data/Preprocessed_ts_matrices/NatalToMarine_Processed_GAM.csv"
gam_data <- read.csv(gam_test_path)

# Load test fish IDs (from ModelTesting.R splits)
test_fish_ids_path <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/data/LA_Data/TrainingTesting/Fish_ID_Splits.csv"
fish_splits <- read.csv(test_fish_ids_path)
test_fish_ids <- fish_splits$Fish_id[fish_splits$Split == "Test"]

# Filter GAM data to only test fish
gam_test <- gam_data %>%
  filter(Fish_id %in% test_fish_ids)

# Load genetic data files
genetic_files <- c(
  "/Users/benjaminmakhlouf/Research_repos/Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/Data/Natal Origin Analysis Data/02_Natal Origins and Genetics/2015_Yukon_Natal_Origins_Genetics.csv",
  "/Users/benjaminmakhlouf/Research_repos/Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/Data/Natal Origin Analysis Data/02_Natal Origins and Genetics/2016_Yukon_Natal_Origins_Genetics.csv",
  "/Users/benjaminmakhlouf/Research_repos/Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/Data/Natal Origin Analysis Data/02_Natal Origins and Genetics/2017_Yukon_Natal_Origins_Genetics.csv",
  "/Users/benjaminmakhlouf/Research_repos/Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/Data/Natal Origin Analysis Data/02_Natal Origins and Genetics/2019_Yukon_Natal_Origins_Genetics.csv",
  "/Users/benjaminmakhlouf/Research_repos/Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/Data/Natal Origin Analysis Data/02_Natal Origins and Genetics/2021_Yukon_Natal_Origins_Genetics.csv"
)

# =============================================================================
# COMBINE GENETIC DATA
# =============================================================================

# Read each year as a separate dataframe
genetic_2015 <- NULL
genetic_2016 <- NULL
genetic_2017 <- NULL
genetic_2019 <- NULL
genetic_2021 <- NULL

for (file_path in genetic_files) {
  if (file.exists(file_path)) {
    # Extract year from file path
    year <- str_extract(file_path, "\\d{4}")
    
    # Read the data
    genetic_data <- read.csv(file_path)
    
    # Add year column if not present
    if (!"Year" %in% colnames(genetic_data)) {
      genetic_data$Year <- year
    }
    
    # Assign to specific year dataframe
    if (year == "2015") {
      genetic_2015 <- genetic_data
    } else if (year == "2016") {
      genetic_2016 <- genetic_data
    } else if (year == "2017") {
      genetic_2017 <- genetic_data
    } else if (year == "2019") {
      genetic_2019 <- genetic_data
    } else if (year == "2021") {
      genetic_2021 <- genetic_data
    }
    
    cat("Loaded genetic data for", year, "- rows:", nrow(genetic_data), "\n")
  } else {
    cat("File not found:", file_path, "\n")
  }
}

# Remove X and X.1 from genetic_2017 
if (!is.null(genetic_2017)) {
  genetic_2017 <- genetic_2017 %>%
    select(-starts_with("X"))
}


# Combine all genetic data
genetic_dataframes <- list(genetic_2015, genetic_2016, genetic_2017, genetic_2019, genetic_2021)
genetic_dataframes <- genetic_dataframes[!sapply(genetic_dataframes, is.null)]
all_genetic_data <- do.call(rbind, genetic_dataframes)

# =============================================================================
# DETERMINE GENETIC GROUPS
# =============================================================================

# Function to determine genetic group based on highest probability
determine_genetic_group <- function(lower, middle, upper) {
  # Check if all values are NA
  if (all(is.na(c(lower, middle, upper)))) {
    return(NA)
  }
  
  # Find which has the highest value
  values <- c(Lower = lower, Middle = middle, Upper = upper)
  max_group <- names(values)[which.max(values)]
  
  return(max_group)
}

# Apply genetic group determination
all_genetic_data$genetic_group <- mapply(
  determine_genetic_group,
  all_genetic_data$Lower,
  all_genetic_data$Middle,
  all_genetic_data$Upper
)

# Save as master genetics list /Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/data
master_genetics_path <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/data/All_Yukon_Genetics.csv"
write.csv(all_genetic_data, master_genetics_path, row.names = FALSE)


