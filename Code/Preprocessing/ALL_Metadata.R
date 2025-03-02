### FULL METADATA 

# List all the files with landmark, trim, etc. 

library(tidyverse)

# list all of the .csv files in a directory 
all_files<- list.files(here("/Users/benjaminmakhlouf/Research_repos/Western_Ak_otolith_stock_discrimination/Data/Processed/Landmarks"), pattern = "*.csv", full.names = T)

# Put all the genetics together 
gen_2015 <- read.csv("/Users/benjaminmakhlouf/Research_repos/Shifting-Habitat-Mosaics-II/Data/Genetic_Prior/2015_Yukon_genetic_prior_.csv") %>%
  mutate(
    fish.id = sprintf("%03d", fish.id),
    Fish_ID = paste0("2015_yk_", fish.id)
  ) %>%
  select(-fish.id)


gen_2016<- read.csv("/Users/benjaminmakhlouf/Research_repos/Shifting-Habitat-Mosaics-II/Data/Genetic_Prior/2016_Yukon_genetic_prior_.csv") %>%
  mutate(
    fish.id = sprintf("%03d", fish.id),
    Fish_ID = paste0("2016_yk_", fish.id)
  ) %>%
  select(-fish.id)


gen_2017<- read.csv("/Users/benjaminmakhlouf/Research_repos/Shifting-Habitat-Mosaics-II/Data/Genetic_Prior/2017_Yukon_genetic_prior_.csv") %>%
  mutate(
    fish.id = sprintf("%03d", fish.id),
    Fish_ID = paste0("2017_yk_", fish.id)
  ) %>%
  select(-fish.id)

gen_2019<- read.csv("/Users/benjaminmakhlouf/Research_repos/Shifting-Habitat-Mosaics-II/Data/Genetic_Prior/2019_Yukon_genetic_prior_.csv")
gen_2019 <- gen_2019 %>%
  rename(Fish_ID = fish.id) %>%
  select(Fish_ID, everything())


gen_2021<- read.csv("/Users/benjaminmakhlouf/Research_repos/Shifting-Habitat-Mosaics-II/Data/Genetic_Prior/2021_Yukon_genetic_prior_.csv")

gen_2021<- gen_2021 %>% 
  rename(Fish_ID = fish.id) %>% 
  select(Fish_ID, everything())


# Combine all the genetics
gen_all<- rbind(gen_2015, gen_2016, gen_2017, gen_2019, gen_2021)

#read in the QC data 
QC_data<- read.csv(here("Data/qc_results.csv"))

# rename the column Fish_id to filename_short
QC_data<- QC_data %>% rename(filename_short = Fish_ID)

# Create a new Fish_ID column that is the filename_short without the _trimLocations
QC_data$Fish_ID<- gsub("_trimLocations", "", QC_data$filename_short)

#Create an empty column for "Lower_gen","Middle_gen","Upper_gen"
QC_data$Lower_gen<- NA
QC_data$Middle_gen<- NA
QC_data$Upper_gen<- NA

for (i in seq_along(all_files)) {
  tryCatch({
    data <- read.csv(all_files[i])
    
    # Extract fish_id
    fish_id <- data$Fish_id[1]
    
    # Get index of fish_id in QC_data
    index <- which(QC_data$Fish_ID == fish_id)
    
    # If multiple matches, choose the second
    if (length(index) > 1) {
      index <- index[2]
    }
    
    # Check if index exists
    if (length(index) == 0) {
      message("Warning: Fish_ID ", fish_id, " not found in QC_data.")
      next  # Skip to the next iteration
    }
    
    # Add data to QC_data
    if (!is.na(data$Year[1])) {
      QC_data[index, "Year"] <- as.numeric(data$Year[1])
      QC_data[index, "Natal_microns_start"] <- as.numeric(data$natal_microns_start[1])
      QC_data[index, "Natal_microns_end"] <- as.numeric(data$natal_microns_end[1])
      QC_data[index, "Natal_origins_iso"] <- as.numeric(data$natal_origin_iso[1])
      QC_data[index, "Marine_Start"] <- as.numeric(data$marine_start[1])
      QC_data[index, "Core"] <- as.numeric(data$Core[1])
    } else {
      message("Warning: Missing data for Fish_ID ", fish_id)
    }
    
    # Check if fish_id is in genetics data
    if (fish_id %in% gen_all$Fish_ID) {
      genetics <- gen_all %>% filter(Fish_ID == fish_id)
      
      if (nrow(genetics) > 0) {
        QC_data[index, "Lower_gen"] <- genetics$Lower
        QC_data[index, "Middle_gen"] <- genetics$Middle
        QC_data[index, "Upper_gen"] <- genetics$Upper
      } else {
        message("Warning: No genetics data found for Fish_ID ", fish_id)
      }
    }
    
  }, error = function(e) {
    message("Error processing file: ", all_files[i], " - ", conditionMessage(e))
    next  # Continue to the next iteration despite the error
  })
}


# Create a new colum called_ likely_gen that is the name of the column with the highest value between Lower,Middle, and Upper
QC_data$likely_gen<- apply(QC_data[, c("Lower_gen", "Middle_gen", "Upper_gen")], 1, function(x) {
  if (all(is.na(x))) {
    return(NA)
  }
  
  names(x)[which.max(x)]
})



# write the QC data to a new fill 
write.csv(QC_data, here("Data/Final/Metadata_and_QC.csv"), row.names = F)
  
  
  
  
  
  
  
  
  
  
  
  