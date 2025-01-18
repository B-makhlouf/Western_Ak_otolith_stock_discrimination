### This script is to assess the baseline preformance of Dynamic Time Warping on 
# Data trimmed between the start of the natal origin and the the beginning of the marine 

library(dplyr)
library(dtwclust)
library(here)


# Load the data
All_Data <- read.csv("Data/Intermediate/PCA_data.csv")

# Preprocess data
Fish_ids <- All_Data$Fish_id
Watershed <- All_Data$Watershed
Natal_iso <- All_Data$Natal_iso

# Remove metadata columns 
All_Data <- All_Data[,-c(1:4)]

###### DTWCLUST 

# Run dynamic time warping on the data and export the distance matrix 

# Run a distance matrix 
Distance_matrix <- proxy::dist(All_Data, method = "dtw_basic", window.size = 100)



