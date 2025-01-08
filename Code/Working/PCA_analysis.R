### This script is to perform PCA analysis on Sr87/86 ts, Sr:Ca (Sr88), and combined 

# Packages 
library(tidyverse)
library(ggplot2)
library(dplyr)
library(here)

# Extract isotope data, interplate to an standard length, and put into an array with 

# Directories 
Nush2014_directory<- here("Data/Intermediate/Cleaned and Trimmed/2014 Nush")
Yukon2015_directory<- here("Data/Intermediate/Cleaned and Trimmed/2015 Yukon")
Kusko2017_directory<- here("Data/Intermediate/Cleaned and Trimmed/2017 Kusko")

# Read in the first file in the directory
Nush2014_files<- list.files(Nush2014_directory, full.names = TRUE)
Yukon2015_files<- list.files(Yukon2015_directory, full.names = TRUE)
Kusko2017_files<- list.files(Kusko2017_directory, full.names = TRUE)

# Read in the data
ind_data<- read_csv(Nush2014_files[1]) # read in an individual file 

watershed<- ind_data$Watershed[1] # get the watershed name

ind_data<- ind_data %>%
  
