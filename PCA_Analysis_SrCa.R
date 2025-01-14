library(tidyverse)
library(here)

# Define directories
data_directories <- list(
  Nush2014 = here("/Users/benjaminmakhlouf/Research_repos/Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/Data/Otolith Data/LA Data/Trimmed/2014 Nush"),
  Yukon2015 = here("/Users/benjaminmakhlouf/Research_repos/Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/Data/Otolith Data/LA Data/Trimmed/2015 Yukon"),
  Yukon2016 = here("/Users/benjaminmakhlouf/Research_repos/Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/Data/Otolith Data/LA Data/Trimmed/2016 Yukon"),
  Kusko2017 = here("/Users/benjaminmakhlouf/Research_repos/Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/Data/Otolith Data/LA Data/Trimmed/2017 Kusko"),
  Kusko2019 = here("/Users/benjaminmakhlouf/Research_repos/Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/Data/Otolith Data/LA Data/Trimmed/2019 Kusko")
)

# Initialize tibble to store combined results
z_normalized_data <- tibble()

#Call the first directory 


