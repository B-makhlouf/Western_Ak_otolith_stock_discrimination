# Load necessary libraries
library(dtw)
library(dtwclust)
library(tidyverse)
library(here)
library(cowplot)

# example(dtw)

################################################################################
# Step 1: Load and Plot Time Series for Specific Fish Pairs
################################################################################

# Load the data for the specific fish pairs
testfish1 <- read.csv(here("Data/Processed/Trim_Locations/2019_yk_410_trimLocations.csv"))
testfish2 <- read.csv(here("Data/Processed/Trim_Locations/2019_yk_415_trimLocations.csv"))
testfish3 <- read.csv(here("Data/Processed/Trim_Locations/2015_yk_493a_trimLocations.csv"))
testfish4 <- read.csv(here("Data/Processed/Trim_Locations/2015_yk_494_trimLocations.csv"))

# Plot iso vs distance for the specific fish pairs
testfish1_plot <- ggplot(testfish1, aes(x = Microns, y = Iso)) +
  geom_point() +
  ggtitle("2019_yk_410")

testfish2_plot <- ggplot(testfish2, aes(x = Microns, y = Iso)) +
  geom_point() +
  ggtitle("2019_yk_415")

testfish3_plot <- ggplot(testfish3, aes(x = Microns, y = Iso)) +
  geom_point() +
  ggtitle("2015_yk_493a")

testfish4_plot <- ggplot(testfish4, aes(x = Microns, y = Iso)) +
  geom_point() +
  ggtitle("2015_yk_494")

# Combine plots into a single figure using cowplot
combined_plot1 <- plot_grid(
  testfish1_plot, testfish2_plot,
  labels = c("A", "B"),
  ncol = 1  # Arrange plots in 2 columns
)

# Display the combined plot
print(combined_plot1)

combined_plot2 <- plot_grid(
  testfish3_plot, testfish4_plot,
  labels = c("A", "B"),
  ncol = 1  # Arrange plots in 2 columns
)

print(combined_plot2)

################################################################################
# Step 2: Compute DTW Distances for Specific Fish Pairs
################################################################################



########## RAW TS (with adjusted yaxis)

testfish1$isonew<- testfish1$Iso *1000
testfish2$isonew<- testfish2$Iso *1000

alignmentOBE <-
  dtw(testfish1$isonew,testfish2$isonew,
      keep=TRUE,
      window.type = slantedBandWindow,
      window.size = 2,
      open.end=FALSE,open.begin=FALSE, 
  )


plot(alignmentOBE,type="two",off=20)



############################################################

# Read in all preproceesed data 

MA_reads<- read.csv(here(here("Data/Processed/all_data_combined_MA.csv")))


# Find the same fish 

# find the index which fish_id == 2019_yk_410
testfish1<- MA_reads %>%
  filter(Fish_id == "2019_yk_410") %>%
  #select only the 6th column to the end 
  select(6:ncol(.)) %>%
  as.numeric()
  
testfish2<- MA_reads %>%
  filter(Fish_id == "2019_yk_415") %>%
  #select only the 6th column to the end 
  select(6:ncol(.)) %>%
  as.numeric()


# Multiply all values in each times 1000
testfish1<- testfish1*1000
testfish2<- testfish2*1000

alignmentOBE <-
  dtw(testfish1,testfish2,
      keep=TRUE,
      window.type = slantedBandWindow,
      window.size = 100,
      open.end=FALSE,open.begin=FALSE, 
  )

alignmentOBE <-
  dtw(testfish1,testfish2,
      keep=TRUE,
      window.type = sakoeChibaWindow ,
      window.size = 20,
      open.end=FALSE,open.begin=FALSE, 
  )



plot(alignmentOBE,type="two",off=10)



################# Testfish 3 and 4

testfish3<- MA_reads %>%
  filter(Fish_id == "2015_yk_493a") %>%
  #select only the 6th column to the end 
  select(6:ncol(.)) %>%
  as.numeric()


testfish3<- testfish3*1000


testfish4<- MA_reads %>%
  filter(Fish_id == "2015_yk_494") %>%
  #select only the 6th column to the end 
  select(6:ncol(.)) %>%
  as.numeric()

testfish4<- testfish4*1000



alignmentOBEOpenEndNO <-
  dtw(testfish3,testfish4,
      keep=TRUE,
      window.type = slantedBandWindow,
      window.size = 50,
      open.end=FALSE,open.begin=FALSE, 
  )


plot(alignmentOBEOpenEndNO,type="two",off=10)


alignmentOBEOpenEndYES <- dtw(
  testfish3, testfish4,
  keep = TRUE,
  window.type = slantedBandWindow,
  window.size = 50,
  step.pattern = asymmetric,  # Use a step pattern that supports normalization
  open.end = FALSE,
  open.begin = TRUE
)


plot(alignmentOBE,type="two",off=10)


## interpolate testfish4 to 700 reads 
testfish3_short <- approx(1:length(testfish3), testfish3, n = 700)$y

alignmentOBEOpenEndYES <- dtw(
  testfish3_short, testfish4,
  keep = TRUE,
  window.type = slantedBandWindow,
  window.size = 50,
  step.pattern = asymmetric,  # Use a step pattern that supports normalization
  open.end = TRUE,
  open.begin = FALSE
)

plot(alignmentOBEOpenEndYES,type="two",off=10)
