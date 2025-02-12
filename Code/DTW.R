# Load necessary libraries
library(dtw)
library(dtwclust)
library(tidyverse)
library(here)
library(cowplot)

example(dtw)


## A noisy sine wave as query

alignment<-dtw(query,reference)
plot(alignment,type="alignment")

plot(alignment$index1,alignment$index2,main="Warping function")
lines(1:100-25,col="red")


alignmentOBE <-
     dtw(query[44:88],reference,
                   keep=TRUE,step=asymmetric,
                    window.type = sakoeChibaWindow(iw, jw, window.size= 20),
                   open.end=TRUE,open.begin=TRUE)



??dtwWindowingFunctions

?dtw

alignmentOBE <-
  dtw(testfish1$isonew,testfish2$isonew,
      keep=TRUE,
      window.type = slantedBandWindow,
      window.size = 2,
      open.end=FALSE,open.begin=FALSE, 
      )



testfish1$isonew<- testfish1$Iso *1000
testfish2$isonew<- testfish2$Iso *1000

plot(alignmentOBE,type="two",off=20)


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
# Function to compute DTW distance and alignment between two time series
compute_dtw <- function(ts1, ts2, title1, title2, color1, color2) {
  # Compute DTW alignment
  alignment <- dtw(ts1$Iso, ts2$Iso, keep = TRUE)
  
  # Create a dataframe for plotting
  aligned_data <- data.frame(
    Index1 = alignment$index1,
    Index2 = alignment$index2,
    Iso1 = ts1$Iso[alignment$index1],
    Iso2 = ts2$Iso[alignment$index2]
  )
  
  # Plot the aligned time series
  alignment_plot <- ggplot(aligned_data, aes(x = Index1)) +
    geom_line(aes(y = Iso1, color = title1), size = 1) +
    geom_line(aes(y = Iso2, color = title2), size = 1) +
    scale_color_manual(values = c(color1, color2)) +
    labs(
      title = paste("DTW Alignment:", title1, "vs", title2),
      x = "Index",
      y = "Iso",
      color = "Fish"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  # Plot the alignment path
  path_plot <- ggplot(aligned_data, aes(x = Index1, y = Index2)) +
    geom_line(color = "black", size = 1) +
    labs(
      title = paste("DTW Alignment Path:", title1, "vs", title2),
      x = "Index (Fish 1)",
      y = "Index (Fish 2)"
    ) +
    theme_minimal()
  
  # Combine the plots
  combined_dtw_plot <- cowplot::plot_grid(alignment_plot, path_plot, ncol = 1)
  
  # Display the combined DTW plot
  print(combined_dtw_plot)
  
  # Return the DTW distance
  return(alignment$distance)
}

# Compute DTW for the first pair (2019_yk_410 vs 2019_yk_415)
dtw_distance_1_2 <- compute_dtw(testfish1, testfish2, "2019_yk_410", "2019_yk_415", "blue", "red")

print(paste("DTW Distance (2019_yk_410 vs 2019_yk_415):", dtw_distance_1_2))

# Compute DTW for the second pair (2015_yk_493a vs 2015_yk_494)
dtw_distance_3_4 <- compute_dtw(testfish3, testfish4, "2015_yk_493a", "2015_yk_494", "blue", "red")
print(paste("DTW Distance (2015_yk_493a vs 2015_yk_494):", dtw_distance_3_4))