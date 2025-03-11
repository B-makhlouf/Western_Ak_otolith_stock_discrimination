# Load necessary libraries
library(dtw)
library(dplyr)
library(ggplot2)
library(tidyr)
library(pheatmap)
library(here)

# Load data
All_GAM_data <- read.csv(here("Data/Processed/all_data_combined_GAM.csv"))

# Separate metadata and GAM data
GAM_metadata <- All_GAM_data[, 1:5]
GAM_reads <- All_GAM_data[, 6:ncol(All_GAM_data)]

# Function to extract and scale time series data
extract_scale <- function(data, fish_id, scale_factor = 1000) {
  series <- data %>%
    filter(Fish_id == fish_id) %>%
    select(6:ncol(.)) %>%
    as.numeric() * scale_factor
  return(series)
}

# Function to compute DTW distance
compute_dtw <- function(series1, series2) {
  dtw(series1, series2,
      window.type = slantedBandWindow,
      window.size = 50,
      open.end = TRUE, open.begin = FALSE)$distance
}

# Function to compute DTW distance matrix for all pairs
compute_dtw_distance_matrix <- function(data, individuals) {
  time_series_list <- lapply(individuals, function(fish_id) extract_scale(data, fish_id))
  names(time_series_list) <- individuals
  
  num_series <- length(time_series_list)
  dtw_distance_matrix <- matrix(NA, nrow = num_series, ncol = num_series)
  rownames(dtw_distance_matrix) <- names(time_series_list)
  colnames(dtw_distance_matrix) <- names(time_series_list)
  
  for (i in 1:(num_series - 1)) {
    for (j in (i + 1):num_series) {
      series1 <- time_series_list[[i]]
      series2 <- time_series_list[[j]]
      dtw_distance_matrix[i, j] <- compute_dtw(series1, series2)
      dtw_distance_matrix[j, i] <- dtw_distance_matrix[i, j]  # Symmetric matrix
    }
  }
  
  return(dtw_distance_matrix)
}

# List of all individuals
individuals <- c("2016_yk_223_redo", "2016_yk_246_redo", "2016_yk_194", "2016_yk_167", "2017_kk_095")

# Compute DTW distance matrix (no trimming)
dtw_distance_matrix <- compute_dtw_distance_matrix(All_GAM_data, individuals)

# Display heatmap
pheatmap(dtw_distance_matrix,
         cluster_rows = TRUE,
         cluster_cols = TRUE,
         display_numbers = TRUE,
         number_format = "%.2f",
         main = "DTW Distance Heatmap (No Trimming)")

# Function to plot all raw time series
# Function to plot all raw time series
plot_raw_time_series <- function(data, individuals, save_path) {
  # Extract and scale all time series
  time_series_list <- lapply(individuals, function(fish_id) extract_scale(data, fish_id))
  names(time_series_list) <- individuals
  
  # Convert to a data frame for ggplot
  time_series_df <- data.frame(
    Time = rep(1:length(time_series_list[[1]]), times = length(individuals)),
    Value = unlist(time_series_list),
    Fish_ID = rep(individuals, each = length(time_series_list[[1]]))
  )
  
  # Plot all time series
  p <- ggplot(time_series_df, aes(x = Time, y = Value, color = Fish_ID)) +
    geom_line() +
    theme_minimal() +
    labs(title = "Raw Time Series (Non-Trimmed)",
         x = "Time",
         y = "Scaled Value",
         color = "Fish ID")
  
  # Save plot as PDF
  ggsave(save_path, p, width = 10, height = 6)
}

# Save raw time series plot as PDF
plot_raw_time_series(All_GAM_data, individuals, here("raw_time_series_plot.pdf"))









################################################################################
# example(dtw)
################################################################################
#### Assessing the ability of DTW to cluster fish based on their iso vs distance data

# Read in the data 


################################################################################
# 2016 yk 119

Yukon119<- All_GAM_data %>%
  filter(Fish_id == "2016_yk_119") %>%
  select(6:ncol(.)) %>%
  as.numeric()

# Plot the data as a line 
plot(Yukon119, type = "l")

################################################################################
# 2017 YK 245

Yukon245<- All_GAM_data %>%
  filter(Fish_id == "2017_yk_245") %>%
  select(6:ncol(.)) %>%
  as.numeric()

plot(Yukon245, type = "l")

################################################################################
#2019 kk 129

Kuskokwim129<- All_GAM_data %>%
  filter(Fish_id == "2017_kk_095") %>%
  select(6:ncol(.)) %>%
  as.numeric()

plot(Kuskokwim129, type = "l")


################################################################################

#### Both Yukon samples 

# Yukon 1 is 119 
# Yukon 2 is 245 

# scale both by 1000 
Yukon119<- Yukon119*1000
Yukon245<- Yukon245*1000

bothYukonDTW <-
  dtw(Yukon119,Yukon245,
      keep=TRUE,
      window.type = slantedBandWindow,
      window.size = 30,
      open.end=TRUE,open.begin=FALSE, 
  )


plot(bothYukonDTW,type="two",off=4)






############################################# 
##################### Yukon 1 (119) and Kusko 

# scale Kusko by 1000
Kuskokwim129<- Kuskokwim129*1000

Yukon_Kusko_DTW <-
  dtw(Yukon119,Kuskokwim129,
      keep=TRUE,
      window.type = slantedBandWindow,
      window.size = 30,
      open.end=TRUE,open.begin=FALSE, 
  )

plot(Yukon_Kusko_DTW,type="two",off=4)

Yukon1_Kusko_Distance<-Yukon_Kusko_DTW$distance


#############################################
##################### Yukon 2 (245) and Kusko

Yukon2_Kusko_DTW <-
  dtw(Yukon245,Kuskokwim129,
      keep=TRUE,
      window.type = slantedBandWindow,
      window.size = 30,
      open.end=TRUE,open.begin=FALSE, 
  )


plot(Yukon2_Kusko_DTW,type="two",off=4)

Yukon2_Kusko_Distance<-Yukon2_Kusko_DTW$distance


###############












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
      window.size = 200,
      open.end=FALSE,open.begin=FALSE, 
  )


plot(alignmentOBEOpenEndNO,type="two",off=10)


alignmentOBEOpenEndYES <- dtw(
  testfish3_short, testfish4,
  keep = TRUE,
  window.type = slantedBandWindow,
  window.size = 50,
  step.pattern = asymmetric,  # Use a step pattern that supports normalization
  open.end = TRUE,
  open.begin = TRUE
)


plot(alignmentOBE,type="two",off=10)


## interpolate testfish4 to 700 reads 
testfish3_short <- approx(1:length(testfish3), testfish3, n = 700)$y



plot(alignmentOBEOpenEndYES,type="two",off=10)













