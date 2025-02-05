### Dynamic Time Warping Clustering and Heatmap Visualization by Watershed ###

# Load required libraries
library(dplyr)
library(dtwclust)
library(proxy)
library(pheatmap)

# Step 1: Load the data
All_Data <- read.csv("Data/Processed/PCA_data.csv")

#Extract the metadata 
Fish_ids <- All_Data$Fish_id
Watershed <- All_Data$Watershed
Natal_iso <- All_Data$Natal_iso
All_Data <- All_Data[,-c(1:4)]  # Remove the metadata columns 

# Step 5: Compute DTW-based distance matrix
#Distance_matrix <- proxy::dist(All_Data, method = "dtw_basic", window.size = 100)
Distance_matrix <- dtw_lb(All_Data,
                          window.size = 100)

Distance_matrix<- as.matrix(Distance_matrix)
write.csv(as.matrix(Distance_matrix), "Data/Processed/DTW_distance_matrix.csv")  # Save the matrix




# for each row, calculate the nearest neighbor (that is not itself) and assign the 
# Watershed based on the nearest neighbor's watershed 

Nearest_Watershed <- character(length = nrow(All_Data))

# Loop over each row to calculate nearest neighbor and assign its watershed
for (i in 1:nrow(Distance_matrix)) {
  # Set the diagonal to infinity to ignore self-neighbor
  Distance_matrix[i, i] <- Inf
  
  # Find the index of the nearest neighbor (the one with the smallest distance)
  nearest_index <- which.min(Distance_matrix[i, ])
  
  # Assign the watershed of the nearest neighbor
  Nearest_Watershed[i] <- Watershed[nearest_index]
}

# Compare the nearest watershed and "Watershed" for which are correctly assigned 
correctly_assigned <- sum(Nearest_Watershed == Watershed)
total <- length(Watershed)
accuracy <- correctly_assigned / total

# List the indices of the individuals which were not correctly assigned 
incorrectly_assigned_indices <- which(Nearest_Watershed != Watershed)

#Find the fish id's which match the index 
incorrectly_assigned_fish_ids <- Fish_ids[incorrectly_assigned_indices]

# Put these AND the natal origin values from the same indixes into a data frame 
incorrectly_assigned_data <- data.frame(Fish_ids = incorrectly_assigned_fish_ids, 
                                        Natal_iso = Natal_iso[incorrectly_assigned_indices])

# show a confusion matrix 
confusion_matrix <- table(Nearest_Watershed, Watershed)

# show confusion matrix in numbers 
confusion_matrix

############### 
library(dplyr)
library(dtwclust)
library(proxy)
library(pheatmap)
library(RColorBrewer)

# Load Distance Matrix
Distance_matrix <- read.csv("Data/Processed/DTW_distance_matrix.csv", row.names = 1)
All_Data <- read.csv("Data/Processed/PCA_data.csv")
Distance_matrix <- as.matrix(Distance_matrix)

# Ensure column names match row names
colnames(Distance_matrix) <- rownames(Distance_matrix)

# Ensure Watershed vector matches row order
Watershed <- All_Data$Watershed
names(Watershed) <- rownames(Distance_matrix)

# Replace Inf values with a high value (e.g., max non-Inf value)
max_dist <- max(Distance_matrix[is.finite(Distance_matrix)])
Distance_matrix[is.infinite(Distance_matrix)] <- max_dist

# Create annotation dataframes for both rows and columns
annotation_row <- data.frame(Watershed = Watershed, row.names = rownames(Distance_matrix))
annotation_col <- data.frame(Watershed = Watershed, row.names = colnames(Distance_matrix))

# Define colors for Watershed classes
ann_colors <- list(Watershed = c("Yukon" = "steelblue", "Kusko" = "tomato", "Nush" = "green"))

all(colnames(Distance_matrix) == rownames(Distance_matrix))

str(annotation_col)
any(is.na(annotation_col))
x
# Plot heatmap with annotations on both axes
pheatmap(Distance_matrix, 
         annotation_row = annotation_row, 
         annotation_col = annotation_col, 
         annotation_colors = ann_colors,
         clustering_distance_rows = "euclidean", 
         clustering_distance_cols = "euclidean", 
         clustering_method = "complete",  # Clustering by similarity
         show_rownames = FALSE, 
         show_colnames = FALSE)
