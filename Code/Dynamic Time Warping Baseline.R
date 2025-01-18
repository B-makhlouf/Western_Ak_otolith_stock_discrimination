### Dynamic Time Warping Clustering and Heatmap Visualization by Watershed ###

# Load required libraries
library(dplyr)
library(dtwclust)
library(proxy)
library(pheatmap)

# Step 1: Load the data
All_Data <- read.csv("Data/Intermediate/PCA_data.csv")

#Extract the metadata 
Fish_ids <- All_Data$Fish_id
Watershed <- All_Data$Watershed
Natal_iso <- All_Data$Natal_iso
All_Data <- All_Data[,-c(1:4)]  # Remove the metadata columns 

# Step 5: Compute DTW-based distance matrix
Distance_matrix <- proxy::dist(All_Data, method = "dtw_basic", window.size = 100)
write.csv(as.matrix(Distance_matrix), "Data/Intermediate/DTW_distance_matrix.csv")  # Save the matrix

Distance_matrix<- as.matrix(Distance_matrix)

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

# Calculate the accuracy
accuracy <- correctly_assigned / total

# List the indices of the individuals which were not correctly assigned 
incorrectly_assigned_indices <- which(Nearest_Watershed != Watershed)

#Find the fish id's which match the index 
incorrectly_assigned_fish_ids <- Fish_ids[incorrectly_assigned_indices]

# Put these AND the natal origin values from the same indixes into a data frame 
incorrectly_assigned_data <- data.frame(Fish_ids = incorrectly_assigned_fish_ids, 
                                        Natal_iso = Natal_iso[incorrectly_assigned_indices])

# Quick plot of the distribution of natal origins of misclassified vs classified correctly 

# Plot the distribution of natal origin values for correctly and incorrectly classified individuals

# Create a data frame for plotting
plot_data <- data.frame(
  Classification = c(rep("Correctly Classified", correctly_assigned), 
                     rep("Incorrectly Classified", total - correctly_assigned)),
  Natal_iso = c(Natal_iso[which(Nearest_Watershed == Watershed)], 
                Natal_iso[which(Nearest_Watershed != Watershed)])
)

# Plot the distribution of natal origin values in a boxplot 

p <- ggplot(plot_data, aes(x = Classification, y = Natal_iso)) +
  geom_boxplot(alpha = .2) +
  geom_jitter(width = .1)  +
  labs(title = "Distribution of Natal Origin Values",
       x = "Classification",
       y = "Natal Origin Value")

plot(p)
?geom_jitter

# show a confusion matrix 
confusion_matrix <- table(Nearest_Watershed, Watershed)

# Plot the confusion matrix
p <- pheatmap(confusion_matrix, 
              main = "Confusion Matrix",
              fontsize = 10,
              cellwidth = 30,
              cellheight = 30,
              cluster_rows = FALSE,
              cluster_cols = FALSE,
              show_colnames = TRUE,
              show_rownames = TRUE,
              color = colorRampPalette(c("white", "blue"))(100))

# show confusion matrix in numbers 
confusion_matrix
