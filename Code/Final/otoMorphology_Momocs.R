# Load required libraries
#install.packages("devtools")

#devtools::install_github("geomorphR/geomorph")
#install.packages("rgl")
#install.packages("devtools")
# devtools::install_github("MomX/Momocs")
#install.packages("shapeR")
#install.packages("tidyverse")
#install.packages("shapeR")
#install.packages("Momocs")
#remove.packages("Momocs")
# install.packages("ggbiplot")
# install.packages("pheatmap")
# install.packages("plotly")

library(tidyverse)
library(shapeR)
library(Momocs)
library(ggbiplot)
library(plotly)
library(grDevices)
library(gridExtra)
library(grid)
library(caret)
library(e1071)   # for SVM
library(randomForest) # for RF




# remove everythng but shape and outlines only 
rm (list = ls()[!ls() %in% c("shape", "outlinesonly")])


######## Extract Outlines ##################################################################################################

# Use ShapeR to extract outlines
shape <- shapeR("/Users/benjaminmakhlouf/Research_repos/Western_Ak_otolith_stock_discrimination/ShapeAnalysis", "FISH.csv")
outlinesonly <- detect.outline(shape, threshold = 0.2, write.outline.w.org = FALSE)

# Extract outlines for each class
yk_outlines <- outlinesonly@outline.list$YK
kk_outlines <- outlinesonly@outline.list$KK
nk_outlines <- outlinesonly@outline.list$NK

# Combine all outlines into one list
all_outlines <- c(kk_outlines, nk_outlines, yk_outlines)

# Extract picnames and watersheds
picnames <- outlinesonly@master.list.org$picname
watershed <- outlinesonly@master.list.org$Watershed

# MANUALLY add and remove bad otolith samples   
badOtos<- c("2022_kk_302", "2022_kk_307", "2022_kk_309", "2022_kk_310", "2022_kk_313", "2022_kk_315","2022_kk_322","2022_kk_329","2022_kk_363","2023_kk_108","2023_kk_128","2023_kk_133","2023_kk_134","2023_kk_141","2015_nk_002","2015_nk_005","2015_nk_006","2015_nk_021","2015_nk_035","2015_nk_047","2015_nk_093","2015_nk_100","2017_nk_146","2019_nk_163","2020_yk_132","2020_yk_182","2023_yk_037","2023_yk_061")
badOtos_indices <- which(picnames %in% badOtos)

# Remove bad outlines, picnames, and watershed
filtered_outlines <- all_outlines[-badOtos_indices]
picnames <- picnames[-badOtos_indices]
watershed <- watershed[-badOtos_indices]

table(watershed) ## HOW Many we got ?? 

# Randomly choose 20 watershed indices from each watershed
# indices <- c(
#   sample(which(watershed == "Kuskokwim"), 80),
#   sample(which(watershed == "Nushagak"), 80),
#   sample(which(watershed == "Yukon"), 80)
# )

### IF I want to select all indices 
indices <- c(
  which(watershed == "Kuskokwim"),
  which(watershed == "Nushagak"),
  which(watershed == "Yukon")
)


# Subsample all outlines, picnames, and watershed to these indices
filtered_outlines <- filtered_outlines[indices]
picnames <- picnames[indices]
watershed <- watershed[indices]

# Initialize lists to store coordinates and associated information
coo <- list()
fac <- data.frame(picname = picnames, watershed = watershed, stringsAsFactors = FALSE)

# Process all the outlines into a format that can be added to the coo object 
for (i in seq_along(filtered_outlines)) {
  shape <- filtered_outlines[[i]]
  x <- shape$X
  y <- shape$Y
  coo[[i]] <- cbind(x, y)
}


num_points <- sapply(coo, nrow)

# Compute the mean number of points per outline
mean_num_points <- mean(num_points)
print(mean_num_points) #5958.648
n_points <- 200 #mean_num_points  # The mean is what everything should be interpolated to. This may be overkill 

# Interpolate all outlines to have the same number of points using the MOMOCS functionality
coo_interpolated <- lapply(coo, Momocs::coo_interpolate, n = n_points)

# Create the "Coo" object with interpolated outlines. This is the data file needed for MOMOCS analysis 
OtoOutlines <- Out(coo_interpolated, fac)

################################################################################
################ Figure showing the mean shape and the quartiles from that shape 
################################################################################

if (T){
  
    # Center outlines
    OtoOutlines <- coo_center(OtoOutlines)
    
    # Compute distances from center for each outline
    distances <- lapply(OtoOutlines$coo, Momocs::coo_centdist)
    
    # Convert to dataframe
    dist_df <- do.call(rbind, lapply(seq_along(distances), function(i) {
      data.frame(
        picname = fac$picname[i],
        watershed = fac$watershed[i],
        point_id = seq_along(distances[[i]]),
        distance = distances[[i]]
      )
    }))
    
    # Compute median and quartiles for each point in each watershed
    summary_by_watershed <- dist_df %>%
      group_by(watershed, point_id) %>%
      summarize(
        Q1_distance = quantile(distance, 0.25, na.rm = TRUE),
        median_distance = quantile(distance, 0.50, na.rm = TRUE),
        Q3_distance = quantile(distance, 0.75, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Convert outlines into a dataframe with X, Y coordinates
    coo_df <- do.call(rbind, lapply(seq_along(OtoOutlines$coo), function(i) {
      data.frame(
        picname = fac$picname[i],
        watershed = fac$watershed[i],
        point_id = seq_along(OtoOutlines$coo[[i]][, 1]),
        X = OtoOutlines$coo[[i]][, 1],
        Y = OtoOutlines$coo[[i]][, 2]
      )
    }))
    
    # Merge distances with coordinates to get the full dataframe
    dist_with_coords <- left_join(dist_df, coo_df, by = c("picname", "watershed", "point_id"))
    
    # Extract points that belong to each quartile
    quartile_shapes <- dist_with_coords %>%
      left_join(summary_by_watershed, by = c("watershed", "point_id")) %>%
      filter(distance <= Q1_distance | distance >= Q3_distance) %>%
      mutate(quartile = case_when(
        distance <= Q1_distance ~ "Q1",
        distance >= Q3_distance ~ "Q3"
      ))
    
    # For each watershed, compute the convex hull for Q1 and Q3 points
    hull_Q1 <- quartile_shapes %>%
      filter(quartile == "Q1") %>%
      group_by(watershed) %>%
      do({
        chull_points <- chull(.$X, .$Y)
        data.frame(X = .$X[chull_points], Y = .$Y[chull_points])
      })
    
    hull_Q3 <- quartile_shapes %>%
      filter(quartile == "Q3") %>%
      group_by(watershed) %>%
      do({
        chull_points <- chull(.$X, .$Y)
        data.frame(X = .$X[chull_points], Y = .$Y[chull_points])
      })
    
    # Calculate the mean shape per watershed
    mean_shape_by_watershed <- coo_df %>%
      group_by(watershed, point_id) %>%
      summarize(
        mean_X = mean(X, na.rm = TRUE),
        mean_Y = mean(Y, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Add a column to indicate quartiles for fill color
    hull_Q1$quartile <- "Q1"
    hull_Q3$quartile <- "Q3"
    
    # Plot with convex hulls for Q1 and Q3, median points, and mean shape per watershed
    ggplot() +
      # Shaded region for Q1 and Q3 with quartile legend
      geom_polygon(data = hull_Q1, aes(x = X, y = Y, fill = quartile), alpha = 0.2) +
      geom_polygon(data = hull_Q3, aes(x = X, y = Y, fill = quartile), alpha = 0.2) +
      # Plot median points
      geom_point(data = quartile_shapes %>% filter(distance == median_distance), 
                 aes(x = X, y = Y), color = "black", size = 3) +
      # Add mean shape per watershed as a line (use the mean X and Y coordinates for each watershed)
      geom_path(data = mean_shape_by_watershed, aes(x = mean_X, y = mean_Y, group = watershed), 
                color = "grey15", size = 1, linetype = "solid") +
      facet_wrap(~ watershed, ncol = 1) +
      coord_equal() +
      labs(title = "Otolith Shape Variation with Mean Shape Overlay (Per Watershed)", 
           x = "X Coordinate", y = "Y Coordinate") +
      scale_fill_manual(values = c("Q1" = "blue", "Q3" = "red"), name = "Quartile") +  # Add custom fill colors and legend
      theme_minimal()
  }
  

################################################################################
################################################################################
################################################################################

## Elliptical Fourier Analysis , with normalization being FALSE 
## "Foo-ree-ay"
## nb.h is the # of harmonics
Oto.fourier<- efourier(OtoOutlines, nb.h = 10, norm = TRUE)

########### 
########### PCA and visualize 
Oto.pca<- PCA(Oto.fourier) # run a PCA 
watershed_colors <- c( "Kuskokwim" = "firebrick", "Nushagak" = "darkgreen", "Yukon" = "dodgerblue")
plot(Oto.pca, ~watershed, col = watershed_colors) #Plot the PCA 

# make watershed a factor 
Oto.fourier$watershed <- as.factor(Oto.f$watershed)

#### Try the PCA, without the first harmonic
Oto.fourier_hm1rm <- rm_harm(Oto.f, 1)
Oto.pca.hm1rm<- PCA(Oto.fourier_hm1rm)

plot(Oto.pca.hm1rm, ~watershed, col = watershed_colors)

# Create the first PCA plot (with all harmonics)
pca_plot1 <- ggbiplot(Oto.pca, 
                      obs.scale = .5, 
                      var.scale = .5, 
                      groups = Oto.f$watershed, 
                      ellipse = TRUE, 
                      ellipse.linewidth = .3, 
                      ellipse.alpha = .1, 
                      circle = FALSE)

# Create the second PCA plot (without the first harmonic)
pca_plot2 <- ggbiplot(Oto.pca.hm1rm, 
                      obs.scale = .5, 
                      var.scale = .5, 
                      groups = Oto.f$watershed, 
                      ellipse = TRUE, 
                      ellipse.linewidth = .3, 
                      ellipse.alpha = .1, 
                      circle = FALSE)

# Display the two PCA plots side by side with labels
grid.arrange(
  pca_plot1, pca_plot2, 
  ncol = 2, 
  top = textGrob("PCA of Otolith Shapes", gp = gpar(fontsize = 15, fontface = "bold"))
)

# Add custom labels using grid.text
grid.text("All harmonics", x = 0.25, y = 0.98, gp = gpar(fontsize = 12, fontface = "bold"))
grid.text("Harmonic 1 removed", x = 0.75, y = 0.98, gp = gpar(fontsize = 12, fontface = "bold"))

# Create a scree plot to visualize the contribution of each PC
scree_plot(Oto.pca)
scree_plot(Oto.pca.hm1rm)

# visualize the principal component contribution 
PCcontrib(Oto.pca,1:4) #Visual contribution of PC
PCcontrib(Oto.pca.hm1rm,1:4) #Visual contribution of PC


#############################################################################################
#################### Classification 
#############################################################################################

# Prepare the data
coeff_df <- as.data.frame(Oto.fourier$coe)
coeff_df$watershed <- Oto.fourier$fac$watershed

# Remove constant variables (if applicable)
constant_vars <- c(1, 11, 21)  # Define constant variables to be removed
coeff_df_filtered <- coeff_df[, -constant_vars]  # Remove the specified columns

# Split the data into training and testing sets (80% training, 20% testing)
set.seed(123)  # For reproducibility
train_index <- createDataPartition(coeff_df_filtered$watershed, p = 0.8, list = FALSE)
train_data <- coeff_df_filtered[train_index, ]
test_data <- coeff_df_filtered[-train_index, ]

# Set up 10-fold cross-validation for the training set
train_control <- trainControl(method = "cv", number = 10)

# --- 1. Fit and evaluate MLP (Multi-Layer Perceptron) ---
mlp_model <- train(watershed ~ ., data = train_data, method = "mlp", trControl = train_control)

# Print MLP model summary
print(mlp_model)

# Make predictions on the testing set
mlp_predictions <- predict(mlp_model, newdata = test_data)
# make both factors 
mlp_predictions <- as.factor(mlp_predictions)
test_data$watershed <- as.factor(test_data$watershed)
# Confusion Matrix for MLP
mlp_confusion_matrix <- confusionMatrix(as.factor(mlp_predictions), test_data$watershed)
print(mlp_confusion_matrix)


# --- 2. Fit and evaluate SVM (Support Vector Machine) ---
svm_model <- train(watershed ~ ., data = train_data, method = "svmRadial", trControl = train_control)

# Print SVM model summary
print(svm_model)

# Make predictions on the testing set
svm_predictions <- predict(svm_model, newdata = test_data)

# Confusion Matrix for SVM
svm_confusion_matrix <- confusionMatrix(as.factor(svm_predictions), test_data$watershed)

print(svm_confusion_matrix)

# --- 3. Fit and evaluate Random Forest (RF) ---
rf_model <- train(watershed ~ ., data = train_data, method = "rf", trControl = train_control)

# Print Random Forest model summary
print(rf_model)

# Make predictions on the testing set
rf_predictions <- predict(rf_model, newdata = test_data)

# Confusion Matrix for Random Forest
rf_confusion_matrix <- confusionMatrix(as.factor(rf_predictions), test_data$watershed)
print(rf_confusion_matrix)

# --- 4. Fit and evaluate KNN (K-Nearest Neighbors) ---
knn_model <- train(watershed ~ ., data = train_data, method = "knn", trControl = train_control)

# Print KNN model summary
print(knn_model)

# Make predictions on the testing set
knn_predictions <- predict(knn_model, newdata = test_data)

# Confusion Matrix for KNN
knn_confusion_matrix <- confusionMatrix(as.factor(knn_predictions), test_data$watershed)
print(knn_confusion_matrix)

# Summary of all models' performance
cat("\n--- Model Performance Summary ---\n")
cat("MLP Accuracy: ", mlp_confusion_matrix$overall['Accuracy'], "\n")
cat("SVM Accuracy: ", svm_confusion_matrix$overall['Accuracy'], "\n")
cat("Random Forest Accuracy: ", rf_confusion_matrix$overall['Accuracy'], "\n")
cat("KNN Accuracy: ", knn_confusion_matrix$overall['Accuracy'], "\n")



#############################################################################################

## Procrustes alignment
Oto.aligned<-fgProcrustes(OtoOutlines) ### Doesnt work because all of the outlines dont have the same size 
panel(OtoOutlines, fac = "watershed", names = FALSE)  # Not aligned, raw outlines
panel(Oto.aligned, fac = "watershed", names = FALSE)  # Aligned


#############################################################################################
# Misc. 

### Heirarchial clustering 
CLUST(Oto.fourier, labels = ~watershed) #Heirarchial clustering

## K means clustering 
KMEANS(Oto.pca, centers = 5)




