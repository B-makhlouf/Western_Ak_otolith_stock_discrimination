# This script is to explore the potential to use otolith morphological metrics to classify. 

### Warning: Before running this analysis you must update the FISH.csv so that all otoliths are included. This can be done with the script FISH_metadata_ShapeAnalysis.R


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

# Use ShapeR to extract outlines
shape <- shapeR("/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/ShapeAnalysis", "FISH.csv")
outlinesonly <- detect.outline(shape, threshold = 0.2, write.outline.w.org = FALSE) # for this to work you need to have the otolith outlines in "ShapeAnalysis/FIXED". 
# The first time you run this you need to have write.outline.w.org = TRUE so that there are outlines available for QC. This is a much slower process. After the first time 
# you can set this = FALSE for quick analysis. 

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
badOtos<- c("2021_kk_306", "2021_kk_314","2021_kk_341", "2021_kk_348","2022_kk_302","2022_kk_307","2022_kk_309","2022_kk_310","2022_kk_313","2022_kk_315","2022_kk_322","2022_kk_328","2022_kk_329","2022_kk_363","2023_kk_131","2023_kk_133","2023_kk_134","2023_kk_141",
            "2015_nk_002", "2015_nk_003","2015_nk_006","2015_nk_021","2015_nk_035","2015_nk_047","2017_nk_146","2019_nk_163","2020_yk_132","2020_yk_182","2020_yk_314","2021_yk_314","2021_yk_315","2021_yk_317","2021_yk_318","2021_yk_323","2021_yk_358","2021_yk_363","2021_yk_384","2021_yk_398","2023_yk_037","2023_yk_061","2023_kk_108","2015_nk_005","2021_yk_356")
badOtos_indices <- which(picnames %in% badOtos)



# how many indices are therE? 
length(badOtos_indices) 


# Remove bad outlines, picnames, and watershed
filtered_outlines <- all_outlines[-badOtos_indices]
picnames <- picnames[-badOtos_indices]
watershed <- watershed[-badOtos_indices]

table(watershed) ## HOW Many we got ?? 
#how many total? 
length(filtered_outlines)

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


###############################################################################################################


# Visualize all of them 
panel(OtoOutlines, fac = "watershed", names = FALSE)  # Not aligned, raw outlines

## Elliptical Fourier Analysis , with normalization being FALSE 
## "Foo-ree-ay"
## nb.h is the # of harmonics
Oto.fourier<- efourier(OtoOutlines, nb.h = 10, norm = TRUE)

########### 
########### PCA and visualize 
Oto.pca<- PCA(Oto.fourier) # run a PCA 
watershed_colors <- c( "Kuskokwim" = "firebrick", "Nushagak" = "darkgreen", "Yukon" = "dodgerblue")
plot(Oto.pca, ~watershed, col = watershed_colors) #Plot the PCA 


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
train_index <- createDataPartition(coeff_df_filtered$watershed, p = 0.7, list = FALSE)
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

# Extract class-specific accuracies from each model's confusion matrix
mlp_class_acc <- mlp_confusion_matrix$byClass[,"Sensitivity"]  # Using Sensitivity (recall) as class accuracy
svm_class_acc <- svm_confusion_matrix$byClass[,"Sensitivity"]
rf_class_acc <- rf_confusion_matrix$byClass[,"Sensitivity"]
knn_class_acc <- knn_confusion_matrix$byClass[,"Sensitivity"]

# Combine into a data frame
accuracy_df <- data.frame(
  Class = rep(names(mlp_class_acc), 4),
  Accuracy = c(mlp_class_acc, svm_class_acc, rf_class_acc, knn_class_acc),
  Model = rep(c("MLP", "SVM", "Random Forest", "KNN"), each = length(mlp_class_acc))
)

# Create the plot
library(ggplot2)

# ggplot(accuracy_df, aes(x = Class, y = Accuracy, fill = Model)) +
#   geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
#   scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
#   labs(title = "Class-Specific Accuracy Comparison Across Models",
#        x = "Watershed Class",
#        y = "Accuracy (Sensitivity)",
#        fill = "Model") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1),
#         plot.title = element_text(hjust = 0.5)) +
#   scale_fill_brewer(palette = "Set1")

library(ggplot2)
library(scales)
library(viridis)  # For color scales

# Create the heatmap plot
ggplot(accuracy_df, aes(x = Model, y = Class, fill = Accuracy)) +
  geom_tile(color = "white", linewidth = 0.5) +  # Add white borders between tiles
  geom_text(aes(label = percent(Accuracy, accuracy = 1)), 
            color = "white", size = 3.5, fontface = "bold") +  # Add percentage labels
  scale_fill_viridis(
    option = "plasma",
    direction = -1,
    labels = percent_format(),
    limits = c(0, 1),
    begin = 0.2,  # Skip the very dark colors
    end = 0.9     # Skip the very light colors
  ) +
  labs(
    title = "Classification Accuracy Heatmap",
    x = "Machine Learning Model",
    y = "Watershed Class",
    fill = "Accuracy"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    panel.grid = element_blank(),
    legend.position = "right",
    legend.key.height = unit(1.5, "cm"),
    text = element_text(family = "sans")
  ) +
  coord_fixed(ratio = 0.8)  # Make tiles rectangular rather than square

# ggplot(accuracy_df, aes(x = Class, y = Accuracy, fill = Class)) +
#   geom_bar(stat = "identity") +
#   facet_wrap(~ Model, ncol = 2) +
#   scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
#   labs(title = "Class-Specific Accuracy by Model",
#        x = "Watershed Class",
#        y = "Accuracy (Sensitivity)") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1),
#         plot.title = element_text(hjust = 0.5),
#         legend.position = "none")

# ggplot(accuracy_df, aes(x = Model, y = Accuracy, color = Class, group = Class)) +
#   geom_point(size = 3) +
#   geom_line() +
#   scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
#   labs(title = "Class Accuracy Across Models",
#        x = "Model",
#        y = "Accuracy (Sensitivity)",
#        color = "Class") +
#   theme_minimal() +
#   theme(plot.title = element_text(hjust = 0.5))

# Extract F1 scores from each model's confusion matrix
mlp_f1 <- mlp_confusion_matrix$byClass[,"F1"]
svm_f1 <- svm_confusion_matrix$byClass[,"F1"] 
rf_f1 <- rf_confusion_matrix$byClass[,"F1"]
knn_f1 <- knn_confusion_matrix$byClass[,"F1"]

# Combine into a data frame
f1_df <- data.frame(
  Class = rep(names(mlp_f1), 4),
  F1_Score = c(mlp_f1, svm_f1, rf_f1, knn_f1),
  Model = rep(c("MLP", "SVM", "Random Forest", "KNN"), each = length(mlp_f1))
)

# Create the F1 score heatmap
ggplot(f1_df, aes(x = Model, y = Class, fill = F1_Score)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = percent(F1_Score, accuracy = 1)), 
            color = "white", size = 3.5, fontface = "bold") +
  scale_fill_viridis(
    option = "viridis",  # Different color scheme for distinction
    direction = -1,
    labels = percent_format(),
    limits = c(0, 1),
    begin = 0.1,
    end = 0.9
  ) +
  labs(
    title = "Classification Performance (F1 Score)",
    x = "Machine Learning Model",
    y = "Watershed Class", 
    fill = "F1 Score"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    panel.grid = element_blank(),
    legend.position = "right",
    legend.key.height = unit(1.5, "cm")
  ) +
  coord_fixed(ratio = 0.8)
