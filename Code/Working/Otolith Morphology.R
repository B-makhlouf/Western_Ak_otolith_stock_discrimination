library(dplyr)
library(shapeR)
library(ggplot2)

shape = shapeR("/Users/benjaminmakhlouf/Research_repos/Western_Ak_otolith_stock_discrimination/ShapeAnalysis", "FISH.csv")
shape = detect.outline(shape, threshold = 0.2, write.outline.w.org = FALSE)


shape = generateShapeCoefficients(shape)
shape = enrich.master.list(shape) #connect to the metadata


plotWaveletShape(shape, "Watershed", show.angle = TRUE, lwd = 2,lty = 1)
Watershed = factor(getMasterlist(shape)$Watershed) #Get factor of interest (Watershed)


####################### RF Classification
################################################################################

library(ggfortify)


# Extract the masterlist 
masterlist<- getMasterlist(shape)

#remove rows with NA from masterlist 
masterlist<- masterlist[complete.cases(masterlist),]


wavelet<- masterlist[11:118]
labels<- masterlist$Watershed

# run a PCA on the wavelet data
pca<- prcomp(wavelet, center = TRUE, scale = TRUE)
summary(pca)
pca_results<- as.data.frame(pca$x)

# Add labels
pca_results$Watershed<- labels

# remove those individuals with a pc2 of less than -8 OR a pc1 of less than -20
pca_results<- pca_results[!(pca_results$PC2 < -8 | pca_results$PC1 < -20),]

# Plot principal component 1 vs 2 and color by labels 
ggplot(pca_results, aes(x = PC1, y = PC2, color = Watershed)) +
  geom_point(size = 1, alpha = 0.8) +
  stat_ellipse(type = "norm") +  # Add probability ellipses
  theme_classic() +
  labs(title = "PCA of Iso Values by Watershed") +
  theme(legend.title = element_blank())

 ggplot(pca_results, aes(x = PC1, y = PC3, color = Watershed)) +
  geom_point(size = 1, alpha = 0.8) +
  stat_ellipse(type = "norm") +  # Add probability ellipses
  theme_classic() +
  labs(title = "PCA of Iso Values by Watershed") +
  theme(legend.title = element_blank())

 ggplot(pca_results, aes(x = PC2, y = PC3, color = Watershed)) +
  geom_point(size = 1, alpha = 0.8) +
  stat_ellipse(type = "norm") +  # Add probability ellipses
  theme_classic() +
  labs(title = "PCA of Iso Values by Watershed") +
  theme(legend.title = element_blank())



pca_plot

########## simple classifier test 

### Random forest using caret 
library(caret)

wavelet_all<- wavelet
labels_all<- labels

# Split the wavelet data  into training and testing 80.20

# Assuming wavelet and labels are your full dataset and response variable
wavelet_all <- wavelet
labels_all <- labels

# Set seed for reproducibility
set.seed(123)

# Split data into training (80%) and testing (20%)
trainIndex <- createDataPartition(labels_all, p = 0.6, list = FALSE)
wavelet_train <- wavelet_all[trainIndex, ]
wavelet_test <- wavelet_all[-trainIndex, ]
labels_train <- labels_all[trainIndex]
labels_test <- labels_all[-trainIndex]

# Define the control using a random forest
control <- trainControl(method="cv", number=5)

# Train the model
model <- train(wavelet_train, labels_train, method="rf", trControl=control)

# Print the model to the console
print(model)

# Make predictions
predictions <- predict(model, wavelet_test)

#make sure predictions and labels_test are factors
predictions <- as.factor(predictions)
labels_test <- as.factor(labels_test)
confusionMatrix(predictions, labels_test)

table(labels_all)  # For the full dataset
table(labels_train)  # For the training set
table(labels_test)   # For the testing set

