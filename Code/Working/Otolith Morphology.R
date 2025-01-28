library(dplyr)
library(shapeR)
library(ggplot2)
shape = shapeR("/Users/benjaminmakhlouf/Research_repos/Western_Ak_otolith_stock_discrimination/ShapeAnalysis", "FISH.csv")
shape = detect.outline(shape, threshold = 0.2, write.outline.w.org = TRUE)
shape = generateShapeCoefficients(shape)
shape = enrich.master.list(shape) #connect to the metadata


plotWaveletShape(shape, "Watershed", show.angle = TRUE, lwd = 2,lty = 1)
Watershed = factor(getMasterlist(shape)$Watershed) #Get factor of interest (Watershed)

?plotWaveletShape


####################### RF Classification
################################################################################


library(ipred)

# Extract the masterlist 
masterlist<- getMasterlist(shape)

#remove rows with NA from masterlist 
masterlist<- masterlist[complete.cases(masterlist),]


wavelet<- masterlist[10:118]
labels<- masterlist$Watershed

# run a PCA on the wavelet data
pca<- prcomp(wavelet, center = TRUE, scale = TRUE)
summary(pca)
pca_results<- as.data.frame(pca$x)

# Add labels
pca_results$Watershed<- labels

# Plot principal component 1 vs 2 and color by labels 
pca_plot <- ggplot(pca_results, aes_string(x = pca_results$PC1, y = pca_results$PC2, color = "Watershed")) +
  geom_point(size = 1, alpha = .8) +
  theme_classic() +
  labs(title = "PCA of Iso Values by Watershed")+
  theme(legend.title = element_blank())

pca_plot


# Split the data into training and testing sets
library(caret)

set.seed(123)

#split wavelet into test and train 
trainIndex <- createDataPartition(labels, p = .8, 
                                  list = FALSE, 
                                  times = 1)

wavelet_train <- wavelet[trainIndex,]

wavelet_test <- wavelet[-trainIndex,]

labels_train <- labels[trainIndex]

labels_test <- labels[-trainIndex]

### Random forest using caret 
library(caret)

# Define the control using a random forest
control <- trainControl(method="cv", number=10)

# Train the model

model <- train(wavelet_train, labels_train, method="rf", trControl=control)

# Print the model to the console
print(model)

# Make predictions
predictions <- predict(model, wavelet_test)

# Confusion matrix

#make sure predictions and labels_test are factors
predictions <- as.factor(predictions)
labels_test <- as.factor(labels_test)
confusionMatrix(predictions, labels_test)
