#### This script will run ML Classifiers on interpolated subset of data 

library(dplyr)
library(caret)
library(here)

All_Data<- read.csv("Data/Intermediate/PCA_data.csv")

Fish_ids<- All_Data$Fish_id
Natal_iso<- All_Data$Natal_iso
Watershed<- All_Data$Watershed

#Remove the first 4 columns (metadata) from the data frame 
All_Data<- All_Data[,-c(1:4)]

# Add "Watershed" back in 
All_Data$Watershed <- Watershed

# Split into training and test data 80/20
set.seed(123)
trainIndex <- createDataPartition(Natal_iso, p = 0.8, list = FALSE)
trainData <- All_Data[trainIndex, ]
testData <- All_Data[-trainIndex, ]

####### Classifier 1: Random Forest ####### 

trainControl <- trainControl(method = "cv", number = 5)  # 5-fold cross-validation

rf_model <- train(
  Watershed ~ .,
  data = trainData,
  method = "rf",
  trControl = trainControl,
  importance = TRUE
)

#Model Summary
print(rf_model)
importance <- varImp(rf_model, scale = TRUE)
print(importance)
plot(importance, top = 10, main = "Variable Importance (Random Forest)")


# Make sure both are factors so we can compare between them 
rf_predictions <- factor(rf_predictions, levels = levels(testData$Watershed))
testData$Watershed <- factor(testData$Watershed)  

#Evaluating the model with the testing data 
rf_predictions <- predict(rf_model, testData)
confMatrix <- confusionMatrix(rf_predictions, testData$Watershed)
print(confMatrix)




