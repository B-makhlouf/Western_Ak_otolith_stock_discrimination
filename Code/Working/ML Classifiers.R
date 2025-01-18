library(dplyr)
library(caret)
library(ggplot2)


# Load the data
All_Data <- read.csv("Data/Intermediate/PCA_data.csv")

## TO TEST 
# Filter all data to be between .705-.709
All_Data <- All_Data %>%
  filter(All_Data$Natal_iso >= 0.706 & All_Data$Natal_iso <= 0.708)


# Preprocess data
Fish_ids <- All_Data$Fish_id
Watershed <- All_Data$Watershed
Natal_iso <- All_Data$Natal_iso

# Remove metadata columns and ensure numeric data
All_Data <- All_Data[,-c(1:4)]

All_Data$Watershed <- factor(Watershed)

# Split into training and test data 80/20
set.seed(123)
trainIndex <- createDataPartition(All_Data$Watershed, p = 0.8, list = FALSE)
trainData <- All_Data[trainIndex, ]
testData <- All_Data[-trainIndex, ]

####### Classifier 1: Random Forest #######
trainControl <- trainControl(method = "cv", number = 5)
rf_model <- train(
  Watershed ~ .,
  data = trainData,
  method = "rf",
  trControl = trainControl,
  importance = TRUE
)


print(rf_model)
importance <- varImp(rf_model, scale = TRUE)
print(importance)
plot(importance, top = 10, main = "Variable Importance (Random Forest)")
rf_predictions <- predict(rf_model, testData)
confMatrix_rf <- confusionMatrix(rf_predictions, testData$Watershed)
print(confMatrix_rf)


####### Classifier 2: K-Nearest Neighbors (KNN) #######
knn_model <- train(
  Watershed ~ .,
  data = trainData,
  method = "knn",
  trControl = trainControl
)
print(knn_model)
knn_predictions <- predict(knn_model, testData)
confMatrix_knn <- confusionMatrix(knn_predictions, testData$Watershed)
print(confMatrix_knn)

####### Classifier 3: Support Vector Machine (SVM) #######
svm_model <- train(
  Watershed ~ .,
  data = trainData,
  method = "svmRadial",
  trControl = trainControl
)
print(svm_model)
svm_predictions <- predict(svm_model, testData)
confMatrix_svm <- confusionMatrix(svm_predictions, testData$Watershed)
print(confMatrix_svm)

####### Classifier 4: Decision Trees #######
dt_model <- train(
  Watershed ~ .,
  data = trainData,
  method = "rpart",
  trControl = trainControl
)
print(dt_model)
dt_predictions <- predict(dt_model, testData)
confMatrix_dt <- confusionMatrix(dt_predictions, testData$Watershed)
print(confMatrix_dt)


################ 
################ ASESSING PREFORMANCE 
################


# Extract accuracies from confusion matrices
accuracy_data <- data.frame(
  Classifier = c("Random Forest", "KNN", "SVM", "Decision Tree"),
  Accuracy = c(
    confMatrix_rf$overall["Accuracy"],
    confMatrix_knn$overall["Accuracy"],
    confMatrix_svm$overall["Accuracy"],
    confMatrix_dt$overall["Accuracy"]
  )
)

# Create a bar graph of accuracy
# Define manual colors for each classifier
accuracy_data$Color <- c("steelblue", "darkseagreen3", "firebrick", "chocolate2")

# Create a bar graph of accuracy with manually assigned colors
accuracy_plot <- ggplot(accuracy_data, aes(x = Classifier, y = Accuracy, fill = Classifier)) +
  geom_bar(stat = "identity", color = "black", alpha = .6) +
  scale_fill_manual(values = accuracy_data$Color) +
  theme_classic() +
  labs(
    title = "Classifier Accuracy Comparison (on testing data)",
    x = "Classifier",
    y = "Accuracy"
  ) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

print(accuracy_plot)

# Compile confusion matrices
conf_matrix_list <- list(
  "Random Forest" = confMatrix_rf$table,
  "KNN" = confMatrix_knn$table,
  "SVM" = confMatrix_svm$table,
  "Decision Tree" = confMatrix_dt$table
)

# Display all confusion matrices
for (name in names(conf_matrix_list)) {
  cat("\nConfusion Matrix for", name, ":\n")
  print(conf_matrix_list[[name]])
}


All_Data$Fish_id <- Fish_ids
All_Data$Natal_iso<- Natal_iso

# Ensure Fish_id is assigned to both trainData and testData
trainData$Fish_id <- All_Data$Fish_id[trainIndex]      # Assign Fish_id to training set
testData$Fish_id <- All_Data$Fish_id[-trainIndex]  
testData$Natal_iso <- All_Data$Natal_iso[-trainIndex]  # Assign Fish_id to testing set
testData$Natal_iso <- All_Data$Natal_iso[-trainIndex]  # Assign Natal_iso to testing set

# Function to create a dataframe of correctly/incorrectly classified fish IDs
create_classification_results <- function(predictions, actuals, fish_ids) {
  result <- data.frame(
    fish_id = fish_ids,
    Correctly_classified = ifelse(predictions == actuals, "Y", "N")
  )
  return(result)
}

# For Random Forest
rf_results <- create_classification_results(rf_predictions, testData$Watershed, testData$Fish_id)

# Add natal iso to the results by matching fish id to ALL data
rf_results$Natal_iso <- All_Data$Natal_iso[match(rf_results$fish_id, All_Data$Fish_id)]

write.csv(rf_results, "Data/Model Results/testing/RF_classification_results.csv", row.names = FALSE)


# For KNN
knn_results <- create_classification_results(knn_predictions, testData$Watershed, testData$Fish_id)
knn_results$Natal_iso <- All_Data$Natal_iso[match(knn_results$fish_id, All_Data$Fish_id)]
write.csv(knn_results, "Data/Model Results/testing/KNN_classification_results.csv", row.names = FALSE)

# For SVM
svm_results <- create_classification_results(svm_predictions, testData$Watershed, testData$Fish_id)
svm_results$Natal_iso <- All_Data$Natal_iso[match(svm_results$fish_id, All_Data$Fish_id)]
write.csv(svm_results, "Data/Model Results/testing/SVM_classification_results.csv", row.names = FALSE)

# For Decision Tree
dt_results <- create_classification_results(dt_predictions, testData$Watershed, testData$Fish_id)
dt_results$Natal_iso <- All_Data$Natal_iso[match(dt_results$fish_id, All_Data$Fish_id)]
write.csv(dt_results, "Data/Model Results/testing/DT_classification_results.csv", row.names = FALSE)



##### Plot Natal Isos for correctly vs incorrectly identified fish in a box plot 
# For Random Forest
rf_results$Correctly_classified <- factor(rf_results$Correctly_classified, levels = c("Y", "N"))

rf_plot <- ggplot(rf_results, aes(x = Correctly_classified, y = Natal_iso, fill = Correctly_classified)) +
  geom_boxplot(alpha = .5, outlier.size = 2) +  # Adjust transparency for boxplot and show outliers
  geom_jitter(width = 0.1, alpha = .6, size = 2) +  # Increase jitter width and size
  scale_fill_manual(values = c("Y" = "dodgerblue", "N" = "firebrick")) +  # Set custom colors
  labs(
    title = "Natal Isotopes for Correctly vs Incorrectly Classified Fish (Random Forest)",
    x = "Correctly Classified",
    y = "Natal Isotopes"
  ) +
  theme_classic()

# For KNN
knn_results$Correctly_classified <- factor(knn_results$Correctly_classified, levels = c("Y", "N"))

knn_plot <- ggplot(knn_results, aes(x = Correctly_classified, y = Natal_iso, fill = Correctly_classified)) +
  geom_boxplot(alpha = .5, outlier.size = 2) +  # Adjust transparency for boxplot and show outliers
  geom_jitter(width = 0.1, alpha = .6, size = 2) +  # Increase jitter width and size
  scale_fill_manual(values = c("Y" = "dodgerblue", "N" = "firebrick")) +  # Set custom colors
  labs(
    title = "Natal Isotopes for Correctly vs Incorrectly Classified Fish (KNN)",
    x = "Correctly Classified",
    y = "Natal Isotopes"
  ) +
  theme_classic()

# For SVM

svm_results$Correctly_classified <- factor(svm_results$Correctly_classified, levels = c("Y", "N"))

svm_plot <- ggplot(svm_results, aes(x = Correctly_classified, y = Natal_iso, fill = Correctly_classified)) +
  geom_boxplot(alpha = .5, outlier.size = 2) +  # Adjust transparency for boxplot and show outliers
  geom_jitter(width = 0.1, alpha = .6, size = 2) +  # Increase jitter width and size
  scale_fill_manual(values = c("Y" = "dodgerblue", "N" = "firebrick")) +  # Set custom colors
  labs(
    title = "Natal Isotopes for Correctly vs Incorrectly Classified Fish (SVM)",
    x = "Correctly Classified",
    y = "Natal Isotopes"
  ) +
  theme_classic()

# For Decision Tree

dt_results$Correctly_classified <- factor(dt_results$Correctly_classified, levels = c("Y", "N"))

dt_plot <- ggplot(dt_results, aes(x = Correctly_classified, y = Natal_iso, fill = Correctly_classified)) +
  geom_boxplot(alpha = .5, outlier.size = 2) +  # Adjust transparency for boxplot and show outliers
  geom_jitter(width = 0.1, alpha = .6, size = 2) +  # Increase jitter width and size
  scale_fill_manual(values = c("Y" = "dodgerblue", "N" = "firebrick")) +  # Set custom colors
  labs(
    title = "Natal Isotopes for Correctly vs Incorrectly Classified Fish (Decision Tree)",
    x = "Correctly Classified",
    y = "Natal Isotopes"
  ) +
  theme_classic()


## Show all 4 plots using cowplot 
cowplot::plot_grid(rf_plot, knn_plot, svm_plot, dt_plot, nrow = 2)

#### 

# Check to see if there are individuals which were misclassified for both random forest and svm 
misclassified_fish <- intersect(rf_results$fish_id[rf_results$Correctly_classified == "N"], svm_results$fish_id[svm_results$Correctly_classified == "N"])

# Filter the original data to only include these fish
misclassified_data_BOTH <- All_Data[All_Data$Fish_id %in% misclassified_fish, ]


print(misclassified_fish)
