library(dplyr)
library(caret)
library(ggplot2)
library(tidyr)

# Clear the environment
rm(list = ls())

# Load all the processed data
All_Data <- read.csv("Data/Processed/all_data_combined.csv")

# Extract metadata and isotopic data
Metadata <- All_Data[, 2:5]
Iso <- All_Data[, 6:ncol(All_Data)]

# Add Watershed column to ModelData
ModelData <- Iso %>% mutate(Watershed = All_Data$Watershed)

# Split data into training and testing sets based on Year
testData <- ModelData %>% filter(All_Data$Year %in% c(2014, 2019))
trainData <- ModelData %>% filter(!All_Data$Year %in% c(2014, 2019))

# Cross-validation settings
trainControl <- trainControl(method = "cv", number = 5)  # 5-fold cross-validation

# Function to train and evaluate models
evaluate_model <- function(model_name, method, train_data, test_data) {
  model <- train(Watershed ~ ., data = train_data, method = method, trControl = trainControl)
  predictions <- predict(model, test_data)
  conf_matrix <- confusionMatrix(predictions, test_data$Watershed)
  
  cat("\n", model_name, " Model Summary:\n")
  print(model)
  cat("\nConfusion Matrix:\n")
  print(conf_matrix)
  
  # Return results dataframe
  correct_classified <- predictions == test_data$Watershed
  data.frame(
    ID = All_Data$Fish_id[All_Data$Year %in% c(2014, 2019)],
    Model = model_name,
    Correct_Classified = ifelse(correct_classified, "Yes", "No")
  )
}

# Train and evaluate models
rf_results <- evaluate_model("Random Forest", "rf", trainData, testData)
knn_results <- evaluate_model("KNN", "knn", trainData, testData)
svm_results <- evaluate_model("SVM", "svmRadial", trainData, testData)

# Combine all results into one dataframe
final_results <- bind_rows(rf_results, knn_results, svm_results)

# Print final results
print(final_results)




















# Extract misclassified fish_id for each model
rf_misclassified <- rf_results %>% filter(Correctly_classified == "N") %>% select(fish_id) %>% mutate(Model = "Random Forest")
knn_misclassified <- knn_results %>% filter(Correctly_classified == "N") %>% select(fish_id) %>% mutate(Model = "KNN")
svm_misclassified <- svm_results %>% filter(Correctly_classified == "N") %>% select(fish_id) %>% mutate(Model = "SVM")

# Combine the misclassified fish_id into a single data frame
misclassified_fish <- bind_rows(rf_misclassified, knn_misclassified, svm_misclassified)

# Rename the columns for clarity
colnames(misclassified_fish) <- c("Fish_ID", "Model")

# Display the resulting data frame
print(misclassified_fish)

# Optionally, save the data frame to a CSV file
write.csv(misclassified_fish, "Data/Model Results/testing/misclassified_fish.csv", row.names = FALSE)






















################ ASESSING PREFORMANCE 
################


# Compile confusion matrices
conf_matrix_list <- list(
  "Random Forest" = confMatrix_rf$table,
  "KNN" = confMatrix_knn$table,
  "SVM" = confMatrix_svm$table
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
  theme_grey()


print(rf_plot)

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
  theme_grey()

print(knn_plot)

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
  theme_grey()

print(svm_plot)


## Show all 3 plots using cowplot 
cowplot::plot_grid(rf_plot, knn_plot, svm_plot, nrow = 1)

############### 

# density plot of classified individuals by natal origin vs misclassified individuals by natal origin 

rf_results$Model <- "Random Forest"
knn_results$Model <- "KNN"
svm_results$Model <- "SVM"

# Combine the results into one data frame
all_results <- rbind(rf_results, knn_results, svm_results)

# Separate the correctly and incorrectly classified individuals
correct_classified <- all_results[all_results$Correctly_classified == "Y", ]
incorrect_classified <- all_results[all_results$Correctly_classified == "N", ]

# Create a combined dataset for both correctly and incorrectly classified
correct_classified$Classification <- "Correct"
incorrect_classified$Classification <- "Incorrect"

# Combine them into one dataframe
combined_results <- rbind(correct_classified, incorrect_classified)

# Create the density plot
density_plot <- ggplot(combined_results, aes(x = Natal_iso, fill = Classification)) +
  geom_density(alpha = 0.6) +  # Set transparency using alpha
  scale_fill_manual(values = c("Correct" = "dodgerblue", "Incorrect" = "firebrick")) +  # Custom colors for correct/incorrect
  labs(
    title = "Density Plot of Natal Isotopes for Correctly vs Incorrectly Classified Fish",
    x = "Natal Isotopes",
    y = "Density"
  ) +
  theme_minimal()

# Display the plot
print(density_plot)


stacked_bar_plot <- ggplot(combined_results, aes(x = Natal_iso, fill = Classification)) +
  geom_bar(stat = "count", position = "stack", width = 0.7) +  # Stacked bar plot
  scale_fill_manual(values = c("Correct" = "dodgerblue", "Incorrect" = "firebrick")) +  # Custom colors for correct/incorrect
  labs(
    title = "Count of Correctly vs Incorrectly Classified Fish by Natal Isotopes",
    x = "Natal Isotopes",
    y = "Count of Individuals"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


bin_breaks <- seq(0.703, 0.740, by = 0.0005)
combined_results$Natal_iso_bin <- cut(combined_results$Natal_iso, breaks = bin_breaks, include.lowest = TRUE, labels = FALSE)


bin_table <- combined_results %>%
  group_by(Natal_iso_bin, Classification) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  spread(Classification, Count, fill = 0)

bin_table$Natal_iso_bin <- sapply(bin_table$Natal_iso_bin, function(x) {
  paste0(format(bin_breaks[x], digits = 3), "-", format(bin_breaks[x + 1], digits = 4))
})

bin_table$Total <- bin_table$Correct + bin_table$Incorrect
bin_table$Proportion_Correct <- bin_table$Correct / bin_table$Total

bin_table <- bin_table %>%
  group_by(Natal_iso_bin) %>%
  mutate(Total = Correct + Incorrect, 
         Correct_Percentage = Correct / Total * 100)

# Create the bar chart
ggplot(bin_table, aes(x = Natal_iso_bin, y = Correct, fill = "Correct")) +
  geom_bar(stat = "identity", position = "stack", show.legend = FALSE) +
  geom_bar(aes(y = Incorrect, fill = "Incorrect"), stat = "identity", position = "stack", show.legend = FALSE) +
  scale_fill_manual(values = c("Correct" = "dodgerblue3", "Incorrect" = "#E7B800")) +
  labs(
    title = "Total Number of Individuals in Each Bin and Their Classification",
    x = "Natal Iso Bin",
    y = "Count"
  ) +
  geom_text(aes(y = Correct + Incorrect, label = paste0(round(Correct_Percentage, 1), "%")), 
            position = position_stack(vjust = .5), color = "white", angle = 90) +
  theme_grey() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(expand = c(0, 0))







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


























################ ASESSING PREFORMANCE 
################


# Compile confusion matrices
conf_matrix_list <- list(
  "Random Forest" = confMatrix_rf$table,
  "KNN" = confMatrix_knn$table,
  "SVM" = confMatrix_svm$table
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
  theme_grey()


print(rf_plot)

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
  theme_grey()

print(knn_plot)

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
  theme_grey()

print(svm_plot)


## Show all 3 plots using cowplot 
cowplot::plot_grid(rf_plot, knn_plot, svm_plot, nrow = 1)

############### 

# density plot of classified individuals by natal origin vs misclassified individuals by natal origin 

rf_results$Model <- "Random Forest"
knn_results$Model <- "KNN"
svm_results$Model <- "SVM"

# Combine the results into one data frame
all_results <- rbind(rf_results, knn_results, svm_results)

# Separate the correctly and incorrectly classified individuals
correct_classified <- all_results[all_results$Correctly_classified == "Y", ]
incorrect_classified <- all_results[all_results$Correctly_classified == "N", ]

# Create a combined dataset for both correctly and incorrectly classified
correct_classified$Classification <- "Correct"
incorrect_classified$Classification <- "Incorrect"

# Combine them into one dataframe
combined_results <- rbind(correct_classified, incorrect_classified)

# Create the density plot
density_plot <- ggplot(combined_results, aes(x = Natal_iso, fill = Classification)) +
  geom_density(alpha = 0.6) +  # Set transparency using alpha
  scale_fill_manual(values = c("Correct" = "dodgerblue", "Incorrect" = "firebrick")) +  # Custom colors for correct/incorrect
  labs(
    title = "Density Plot of Natal Isotopes for Correctly vs Incorrectly Classified Fish",
    x = "Natal Isotopes",
    y = "Density"
  ) +
  theme_minimal()

# Display the plot
print(density_plot)


stacked_bar_plot <- ggplot(combined_results, aes(x = Natal_iso, fill = Classification)) +
  geom_bar(stat = "count", position = "stack", width = 0.7) +  # Stacked bar plot
  scale_fill_manual(values = c("Correct" = "dodgerblue", "Incorrect" = "firebrick")) +  # Custom colors for correct/incorrect
  labs(
    title = "Count of Correctly vs Incorrectly Classified Fish by Natal Isotopes",
    x = "Natal Isotopes",
    y = "Count of Individuals"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


bin_breaks <- seq(0.703, 0.740, by = 0.0005)
combined_results$Natal_iso_bin <- cut(combined_results$Natal_iso, breaks = bin_breaks, include.lowest = TRUE, labels = FALSE)


bin_table <- combined_results %>%
  group_by(Natal_iso_bin, Classification) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  spread(Classification, Count, fill = 0)

bin_table$Natal_iso_bin <- sapply(bin_table$Natal_iso_bin, function(x) {
  paste0(format(bin_breaks[x], digits = 3), "-", format(bin_breaks[x + 1], digits = 4))
})

bin_table$Total <- bin_table$Correct + bin_table$Incorrect
bin_table$Proportion_Correct <- bin_table$Correct / bin_table$Total

bin_table <- bin_table %>%
  group_by(Natal_iso_bin) %>%
  mutate(Total = Correct + Incorrect, 
         Correct_Percentage = Correct / Total * 100)

# Create the bar chart
ggplot(bin_table, aes(x = Natal_iso_bin, y = Correct, fill = "Correct")) +
  geom_bar(stat = "identity", position = "stack", show.legend = FALSE) +
  geom_bar(aes(y = Incorrect, fill = "Incorrect"), stat = "identity", position = "stack", show.legend = FALSE) +
  scale_fill_manual(values = c("Correct" = "dodgerblue3", "Incorrect" = "#E7B800")) +
  labs(
    title = "Total Number of Individuals in Each Bin and Their Classification",
    x = "Natal Iso Bin",
    y = "Count"
  ) +
  geom_text(aes(y = Correct + Incorrect, label = paste0(round(Correct_Percentage, 1), "%")), 
            position = position_stack(vjust = .5), color = "white", angle = 90) +
  theme_grey() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(expand = c(0, 0))


