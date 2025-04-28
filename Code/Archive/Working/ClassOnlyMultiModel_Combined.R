######### This script does not do the PCA and visualization, only classification using multiple models and exports the model and the model results 

######### For Sr8786 

library(viridis)
library(patchwork)
library(plotly)
library(tidyverse)
library(here)
library(caret)
library(shiny)

######################

source(here("/Users/benjaminmakhlouf/Research_repos/Western_Ak_otolith_stock_discrimination/Code/Helper Code/PCA_functions.R"))### This script contains helper functions to run PCA and a few important figures
source(here("/Users/benjaminmakhlouf/Research_repos/Western_Ak_otolith_stock_discrimination/Code/Helper Code/Raw_Data_Preprocessing.R"))
All_Metadata<- read.csv(here("Data/Final/Metadata_and_QC.csv"))

########### Read in the most recent processed data 
################################################################################
# Define the data types to iterate over
data_types <- c("RAW", "GAM", "MA")

# Define landmarks used during processing
#landmark_filter <- c("Core", "Fw")
landmark_filter<- c("Core")
landmark_filter<- c("Fw")

# Load processed data for the current data_type
#processed_data <- load_processed_data(data_type, landmark_filter)

processed_data<- read.csv("/Users/benjaminmakhlouf/Research_repos/Western_Ak_otolith_stock_discrimination/Data/Classification_ts_matrices/Sr88/Processed_Core_Fw_Sr88_Iso.csv")

# Initialize an empty dataframe to store all results
all_results_df <- data.frame()


  
  # Merge with metadata
  AnalysisDataAll <- processed_data %>%
    left_join(All_Metadata %>% select(-Year), by = c("Fish_id" = "Fish_ID")) %>%
    select((ncol(.)-12):ncol(.), everything()) %>%
    # Select only those with QC = "Yes"
    filter(QC_Grade == "Yes")
  
  # Separate metadata and isotope time series data
  Analysis_metadata <- AnalysisDataAll[,1:17]  
  Analysis_ts_data <- AnalysisDataAll[,18:ncol(AnalysisDataAll)]  
  
  # Ensure selected_data is a dataframe and add Watershed
  ModelData <- Analysis_ts_data %>% 
    as.data.frame() %>% 
    mutate(Watershed = Analysis_metadata$Watershed)
  
  # Split data into training (80%) and testing (20%)
  set.seed(123)
  trainIndex <- createDataPartition(ModelData$Watershed, p = 0.8, list = FALSE)
  traindata <- ModelData[trainIndex, ]
  testdata <- ModelData[-trainIndex, ]
  
  # Count number of samples for each Watershed in training and testing sets
  train_counts <- as.data.frame(table(traindata$Watershed))
  colnames(train_counts) <- c("Watershed", "Train_Count")
  
  test_counts <- as.data.frame(table(testdata$Watershed))
  colnames(test_counts) <- c("Watershed", "Test_Count")
  
  # Set up cross-validation
  control <- trainControl(method = "cv", number = 5, classProbs = TRUE)  
  
  # Define a list of models to train
  models <- c("rf", "svmRadial", "knn")
  
  for (model_type in models) {
    
    # Train the model
    model <- train(Watershed ~ ., data = traindata, method = model_type, trControl = control)
    
    # Make predictions (both class labels and probabilities)
    predictions <- predict(model, testdata)
    probabilities <- predict(model, testdata, type = "prob")
    
    # Extract IDs for test samples
    idScores <- Analysis_metadata[-trainIndex, ] %>%
      select(Fish_id) %>%
      mutate(
        Predicted = predictions,
        Actual = testdata$Watershed,
        Confidence = apply(probabilities, 1, max),
        Correct = Predicted == Actual
      ) %>%
      bind_cols(probabilities)  # Add probability columns
    
    # Convert factors
    idScores <- idScores %>%
      mutate(Predicted = as.factor(Predicted), Actual = as.factor(Actual))
    
    # Compute confusion matrix
    conf_matrix <- confusionMatrix(idScores$Predicted, idScores$Actual)
    
    # Extract overall classification accuracy
    overall_accuracy <- conf_matrix$overall["Accuracy"]
    
    # Extract sensitivity & specificity for each class
    class_metrics <- as.data.frame(conf_matrix$byClass)
    class_metrics$Watershed <- rownames(class_metrics)
    
    # Delete the row names 
    rownames(class_metrics) <- NULL
    
    # Remove "Class:_" from class names using gsub
    class_metrics$Watershed <- gsub("Class: ", "", class_metrics$Watershed)
    
    # Combine training, testing, accuracy, and class metrics into a single dataframe
    results_df <- train_counts %>%
      full_join(test_counts, by = "Watershed") %>%
      full_join(class_metrics, by = "Watershed") %>%
      mutate(Overall_Accuracy = overall_accuracy)
    
    # Add model specifications to the results dataframe
    results_df <- results_df %>%
      mutate(
        Model_Landmarks = paste(landmark_filter, collapse = ","),
        Data_Type = data_type,
        Model_Method = model_type
      )
    
    # Append to the main results dataframe
    all_results_df <- bind_rows(all_results_df, results_df)
  }


# View the final combined dataset
print(all_results_df)

# Save the trained Rf Model 
Modelname<- paste("Rf_Sr88_Sr8786_", paste(landmark_filter, collapse = "_"), ".rds", sep = "")
Filename<- paste("/Users/benjaminmakhlouf/Research_repos/Western_Ak_otolith_stock_discrimination/Models/rf_models/", Modelname, sep = "")
saveRDS(model, Filename)

# Save the trained SVM Model
Modelname<- paste("SVM_Sr88_Sr8786_", paste(landmark_filter, collapse = "_"), ".rds", sep = "")
Filename<- paste("/Users/benjaminmakhlouf/Research_repos/Western_Ak_otolith_stock_discrimination/Models/svm_models/", Modelname, sep = "")
saveRDS(model, Filename)

# Save the trained KNN Model
Modelname<- paste("KNN_Sr8786_", paste(landmark_filter, collapse = "_"), ".rds", sep = "")
Filename<- paste("/Users/benjaminmakhlouf/Research_repos/Western_Ak_otolith_stock_discrimination/Models/knn_models/", Modelname, sep = "")
saveRDS(model, Filename)

## Save as a .csv with a name that includes the landmark filter 
filename <- paste("/Users/benjaminmakhlouf/Research_repos/Western_Ak_otolith_stock_discrimination/Model Results/Sr8786_sr88_MultiModelClass_", paste(landmark_filter, collapse = "_"), ".csv", sep = "")
write.csv(all_results_df, filename, row.names = FALSE)



# Plot with adjusted y-axis scale, muted colors, and facet by both Model_Method and Data_Type
# Define specific colors for each watershed
watershed_colors <- c(
  "Yukon" = "#1f77b4",    # Shade of blue for Yukon
  "Nush" = "#ff7f0e",  # Shade of orange for Nushagak
  "Kusko" = "#2ca02c"  # Shade of green for Kuskokwim
)
# Assuming landmark_filter is already defined in your environment
landmark_title <- paste( paste(landmark_filter, collapse = "_"), ",_sr88_Sr8786")

# Sensitivity (correct identified )
sensitivityplot<- ggplot(all_results_df, aes(x = Watershed, y = Sensitivity, fill = Watershed)) +
  geom_bar(stat = "identity", position = "dodge", alpha = .7) +
  facet_grid(Data_Type ~ Model_Method, scales = "free_y") +  # Facet by both Model_Method and Data_Type
  labs(
    title = landmark_title,  # Dynamic title including landmark filter and Sr8786
    subtitle = "Sensitivity (Recall) ",
    x = "Watershed",
    y = "Sensitivity"
  ) +
  theme_grey() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 12),  # Adjust facet labels size
    strip.background = element_blank()  # Remove background for facet labels
  ) +
  scale_y_continuous(limits = c(0, 1)) +  # Ensure y-axis is between 0 and 1
  scale_fill_manual(values = watershed_colors)+  # Apply specific colors to watersheds
  geom_text(
    aes(label = round(Sensitivity, 2)),    # Add the text labels inside bars, rounding to 2 decimal places
    position = position_dodge(width = 0.8),  # Position the text in the middle of the bars
    vjust = 1.5,                            # Adjust vertical positioning of text
    color = "white",                        # Set the text color to white for visibility
    size = 3                                 # Set the text size
  )


# Specificity (correctly identified non-targets)

specificityplot<-ggplot(all_results_df, aes(x = Watershed, y = Specificity, fill = Watershed)) +
  geom_bar(stat = "identity", position = "dodge", alpha = .7) +
  facet_grid(Data_Type ~ Model_Method, scales = "free_y") +  # Facet by both Model_Method and Data_Type
  labs(
    title = landmark_title,  # Dynamic title including landmark filter and Sr8786
    subtitle = "Specificity (Precision) ",
    x = "Watershed",
    y = "Specificity"
  ) +
  theme_grey() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 12),  # Adjust facet labels size
    strip.background = element_blank()  # Remove background for facet labels
  ) +
  scale_y_continuous(limits = c(0, 1)) +  # Ensure y-axis is between 0 and 1
  scale_fill_manual(values = watershed_colors)+  # Apply specific colors to watersheds
  geom_text(
    aes(label = round(Specificity, 2)),    # Add the text labels inside bars, rounding to 2 decimal places
    position = position_dodge(width = 0.8),  # Position the text in the middle of the bars
    vjust = 1.5,                            # Adjust vertical positioning of text
    color = "white",                        # Set the text color to white for visibility
    size = 3                                 # Set the text size
  )


Balanced_sensitivtyplot<-ggplot(all_results_df, aes(x = Watershed, y = `Balanced Accuracy`, fill = Watershed)) +
  geom_bar(stat = "identity", position = "dodge", alpha = .7) +
  facet_grid(Data_Type ~ Model_Method, scales = "free_y") +  # Facet by both Model_Method and Data_Type
  labs(
    title = landmark_title,  # Dynamic title including landmark filter and Sr8786
    subtitle = "Balanced Accuracy ",
    x = "Watershed",
    y = "Balanced Accuracy"
  ) +
  theme_grey() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 12),  # Adjust facet labels size
    strip.background = element_blank()  # Remove background for facet labels
  ) +
  scale_y_continuous(limits = c(0, 1)) +  # Ensure y-axis is between 0 and 1
  scale_fill_manual(values = watershed_colors)+  # Apply specific colors to watersheds
  geom_text(
    aes(label = round(all_results_df$`Balanced Accuracy`, 2)),    # Add the text labels inside bars, rounding to 2 decimal places
    position = position_dodge(width = 0.8),  # Position the text in the middle of the bars
    vjust = 1.5,                            # Adjust vertical positioning of text
    color = "white",                        # Set the text color to white for visibility
    size = 3                                 # Set the text size
  )


ggplot(all_results_df, aes(x = Watershed, y = Overall_Accuracy, fill = Watershed)) +
  geom_bar(stat = "identity", position = "dodge", alpha = .7) +
  facet_grid(Data_Type ~ Model_Method, scales = "free_y") +  # Facet by both Model_Method and Data_Type
  labs(
    title = landmark_title,  # Dynamic title including landmark filter and Sr8786
    subtitle = "Balanced Accuracy ",
    x = "Watershed",
    y = "Balanced Accuracy"
  ) +
  theme_grey() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 12),  # Adjust facet labels size
    strip.background = element_blank()  # Remove background for facet labels
  ) +
  scale_y_continuous(limits = c(0, 1)) +  # Ensure y-axis is between 0 and 1
  scale_fill_manual(values = watershed_colors)+  # Apply specific colors to watersheds
  geom_text(
    aes(label = round(all_results_df$Overall_Accuracy, 2)),    # Add the text labels inside bars, rounding to 2 decimal places
    position = position_dodge(width = 0.8),  # Position the text in the middle of the bars
    vjust = 1.5,                            # Adjust vertical positioning of text
    color = "white",                        # Set the text color to white for visibility
    size = 3                                 # Set the text size
  )


# Create filenames in the Figures directory
sensitivity_filename <- paste0("Figures/ModelOutputs/", landmark_title, "_Sensitivity.png")
specificity_filename <- paste0("Figures/ModelOutputs/", landmark_title, "_Specificity.png")
Balanced_sensitivity_filename <- paste0("Figures/ModelOutputs/", landmark_title, "_Balanced_Accuracy.png")

# Save the plots
ggsave(sensitivity_filename, sensitivityplot, width = 12, height = 8, dpi = 300)
ggsave(specificity_filename, specificityplot, width = 12, height = 8, dpi = 300)
ggsave(Balanced_sensitivity_filename, Balanced_sensitivtyplot, width = 12, height = 8, dpi = 300)






