library(viridis)
library(patchwork)
library(plotly)
library(tidyverse)
library(here)
library(caret)
library(shiny)

######################

source(here("/Users/benjaminmakhlouf/Research_repos/Western_Ak_otolith_stock_discrimination/Code/Helper Code/PCA_functions.R"))### This script contains helper functions to run PCA and a few important figures
All_Metadata<- read.csv(here("Data/Final/Metadata_and_QC.csv"))

########### Read in the most recent processed data 
################################################################################

iso_data_raw<- read.csv(here("Data/Processed/all_data_combined_RAW.csv"))
iso_data_MA<- read.csv(here("Data/Processed/all_data_combined_MA.csv"))
iso_data_GAM<- read.csv(here("Data/Processed/all_data_combined_GAM.csv"))

# For individuals in iso_data_raw. If Watershed == Yukon, change to Yukon_Low, Yukon, Middle, and Yukon, High based on the associated likely_gen in All_Metadata based on matching Fish_ID and Fish_id 
# Merge iso_data_raw with All_Metadata based on Fish_id and Fish_ID
merged_data <- iso_data_raw %>%
  left_join(All_Metadata, by = c("Fish_id" = "Fish_ID"))

# Update the Watershed column based on the likely_gen column
updated_data <- merged_data %>%
  mutate(Watershed = case_when(
    Watershed == "Yukon" & likely_gen == "Lower_gen" ~ "Yukon_Low",
    Watershed == "Yukon" & likely_gen == "Middle_gen" ~ "Yukon_Middle",
    Watershed == "Yukon" & likely_gen == "Upper_gen" ~ "Yukon_High",
    TRUE ~ Watershed  # Keep the original value if no condition is met
  ))

# Select only the relevant columns to match the original iso_data_raw structure
iso_data_raw <- updated_data %>%
  select(1:length(iso_data_raw))  # Select the same number of columns as iso_data_raw

# Rename year.x to year again 
iso_data_raw <- iso_data_raw %>%
  rename(Year = Year.x)



#############################
#### SELECT WHICH DATA TO RUN 
#############################
AnalysisDataAll<- iso_data_raw 
Analysis_metadata<- AnalysisDataAll[,1:5] #Seperate Metadata 
Analysis_ts_data<- AnalysisDataAll[,6:ncol(AnalysisDataAll)] #Seperate Isotope ts data 








# # Find indices in Analysis_metadata where Watershed == "Yukon"
# yukon_indices <- which(Analysis_metadata$Watershed == "Yukon" | Analysis_metadata$Watershed == "Nush" | Analysis_metadata$Watershed == "Kusko" ) 
# 
# # Delete from Analysis_metadata and Analysis_ts_data
# Analysis_metadata <- Analysis_metadata[-yukon_indices, ]
# Analysis_ts_data <- Analysis_ts_data[-yukon_indices, ]
# 


if (T){
  #############################
  #############################
  ##### PCA 
  #############################
  
  PCA_raw <- prcomp(Analysis_ts_data, scale. = TRUE) #run the pca 
  PCA_full<- run_pca(Analysis_ts_data, Analysis_metadata) #add all the metadata
  
  #### PLOTS 
  # Plot of Iso and Natal Origin
  natalIsoPCAPlot<-pca_plot(PCA_full,1,2) 
  feature_figure<- plot_pca_loadings(PCA_raw, plot_type = "line")
  scree_plot_1<-scree_plot(PCA_full)
  
  ### Arrange natalIsoPCAPlot and feature_figure in a grid
  combined_plot <- natalIsoPCAPlot / (scree_plot_1 | feature_figure)
  print(combined_plot)
  
  # save as PCA dashboard 
  ggsave(here("Figures/PCA_dashboard.pdf"), plot = combined_plot, width = 30, height = 30, units = "in", dpi = 300)
  
  # ####
  #3D plot
  plot_ly(
    x = PCA_full$PC1,
    y = PCA_full$PC2,
    z = PCA_full$PC3,
    type = "scatter3d",
    mode = "markers",
    marker = list(
      size = 1,  # Adjust size
      opacity = 0.5  # Adjust transparency (0 = fully transparent, 1 = fully opaque)
    ),
    color = PCA_full$Watershed
  )
}

###############################
##############################


### RF 

# Ensure selected_data is a dataframe and add Watershed
ModelData <- Analysis_ts_data %>% as.data.frame() %>% mutate(Watershed = Analysis_metadata$Watershed)

# Split data into training (80%) and testing (20%)
set.seed(123)
trainIndex <- createDataPartition(ModelData$Watershed, p = 0.8, list = FALSE)
traindata <- ModelData[trainIndex, ]
testdata <- ModelData[-trainIndex, ]

# Set up cross-validation
control <- trainControl(method = "cv", number = 5, classProbs = TRUE)  

# Train Random Forest model
set.seed(123)

model <- train(Watershed ~ ., data = traindata, method = "rf", trControl = control)

# Make predictions (both class labels and probabilities)
predictions <- predict(model, testdata)
probabilities <- predict(model, testdata, type = "prob")

# Extract IDs for test samples
idScores <- Analysis_metadata[-trainIndex,] %>%
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

# View results
print(conf_matrix)

# Convert the confusion matrix to a tidy format
conf_matrix_df <- as.data.frame(conf_matrix$table)

# Plot the heatmap
Heatmap<-ggplot(conf_matrix_df, aes(x = Prediction, y = Reference, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "grey20", size = 6) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  theme_minimal() +
  labs(title = "Confusion Matrix Heatmap", x = "Predicted Label", y = "Actual Label")

## Confidence Scores 
confidence_scores<-ggplot(idScores, aes(x = Confidence, fill = Correct)) +
  geom_density(alpha = 0.6) +
  scale_fill_manual(values = c("red", "green")) +
  theme_minimal() +
  labs(title = "Confidence Score Distribution", x = "Confidence Score", y = "Density")

# Plot correct vs incorrect results by proportion of year 

# Add metadata to idScored by Fish_ID 
idScores <- left_join(idScores, All_Metadata, by = c("Fish_id" = "Fish_ID"))

# Plot the proportion of year for incorrect vs correctly identified as a stacked bar plot 
year_proportion <- idScores %>%
  group_by(Year, Actual, Correct) %>%
  summarise(Count = n(), .groups = "drop") %>%  # Count occurrences
  group_by(Year, Actual) %>%  # Group again by Year and Actual only
  mutate(Proportion = Count / sum(Count)) %>%  # Compute proportion within each group
  ggplot(aes(x = Year, y = Proportion, fill = Correct)) +
  geom_bar(stat = "identity", position = "stack", alpha = 0.8) +  # Adjust transparency
  scale_fill_manual(values = c("firebrick", "seagreen")) +
  facet_wrap(~Actual, ncol = 1, scales = "free_y") +  # Display in one column
  theme_grey() +
  labs(
    title = "Year and Watershed",
    x = "Year",
    y = "Proportion"
  )

# Correctly vs incorrect by QC_score 

qc_proportion <- idScores %>%
  filter(!QC_Grade %in% c("Good", NA)) %>%  # Exclude "Good" and NA values
  group_by(QC_Grade, Correct) %>%  # Group by QC_Grade and Correct first
  summarise(Count = n(), .groups = "drop") %>%  # Count occurrences
  group_by(QC_Grade) %>%  # Group again by QC_Grade only
  mutate(Proportion = Count / sum(Count)) %>%  # Compute proportion within QC_Grade
  ggplot(aes(x = QC_Grade, y = Proportion, fill = Correct)) +
  geom_bar(stat = "identity", position = "stack", alpha = 0.8) +  # Ensure stacking
  scale_fill_manual(values = c("firebrick", "seagreen")) +
  theme_grey() +
  labs(
    title = "QC_Grade",
    x = "QC_Grade",
    y = "Proportion"
  )

## Same thing but by Core_status

core_proportion <- idScores %>%
  filter(!Core_Status %in% c("Good", NA)) %>%  # Exclude "Good" and NA values
  group_by(Core_Status, Correct) %>%  # Group by Core_status and Correct first
  summarise(Count = n(), .groups = "drop") %>%  # Count occurrences
  group_by(Core_Status) %>%  # Group again by Core_status only
  mutate(Proportion = Count / sum(Count)) %>%  # Compute proportion within Core_status
  ggplot(aes(x = Core_Status, y = Proportion, fill = Correct)) +
  geom_bar(stat = "identity", position = "stack", alpha = 0.8) +  # Ensure stacking
  scale_fill_manual(values = c("firebrick", "seagreen")) +
  theme_grey() +
  labs(
    title = "Core_status",
    x = "Core_status",
    y = "Proportion"
  )


# Filter idScores to be only "Actual" == Yukon 
yukon_idScores <- idScores %>%
  filter(Actual == "Yukon")

# Plot the proportion of correct vs incorrecy by "gen_Likely" 

gen_proportion <- yukon_idScores %>%
  group_by(likely_gen, Correct) %>%  # Group by likely_gen and Correct first
  summarise(Count = n(), .groups = "drop") %>%  # Count occurrences
  group_by(likely_gen) %>%  # Group again by likely_gen only
  mutate(Proportion = Count / sum(Count)) %>%  # Compute proportion within each likely_gen
  ggplot(aes(x = likely_gen, y = Proportion, fill = Correct)) +
  geom_bar(stat = "identity", position = "stack", alpha = 0.8) +  # Adjust transparency
  scale_fill_manual(values = c("firebrick", "seagreen")) +
  theme_grey() +
  labs(
    title = "Genetic Groups",
    x = "Gen_Likely",
    y = "Proportion"
  )

# Arrange the plots in one big figure using patchwork
final_plot <- (Heatmap + confidence_scores) / 
  (year_proportion + qc_proportion) / 
  (core_proportion + gen_proportion)

ggsave("figures/RF_Dashboard.pdf", plot = final_plot, width = 30, height = 30, units = "cm")
