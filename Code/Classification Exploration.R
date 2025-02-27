library(viridis)
library(patchwork)
library(plotly)
library(tidyverse)
library(here)
library(caret)

######################

source(here("/Users/benjaminmakhlouf/Research_repos/Western_Ak_otolith_stock_discrimination/Code/Helper Code/Raw_Data_Preprocessing.R")) ### This script contains the function which preprocesses all of the raw data into a form that can be used for PCA/ML/Etc.  
source(here("/Users/benjaminmakhlouf/Research_repos/Western_Ak_otolith_stock_discrimination/Code/Helper Code/PCA_functions.R"))### This script contains helper functions to run PCA and a few important figures
QC_data<- read.csv(here("Data/qc_results.csv"))

########### Read in the most recent processed data 
################################################################################

iso_data_raw<- read.csv(here("Data/Processed/all_data_combined_RAW.csv"))
iso_data_MA<- read.csv(here("Data/Processed/all_data_combined_MA.csv"))
iso_data_GAM<- read.csv(here("Data/Processed/all_data_combined_GAM.csv"))

#############################
#### SELECT WHICH DATA TO RUN 
#############################
AnalysisDataAll<- iso_data_raw 
Analysis_metadata<- AnalysisDataAll[,1:5] #Seperate Metadata 
Analysis_ts_data<- AnalysisDataAll[,6:ncol(AnalysisDataAll)] #Seperate Isotope ts data 


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
combined_plot <- ( scree_plot_1 / feature_figure) | natalIsoPCAPlot
print(combined_plot)

# ####
# # 3D plot 
# plot_ly(
#   x = PCA_full$PC1, 
#   y = PCA_full$PC2,, 
#   z = PCA_full$PC3, 
#   type = "scatter3d", 
#   mode = "markers", 
#   marker = list(size = 3),  # Adjust size here
#   color = PCA_full$Watershed
# )
}

###############################
##############################
### RF 

# Ensure selected_data is a dataframe and add Watershed
ModelData <- selected_data %>% as.data.frame() %>% mutate(Watershed = selected_metadata$Watershed)

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






