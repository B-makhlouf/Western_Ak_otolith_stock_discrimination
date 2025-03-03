############### This is a trial script so that model results can be saved to be loaded into a shiny app when need be 

# Script to train and save the Random Forest model
library(tidyverse)
library(caret)
library(here)

# Load data
All_Metadata <- read.csv(here("Data/Final/Metadata_and_QC.csv"))
processed_data <- load_processed_data("RAW", c("Core", "Fw"))

# Merge and prepare data
AnalysisDataAll <- processed_data %>%
  left_join(All_Metadata %>% select(-Year), by = c("Fish_id" = "Fish_ID")) %>%
  select((ncol(.)-12):ncol(.), everything())

Analysis_metadata <- AnalysisDataAll[, 1:17]
Analysis_ts_data <- AnalysisDataAll[, 18:length(AnalysisDataAll)]

# Prepare data for modeling
ModelData <- Analysis_ts_data %>% as.data.frame() %>% mutate(Watershed = Analysis_metadata$Watershed)

# Train Random Forest model
set.seed(123)
trainIndex <- createDataPartition(ModelData$Watershed, p = 0.8, list = FALSE)
traindata <- ModelData[trainIndex, ]
testdata <- ModelData[-trainIndex, ]

control <- trainControl(method = "cv", number = 5, classProbs = TRUE)
model <- train(Watershed ~ ., data = traindata, method = "rf", trControl = control)

# Save the model to a file
saveRDS(model, here("Models/rf_models/test.rds"))
