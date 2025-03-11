# Install necessary packages
install.packages("tidymodels")
install.packages("discrim")
install.packages("probably")

# Load libraries
library(tidymodels)
library(probably)
library(discrim)
library(dplyr)
library(caret)

# Load data
processed_data <- read.csv("/Users/benjaminmakhlouf/Research_repos/Western_Ak_otolith_stock_discrimination/Data/Processed/Preprocessed_ts_matrices/Processed_Core_Fw_GAM.csv")
All_Metadata <- read.csv(here::here("Data/Final/Metadata_and_QC.csv"))

# Merge metadata
AnalysisDataAll <- processed_data %>%
  left_join(All_Metadata %>% select(-Year), by = c("Fish_id" = "Fish_ID")) %>%
  select((ncol(.)-12):ncol(.), everything()) %>%
  filter(QC_Grade == "Yes")


# Separate metadata and time series data
Analysis_metadata <- AnalysisDataAll[, 1:17]
Analysis_ts_data <- AnalysisDataAll[, 18:ncol(AnalysisDataAll)]

# Prepare model data
ModelData <- Analysis_ts_data %>%
  as.data.frame() %>%
  mutate(Watershed = Analysis_metadata$Watershed)

# Ensure Watershed is a factor
ModelData$Watershed <- as.factor(ModelData$Watershed)

# Split data into training and testing
set.seed(123)

# Ensure at most 500 samples per watershed
traindata <- ModelData %>%
  group_by(Watershed) %>%
  sample_n(min(500, n())) %>%
  ungroup()

# Create test dataset from remaining data
testdata <- anti_join(ModelData, traindata, by = colnames(ModelData))

# Ensure factor consistency
testdata$Watershed <- factor(testdata$Watershed, levels = levels(traindata$Watershed))
traindata$Watershed <- factor(traindata$Watershed, levels = levels(traindata$Watershed))

# Are there NAs in traindata?
sum(is.na(traindata))

### Train the model using tidymodels

library(tidymodels)

rf_fit<- rand_forest() %>%
  set_mode("classification") %>%
  fit(Watershed ~ ., data = traindata)
  
rf_fit

# Predict probabilities
cal_pred<- 
  predict(rf_fit, testdata, type = "prob") %>%
  bind_cols(testdata)


cal_plot_windowed(cal_pred, truth = Watershed, window_size = 0.1, step_size = 0.03)

# Calibrate probabilities
smoothed_mn <- cal_estimate_multinomial(cal_pred, truth = Watershed)

new_test_pred <- cal_apply(cal_pred, smoothed_mn)

cal_plot_windowed(new_test_pred, truth = Watershed, window_size = 0.1, step_size = 0.03)







# Train model
control <- trainControl(method = "cv", number = 5, classProbs = TRUE)

model <- train(Watershed ~ ., data = traindata, method = "rf", trControl = control)





# Predict probabilities
probabilities <- predict(model, testdata, type = "prob")

# Add true and predicted labels
probabilities <- probabilities %>%
  mutate(Truth = testdata$Watershed, Predicted = predict(model, testdata))

# Calibrate probabilities
class_prob_cols <- colnames(probabilities)[!colnames(probabilities) %in% c("Truth", "Predicted")]

calibrated_probabilities <- cal_estimate_multinomial(
  .data = probabilities,
  truth = Truth,
  estimate = all_of(class_prob_cols),
  smooth = TRUE
)

#view results 
calibrated_probabilities
