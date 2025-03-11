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
library(cowplot)

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


### Train the model using tidymodels

library(tidymodels)

set.seed(123)  # For reproducibility
cv_folds <- vfold_cv(traindata, v = 5, strata = Watershed) #Ensures stratified sampling based on the target variable

rf_model <- rand_forest() %>%
  set_mode("classification") %>%
  set_engine("ranger")  # Use 'ranger' for efficient random forests

rf_recipe <- recipe(Watershed ~ ., data = traindata) %>%
  step_normalize(all_numeric_predictors()) %>%  # Normalize numeric predictors
  step_dummy(all_nominal_predictors())  

rf_workflow <- workflow() %>%
  add_model(rf_model) %>%
  add_recipe(rf_recipe)

rf_results <- rf_workflow %>%
  fit_resamples(
    resamples = cv_folds,
    metrics = metric_set(accuracy, roc_auc, sens, spec, f_meas),
    control = control_resamples(save_pred = TRUE)
  )

rf_results %>% collect_metrics()

final_rf_fit <- rf_workflow %>% fit(traindata)

cal_pred <- predict(final_rf_fit, testdata, type = "prob") %>%
  bind_cols(testdata)

rf_fit<- rand_forest() %>%
  set_mode("classification") %>%
  fit(Watershed ~ ., data = traindata)
  
# Predict probabilities
cal_pred<- 
  predict(rf_fit, testdata, type = "prob") %>%
  bind_cols(testdata)

conf_mat(rf_predictions, truth = Watershed, estimate = .pred_class)

################################################### TUNE ########################

## First, visualize what the distribution looks like before 
beforecal<-cal_plot_windowed(cal_pred, truth = Watershed, window_size = 0.2, step_size = 0.02)

# Calibrate probabilities on the prediction set (testing data)
smoothed_mn <- cal_estimate_multinomial(cal_pred, truth = Watershed)

# Apply the calibration to the prediction set
new_test_pred <- cal_apply(cal_pred, smoothed_mn)

# Visualize how things have changes
aftercal<-cal_plot_windowed(new_test_pred, truth = Watershed, window_size = 0.2, step_size = 0.03)

# Resample 
metrics<-new_test_pred %>%
  rsample::vfold_cv() %>%
  cal_validate_multinomial(Watershed)%>%
  collect_metrics()

#Plot both plots one on top o another using cowplot
beforecal <- beforecal + ggtitle("Before Calibration")
aftercal <- aftercal + ggtitle("After Calibration")

# Combine the plots
plot_grid(beforecal, aftercal, ncol = 1) +
  theme(legend.position = "none") + 
  ggtitle("Before and After Calibration") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(axis.text.y = element_text(size = 8)) +
  theme(axis.title = element_text(size = 10)) +
  theme(legend.text = element_text(size = 8)) +
  theme(legend.title = element_text(size = 10))

###############################################################################################

#
#cal_validate_multinomial(new_test_pred, truth = Watershed, window_size = .2, step_size = .03)

