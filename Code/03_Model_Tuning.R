# 03_model_tuning.R
# Calibrates model probabilities to ensure reliable probability estimates
# Focuses only on accuracy metrics

library(tidyverse)
library(tidymodels)
library(probably)
library(here)
library(ggplot2)
library(gridExtra)
library(cowplot)

# best model is random forest combined 

# Read in the best model 
ts_models<- readRDS("/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/data/models/ts_models.rds")
combined_rf<- ts_models$Combined_rf

### Read in the trian and test data 
splitdata<- readRDS("/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/data/ts_split_data.rds")

test_data <- splitdata$Combined$test
train_data <- splitdata$Combined$train

# Get the probabilities from the model 
predictions <- predict(combined_rf, test_data, type = "prob") %>%
  bind_cols(test_data) %>%
  select(.pred_Kusko,.pred_Yukon,.pred_Nush,Watershed)

# make watershed a factor 
predictions$Watershed <- as.factor(predictions$Watershed)

# Get a quick plot of what these class tunings look like before tuning
beforecal<-cal_plot_windowed(predictions, truth = Watershed, window_size = 0.2, step_size = 0.02)

# tune 
smoothed_mn <- cal_estimate_multinomial(predictions, truth = Watershed)

# Now apply back to the training set 
calibrated_predictions <- cal_apply(predictions, smoothed_mn)

# visualize how things have changed 
aftercal<-cal_plot_windowed(calibrated_predictions, truth = Watershed, window_size = 0.2, step_size = 0.03)

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


# Resample 

metrics<- calibrated_predictions %>%
  rsample::vfold_cv() %>%
  cal_validate_multinomial(Watershed)%>%
  collect_metrics()

metrics


