# 03_model_tuning.R
# Calibrates model probabilities for Yukon class only
library(tidyverse)
library(tidymodels)
library(probably)
library(here)
library(ggplot2)
library(gridExtra)
library(cowplot)

# Read in the best model 
ts_models <- readRDS("/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/data/models/ts_models.rds")
combined_rf <- ts_models$Combined_rf

# Read in the train and test data 
splitdata <- readRDS("/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/data/ts_split_data.rds")
test_data <- splitdata$Combined$test
train_data <- splitdata$Combined$train

# Get the probabilities from the model 
predictions <- predict(combined_rf, test_data, type = "prob") %>%
  bind_cols(test_data) %>%
  select(.pred_Kusko, .pred_Yukon, .pred_Nush, Watershed)

# Make watershed a factor 
predictions$Watershed <- as.factor(predictions$Watershed)

# Get a quick plot of what these class tunings look like before tuning
beforecal <- cal_plot_windowed(predictions, truth = Watershed, window_size = 0.3, step_size = 0.05)

# ---------------------------------------------------------
# APPROACH: Manual Isotonic Calibration for Yukon only
# ---------------------------------------------------------

# Create a data frame specifically for Yukon calibration
yukon_data <- predictions %>%
  mutate(Target = ifelse(Watershed == "Yukon", 1, 0))

# Sort by predicted Yukon probability
yukon_sorted <- yukon_data %>%
  arrange(.pred_Yukon)

# We'll use a simple isotonic calibration approach
# First, let's create bins of predictions
num_bins <- 10
bin_size <- ceiling(nrow(yukon_sorted) / num_bins)

# Create bins and calculate actual frequency in each bin
yukon_bins <- yukon_sorted %>%
  mutate(bin = ceiling(row_number() / bin_size)) %>%
  group_by(bin) %>%
  summarize(
    min_prob = min(.pred_Yukon),
    max_prob = max(.pred_Yukon),
    avg_prob = mean(.pred_Yukon),
    actual_freq = mean(Target),
    count = n()
  )

# Now let's create a mapping function from old probabilities to calibrated ones
# We'll use a simple piecewise function based on the bins
calibrate_yukon <- function(prob) {
  # Find the bin this probability falls into
  for (i in 1:nrow(yukon_bins)) {
    if (prob >= yukon_bins$min_prob[i] && prob <= yukon_bins$max_prob[i]) {
      return(yukon_bins$actual_freq[i])
    }
  }
  # If not found in any bin (shouldn't happen, but just in case)
  if (prob < yukon_bins$min_prob[1]) return(yukon_bins$actual_freq[1])
  if (prob > yukon_bins$max_prob[nrow(yukon_bins)]) return(yukon_bins$actual_freq[nrow(yukon_bins)])
}

# Apply calibration to all Yukon probabilities
calibrated_predictions <- predictions %>%
  mutate(
    # Store original probabilities
    orig_yukon = .pred_Yukon,
    # Apply calibration to Yukon class only
    .pred_Yukon = sapply(.pred_Yukon, calibrate_yukon),
    # Calculate the difference
    prob_diff = .pred_Yukon - orig_yukon,
    # Adjust other probabilities proportionally to maintain sum = 1
    kusko_prop = ifelse((.pred_Kusko + .pred_Nush) > 0, 
                        .pred_Kusko / (.pred_Kusko + .pred_Nush), 0),
    nush_prop = ifelse((.pred_Kusko + .pred_Nush) > 0, 
                       .pred_Nush / (.pred_Kusko + .pred_Nush), 0),
    .pred_Kusko = .pred_Kusko - (prob_diff * kusko_prop),
    .pred_Nush = .pred_Nush - (prob_diff * nush_prop)
  ) %>%
  # Remove helper columns
  select(-.pred_Kusko, -.pred_Yukon, -.pred_Nush, -orig_yukon, -prob_diff, -kusko_prop, -nush_prop, everything()) %>%
  select(.pred_Kusko, .pred_Yukon, .pred_Nush, Watershed)

# Save calibration bins for future use
saveRDS(yukon_bins, "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/data/models/yukon_calibration_bins.rds")

# Save calibrated predictions
saveRDS(calibrated_predictions, "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/data/models/yukon_calibrated_predictions.rds")

# visualize how things have changed 
aftercal <- cal_plot_windowed(calibrated_predictions, truth = Watershed, window_size = 0.2, step_size = 0.03)

# Plot both plots one on top of another using cowplot
beforecal <- beforecal + ggtitle("Before Calibration")
aftercal <- aftercal + ggtitle("After Calibration (Yukon Only)")

# Combine the plots
plot_grid(beforecal, aftercal, ncol = 1) +
  theme(legend.position = "none") + 
  ggtitle("Before and After Yukon-Only Calibration") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(axis.text.y = element_text(size = 8)) +
  theme(axis.title = element_text(size = 10)) +
  theme(legend.text = element_text(size = 8)) +
  theme(legend.title = element_text(size = 10))

# Save the plot
ggsave("/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/figures/yukon_calibration_comparison.png", width = 10, height = 8)

# Evaluate metrics with cross-validation
metrics <- calibrated_predictions %>%
  rsample::vfold_cv() %>%
  cal_validate_multinomial(Watershed) %>%
  collect_metrics()

# Display metrics
metrics

# Function to apply calibration to new data
apply_yukon_calibration <- function(new_data) {
  # Load the model
  if(!exists("combined_rf")) {
    combined_rf <- readRDS("/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/data/models/ts_models.rds")$Combined_rf
  }
  
  # Load the yukon calibration bins
  if(!exists("yukon_bins")) {
    yukon_bins <- readRDS("/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/data/models/yukon_calibration_bins.rds")
  }
  
  # Define calibration function using the bins
  calibrate_yukon <- function(prob) {
    for (i in 1:nrow(yukon_bins)) {
      if (prob >= yukon_bins$min_prob[i] && prob <= yukon_bins$max_prob[i]) {
        return(yukon_bins$actual_freq[i])
      }
    }
    if (prob < yukon_bins$min_prob[1]) return(yukon_bins$actual_freq[1])
    if (prob > yukon_bins$max_prob[nrow(yukon_bins)]) return(yukon_bins$actual_freq[nrow(yukon_bins)])
  }
  
  # Generate predictions
  preds <- predict(combined_rf, new_data, type = "prob")
  
  # Calibrate Yukon probabilities
  calibrated_preds <- preds %>%
    mutate(
      orig_yukon = .pred_Yukon,
      .pred_Yukon = sapply(.pred_Yukon, calibrate_yukon),
      prob_diff = .pred_Yukon - orig_yukon,
      kusko_prop = ifelse((.pred_Kusko + .pred_Nush) > 0, 
                          .pred_Kusko / (.pred_Kusko + .pred_Nush), 0),
      nush_prop = ifelse((.pred_Kusko + .pred_Nush) > 0, 
                         .pred_Nush / (.pred_Kusko + .pred_Nush), 0),
      .pred_Kusko = .pred_Kusko - (prob_diff * kusko_prop),
      .pred_Nush = .pred_Nush - (prob_diff * nush_prop)
    ) %>%
    select(-.pred_Kusko, -.pred_Yukon, -.pred_Nush, -orig_yukon, -prob_diff, -kusko_prop, -nush_prop, everything()) %>%
    select(.pred_Kusko, .pred_Yukon, .pred_Nush)
  
  return(calibrated_preds)
}

# Save the function for future use
saveRDS(apply_yukon_calibration, "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/data/models/apply_yukon_calibration.rds")

# Example of using the function on full dataset
if(file.exists("/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/data/full_dataset.rds")) {
  full_data <- readRDS("/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/data/full_dataset.rds")
  
  # Apply calibrated predictions to full dataset
  full_predictions <- apply_yukon_calibration(full_data)
  
  # Combine with original data
  full_results <- bind_cols(full_data, full_predictions)
  
  # Save results
  saveRDS(full_results, "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/results/full_dataset_yukon_calibrated.rds")
}



############ for now, we've decided the original data is calibrated. Save this as calibrated model for ts 
# Save the calibrated model
saveRDS(combined_rf, "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/data/models/ts_model_calibrated.rds")
