library(tidyverse)
library(patchwork)
library(caret)
library(here)

# Load datasets
datasets <- list(
  "Sr8786_Full" = read.csv(here("Data/Preprocessed_ts_matrices/Processed_Core_Fw_GAM.csv")),
  "Sr8786_Core" = read.csv(here("Data/Preprocessed_ts_matrices/Processed_Core_GAM.csv")),
  "Sr8786_Fw" = read.csv(here("Data/Preprocessed_ts_matrices/Processed_Fw_GAM.csv"))
)

# Function to run RF and return metrics
run_rf_model <- function(dataset_name, data) {
  # Prepare data
  data <- data[complete.cases(data), ]
  metadata <- data[, 1:4]
  analysis_ts <- data[, -c(1:4)]
  analysis_ts <- cbind(watershed = metadata$Watershed, analysis_ts)
  
  # Split data
  set.seed(123)
  train_index <- createDataPartition(analysis_ts$watershed, p = 0.8, list = FALSE)
  train_data <- analysis_ts[train_index, ]
  temp_data <- analysis_ts[-train_index, ]
  
  temp_index <- createDataPartition(temp_data$watershed, p = 0.5, list = FALSE)
  test_data <- temp_data[-temp_index, ]
  
  # Train model
  train_control <- trainControl(method = "cv", number = 5, savePredictions = TRUE)
  model <- train(watershed ~ ., data = train_data, method = "rf", trControl = train_control)
  
  # Evaluate
  predictions <- predict(model, test_data)
  predictions <- as.factor(predictions)
  test_data$watershed <- as.factor(test_data$watershed)
  cm <- confusionMatrix(predictions, test_data$watershed)
  
  # Extract metrics
  metrics <- data.frame(
    Class = names(cm$byClass[,"Sensitivity"]),
    Sensitivity = cm$byClass[,"Sensitivity"],
    F1_Score = cm$byClass[,"F1"],
    Precision = cm$byClass[,"Precision"],
    Recall = cm$byClass[,"Recall"],
    Dataset = dataset_name,
    Model = "Random Forest"
  )
  
  # Add overall metrics
  overall <- data.frame(
    Class = "Overall",
    Sensitivity = cm$overall["Accuracy"],
    F1_Score = mean(metrics$F1_Score, na.rm = TRUE),
    Precision = mean(metrics$Precision, na.rm = TRUE),
    Recall = mean(metrics$Recall, na.rm = TRUE),
    Dataset = dataset_name,
    Model = "Random Forest"
  )
  
  rbind(metrics, overall)
}

# Process all datasets
all_results <- map2_dfr(names(datasets), datasets, ~run_rf_model(.x, .y))

# Visualization ----------------------------------------------------------
# Consistent styling from your original script
main_fill_low <- "#5bc0be"
main_fill_high <- "#ba3f1d"
text_color <- "white"
tile_color <- "white"
tile_size <- 0.7
base_text_size <- 10
axis_angle <- 45
axis_hjust <- 1

common_theme <- function(base_size = base_text_size) {
  theme_minimal(base_size = base_size) +
    theme(
      axis.text.x = element_text(angle = axis_angle, hjust = axis_hjust),
      plot.title = element_text(hjust = 0.5, face = "bold", size = rel(1.1)),
      legend.position = "bottom",
      legend.title = element_text(size = rel(0.9)),
      legend.text = element_text(size = rel(0.8)),
      panel.grid = element_blank(),
      plot.margin = margin(5, 5, 5, 5)
    )
}

create_metric_plot <- function(data, metric, title, metric_name) {
  range_vals <- range(data[[metric]], na.rm = TRUE)
  
  ggplot(data, aes(x = Model, y = Dataset, fill = .data[[metric]])) +
    geom_tile(color = tile_color, linewidth = tile_size) +
    geom_text(aes(label = round(.data[[metric]], 2)), 
              color = text_color, size = 3) +
    scale_fill_gradient(
      low = main_fill_low, 
      high = main_fill_high,
      limits = range_vals
    ) +
    labs(
      title = title,
      x = NULL,
      y = NULL,
      fill = metric_name
    ) +
    common_theme() +
    theme(legend.position = "none")
}

# Prepare data
overall_data <- all_results %>% filter(Class == "Overall")
class_data <- all_results %>% filter(Class %in% c("Class: Nush", "Class: Kusko", "Class: Yukon")) 

# Create plots
accuracy_overall <- create_metric_plot(
  overall_data, "Sensitivity", "Overall Accuracy", "Accuracy"
)

f1_overall <- create_metric_plot(
  overall_data, "F1_Score", "Overall F1 Score", "F1 Score"
)

spec_overall <- create_metric_plot(
  overall_data, "Recall", "Overall Specificity", "Specificity"
)

nushagak_accuracy <- create_metric_plot(
  class_data %>% filter(Class == "Class: Nush"),
  "Sensitivity", "Nushagak Accuracy", "Accuracy"
)

kuskokwim_accuracy <- create_metric_plot(
  class_data %>% filter(Class == "Class: Kusko"),
  "Sensitivity", "Kuskokwim Accuracy", "Accuracy"
)

yukon_accuracy <- create_metric_plot(
  class_data %>% filter(Class == "Class: Yukon"),
  "Sensitivity", "Yukon Accuracy", "Accuracy"
)

# Combine panels
six_panel_figure <- (
  (accuracy_overall + f1_overall + spec_overall) /
    (nushagak_accuracy + kuskokwim_accuracy + yukon_accuracy)
) +
  plot_layout(guides = "collect") &
  theme(legend.position = "none")

# Add title and save
six_panel_figure <- six_panel_figure +
  plot_annotation(
    title = "Random Forest Performance: Full vs Segmented Data",
    subtitle = "Comparison between full transect, core-only, and freshwater-only segments",
    theme = theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold", margin = margin(b = 5)),
      plot.subtitle = element_text(hjust = 0.5, size = 12, margin = margin(b = 10))
    )
  )

# Display and save
six_panel_figure
ggsave(
  here("Results/Model Comparison/RF_segment_comparison.png"),
  six_panel_figure, 
  width = 14, 
  height = 9, 
  dpi = 300
)












