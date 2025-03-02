library(viridis)
library(patchwork)
library(plotly)
library(tidyverse)
library(here)
library(caret)
library(shiny)

source(here("/Users/benjaminmakhlouf/Research_repos/Western_Ak_otolith_stock_discrimination/Code/Helper Code/PCA_functions.R"))
source(here("/Users/benjaminmakhlouf/Research_repos/Western_Ak_otolith_stock_discrimination/Code/Helper Code/Raw_Data_Preprocessing.R"))

All_Metadata <- read.csv(here("Data/Final/Metadata_and_QC.csv"))

# Define landmark filters to compare
landmark_filters <- list("Core", "Fw", c("Core", "Fw"))
results <- list()

for (landmark_filter in landmark_filters) {
  data_type <- "RAW"  # Keep consistent across all analyses
  data_identifier <- paste(data_type, paste(landmark_filter, collapse = "_"), sep = "_")
  
  # Load processed data for the given landmark filter
  processed_data <- load_processed_data(data_type, landmark_filter)
  
  # Merge metadata
  AnalysisDataAll <- processed_data %>%
    left_join(All_Metadata %>% select(-Year), by = c("Fish_id" = "Fish_ID")) %>%
    select((ncol(.) - 12):ncol(.), everything())
  
  # Separate metadata and time series data
  Analysis_metadata <- AnalysisDataAll[, 1:17]
  Analysis_ts_data <- AnalysisDataAll[, 18:ncol(AnalysisDataAll)]
  
  ##### PCA #####
  PCA_raw <- prcomp(Analysis_ts_data, scale. = TRUE)
  PCA_full <- run_pca(Analysis_ts_data, Analysis_metadata)
  
  # Generate PCA plots
  natalIsoPCAPlot <- pca_plot(PCA_full, 1, 2)
  feature_figure <- plot_pca_loadings(PCA_raw, plot_type = "line")
  scree_plot_1 <- scree_plot(PCA_full)
  
  # Combine plots
  combined_plot <- natalIsoPCAPlot / (scree_plot_1 | feature_figure)
  
  # Save PCA plot
  ggsave(here(paste0("Figures/PCA_", data_identifier, ".pdf")), plot = combined_plot, width = 30, height = 30, units = "in", dpi = 300)
  
  ##### Random Forest Classification #####
  ModelData <- Analysis_ts_data %>%
    as.data.frame() %>%
    mutate(Watershed = Analysis_metadata$Watershed)
  
  # Split data
  set.seed(123)
  trainIndex <- createDataPartition(ModelData$Watershed, p = 0.8, list = FALSE)
  traindata <- ModelData[trainIndex, ]
  testdata <- ModelData[-trainIndex, ]
  
  # Train Random Forest model
  control <- trainControl(method = "cv", number = 5, classProbs = TRUE)
  model <- train(Watershed ~ ., data = traindata, method = "rf", trControl = control)
  
  # Make predictions
  predictions <- predict(model, testdata)
  probabilities <- predict(model, testdata, type = "prob")
  
  # Classification results
  idScores <- Analysis_metadata[-trainIndex, ] %>%
    select(Fish_id) %>%
    mutate(
      Predicted = predictions,
      Actual = testdata$Watershed,
      Confidence = apply(probabilities, 1, max),
      Correct = Predicted == Actual
    ) %>%
    bind_cols(probabilities)
  
  idScores <- idScores %>%
    mutate(Predicted = as.factor(Predicted), Actual = as.factor(Actual))
  
  # Compute confusion matrix
  conf_matrix <- confusionMatrix(idScores$Predicted, idScores$Actual)
  
  # Store results
  results[[data_identifier]] <- list(
    pca_plot = combined_plot,
    confusion_matrix = conf_matrix,
    id_scores = idScores
  )
}

# Print results for comparison
for (id in names(results)) {
  cat("\n### Results for", id, "###\n")
  print(results[[id]]$confusion_matrix)
}
