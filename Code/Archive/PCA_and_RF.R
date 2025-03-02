# Load necessary libraries
library(tidyverse)
library(here)
library(shiny)
library(viridis)
library(shiny)
library(shiny)
library(ggplot2)
library(dplyr)
library(zoo)
library(caret)
#install.packages("cowplot")
library(cowplot)

### This script contains the function which preprocesses all of the raw data into a form that can be used for PCA/ML/Etc.  
source(here("/Users/benjaminmakhlouf/Research_repos/Western_Ak_otolith_stock_discrimination/Code/Helper Code/Raw_Data_Preprocessing.R"))
### This script contains helper functions to run PCA and a few important figures
source(here("/Users/benjaminmakhlouf/Research_repos/Western_Ak_otolith_stock_discrimination/Code/Helper Code/PCA_functions.R"))


QC_data<- read.csv(here("Data/qc_results.csv"))





################################################################################
################################################################################
###################### Data Preprocessing ######################################
################################################################################

# Take all of the raw data, 
#interpolate to 1000 (or specified), 
#run a GAM, 
#run a MA, 
#collect metadata
#add to a matrix 
processed_data<-process_trimmed_data(microns_before = 200, microns_after = 300) 

############# Here, all the data has been preprocessed. ########################

# Pull out the metadata
metadata <- tibble(
  Fish_id = processed_data$ids,
  Watershed = processed_data$watersheds,
  Natal_Iso = processed_data$natal_origins,
  Year = processed_data$Year
)

iso_data_raw<- processed_data$measurement_array #RAW interpolated data 
iso_data_MA<- processed_data$moving_avg_array # Moving average data
iso_data_MA <- iso_data_MA[, colSums(is.na(iso_data_MA)) == 0] # MA has tails of NA, remove
iso_data_GAM<- processed_data$gam_smoothed_array # GAM smoothed data


#identifier is the Fish ID without "_trimlocations"
QC_data$Identifier<- gsub("_trimLocations", "", QC_data$Fish_ID)

# list out the Identifiers which are QC Grade "Redo" or "Delete"
redo_delete<- QC_data %>% filter(QC_Grade == "Redo" | QC_Grade == "Delete" ) %>% select(Identifier)

# Find the indices in metadata with Fish_IDs that are in the redo_delete list
redo_delete_indices<- which(metadata$Fish_id %in% redo_delete$Identifier)

# Remove the indices from the metadata and the iso_data
metadata<- metadata[-redo_delete_indices,]
iso_data_raw<- iso_data_raw[-redo_delete_indices,]
iso_data_MA<- iso_data_MA[-redo_delete_indices,]
iso_data_GAM<- iso_data_GAM[-redo_delete_indices,]


all_data_combined_raw<- cbind(metadata, iso_data_raw) # combine the metadata and the raw data
write.csv(all_data_combined_raw, file = here("Data/Processed/all_data_combined_RAW.csv"))

all_data_combined_MA<- cbind(metadata, iso_data_MA) # combine the metadata and the moving average data
write.csv(all_data_combined_MA, file = here("Data/Processed/all_data_combined_MA.csv"))

all_data_combined_GAM<- cbind(metadata, iso_data_GAM) # combine the metadata and the GAM smoothed data
write.csv(all_data_combined_GAM, file = here("Data/Processed/all_data_combined_GAM.csv"))


################################################################################
################################################################################
#If you don't need to change any of the preprocessing paramaters, skip the above
#Read in the data below. 
################################################################################

### READ IN ALL THREE 

iso_data_raw<- read.csv(here("Data/Processed/all_data_combined_RAW.csv"))
iso_data_MA<- read.csv(here("Data/Processed/all_data_combined_MA.csv"))
iso_data_GAM<- read.csv(here("Data/Processed/all_data_combined_GAM.csv"))



iso_data<- iso_data_raw ### Choose which set you want for analysis, call it just "iso_data"



# re-separate iso and metadata 
metadata<- iso_data[,1:5]
iso_data<- iso_data[,6:ncol(iso_data)]

################################################################################
#FILTER

# NO FILTER 
selected_indices <- 1:nrow(metadata)

# ADD FILTER HERE 
#selected_indices <- which((metadata$Natal_Iso >= 0.707 & metadata$Natal_Iso <= 0.7075))

# Filter both
selected_metadata <- metadata[selected_indices,]
selected_data <- iso_data[selected_indices,]
selected_data<- as.matrix(selected_data)

################################################################################
################################################################################
#################### PCA Exploration ###########################################
################################################################################

PCA_raw <- prcomp(selected_data, scale. = TRUE) #run the pca 
PCA_full<- run_pca(selected_data, selected_metadata) #add all the metadata

#### PCA plot with natal origin colored PCA plot 
### Changing the numbers in the function will change the axes
natalIsoPCAPlot<-pca_natal_plot(PCA_full,1,2)
print(natalIsoPCAPlot)

library(plotly)
# 3d PCA plot with watershed colored using plotly
plot_ly(
  x = PCA_full$PC2, 
  y = PCA_full$PC3,, 
  z = PCA_full$PC4, 
  type = "scatter3d", 
  mode = "markers", 
  marker = list(size = 3),  # Adjust size here
  color = PCA_full$Watershed
)

### Feature importance visualized along the timeseries 
## Changing the plot type will change the visualization (options are "line" or "bar")
feature_figure<- plot_pca_loadings(PCA_raw, plot_type = "line")
plot(feature_figure)

################################################################################
################################################################################
################# Machine Learning Classifier ##################################
################################################################################

# Add "Watershed" back into selected_data and create ModelData
ModelData <- as.data.frame(selected_data)  # Convert matrix to data frame
ModelData$Watershed <- selected_metadata$Watershed  # Add Watershed column

# Split the data into training (80%) and testing (20%) sets
set.seed(123)
trainIndex <- createDataPartition(ModelData$Watershed, p = 0.8, list = FALSE)
traindata <- ModelData[trainIndex, ]
testdata <- ModelData[-trainIndex, ]

# Set up cross-validation parameters
control <- trainControl(method = "cv", number = 5, classProbs = TRUE)  # Enable class probabilities

# Train the Random Forest model
set.seed(123)
model <- train(Watershed ~ ., data = traindata, method = "rf", trControl = control)

# Make predictions on the test set (both class labels and probabilities)
predictions <- predict(model, testdata)
probabilities <- predict(model, testdata, type = "prob")

# Create a df with probabilities and ID 
ids<- metadata[-trainIndex,]
ids<- ids$Fish_id
idScores<- data.frame(ids, probabilities)

# Compute confidence scores (highest probability for each prediction)
confidence_scores <- apply(probabilities, 1, max)

# Create results dataframe with predictions, actual values, and confidence scores
results <- data.frame(
  Predicted = predictions,
  Actual = testdata$Watershed,
  Confidence = confidence_scores
)

idScores$Predicted<- results$Predicted
idScores$Actual<- results$Actual
idScores$Correct<- idScores$Predicted == idScores$Actual

# MAke both factors
results$Predicted <- as.factor(results$Predicted)
results$Actual <- as.factor(results$Actual)

# Compute confusion matrix
conf_matrix <- confusionMatrix(results$Predicted, results$Actual)







# Display results
print(results)  # View first few rows of predictions with confidence
print(conf_matrix)    # View model performance

# Histogram for correctly classified Kusko
plot_correct_Kusko <- ggplot(results[results$Actual == "Kusko" & results$Correct == TRUE,], 
                             aes(x = Confidence)) +
  geom_histogram(fill = "blue", alpha = 0.5, binwidth = 0.05, color = "black") +
  labs(title = "Actually Kusko, classified as Kusko", x = "Score", y = "Frequency") +
  theme_minimal()

# Histogram for incorrectly classified Kusko
plot_incorrect_Kusko <- ggplot(results[results$Actual == "Kusko" & results$Correct == FALSE,], 
                               aes(x = Confidence)) +
  geom_histogram(fill = "red", alpha = 0.5, binwidth = 0.05, color = "black") +
  labs(title = "Actually Kusko, classified as other", x = "Score", y = "Frequency") +
  theme_minimal()

# Histogram for correctly classified Yukon
plot_correct_Yukon <- ggplot(results[results$Actual == "Yukon" & results$Correct == TRUE,], 
                             aes(x = Confidence)) +
  geom_histogram(fill = "blue", alpha = 0.5, binwidth = 0.05, color = "black") +
  labs(title = "Actually Yukon, classified as Yukon", x = "Score", y = "Frequency") +
  theme_minimal()

# Histogram for incorrectly classified Yukon
plot_incorrect_Yukon <- ggplot(results[results$Actual == "Yukon" & results$Correct == FALSE,], 
                               aes(x = Confidence)) +
  geom_histogram(fill = "red", alpha = 0.5, binwidth = 0.05, color = "black") +
  labs(title = "Actually Yukon, classified as other", x = "Score", y = "Frequency") +
  theme_minimal()

# Histogram for correctly classified Nushagak
plot_correct_Nushagak <- ggplot(results[results$Actual == "Nush" & results$Correct == TRUE,], 
                                aes(x = Confidence)) +
  geom_histogram(fill = "blue", alpha = 0.5, binwidth = 0.05, color = "black") +
  labs(title = "Actually Nushagak, classified as Nushagak", x = "Score", y = "Frequency") +
  theme_minimal()

# Histogram for incorrectly classified Nushagak
plot_incorrect_Nushagak <- ggplot(results[results$Actual == "Nush" & results$Correct == FALSE,], 
                                  aes(x = Confidence)) +
  geom_histogram(fill = "red", alpha = 0.5, binwidth = 0.05, color = "black") +
  labs(title = "Actually Nushagak, classified as other", x = "Score", y = "Frequency") +
  theme_minimal()

# Combine the plots into one figure using cowplot
final_plot <- plot_grid(
  plot_correct_Kusko, plot_incorrect_Kusko, 
  plot_correct_Yukon, plot_incorrect_Yukon, 
  plot_correct_Nushagak, plot_incorrect_Nushagak, 
  ncol = 2
)

# Display the final combined plot
print(final_plot)


################################## 
############ Converting to probability 




##################### For the Test Dataset #####################
# Step 1: Convert labels to binary ("Kusko" vs. "nonKusko")
results_modified <- results %>%
  mutate(Actual = ifelse(Actual == "Kusko", "Kusko", "nonKusko")) %>%
  filter(Predicted == "Kusko")


# Step 2: Bin the confidence scores into intervals from 0.0 to 1.0 
results_modified$ConfidenceBin <- cut(
  results_modified$Confidence, 
  breaks = seq(0, 1, by = 0.05), 
  include.lowest = TRUE
)

# Step 3: Calculate the number of Kusko vs. nonKusko in each bin for Actual
confidence_table_actual <- table(results_modified$ConfidenceBin, results_modified$Actual)

# Step 4: Convert the table to a data frame
confidence_table_actual_df <- as.data.frame(confidence_table_actual)

# Step 5: Rename columns for clarity
colnames(confidence_table_actual_df) <- c("ConfidenceBin", "Actual", "Count")

# Step 6: Extract the upper bound of each bin
confidence_table_actual_df <- confidence_table_actual_df %>%
  mutate(ConfidenceUpper = as.numeric(sub(".+,(.+)]", "\\1", ConfidenceBin)))

# Step 7: Display the table of actual counts
print(confidence_table_actual_df)

# Step 8: Calculate the proportion of Kusko in each bin
confidence_table_actual_summary <- confidence_table_actual_df %>%
  group_by(ConfidenceUpper) %>%
  summarize(
    TotalCount = sum(Count), # Total number of samples in the bin
    KuskoCount = sum(Count[Actual == "Kusko"]), # Number of "Kusko" in the bin
    KuskoProportion = KuskoCount / TotalCount # Proportion of "Kusko"
  )

# Step 9: Display the summary table
print(confidence_table_actual_summary)

#Make any NAs 0 
confidence_table_actual_summary[is.na(confidence_table_actual_summary)] <- 0


# Step 10: Plot the proportion of Kusko vs. confidence bins for the test set
ggplot(confidence_table_actual_summary, aes(x = ConfidenceUpper, y = KuskoProportion)) +
  geom_point(size = 3, color = "dodgerblue3") +  # Scatterplot points+ 
  geom_smooth(method = "loess", se = FALSE, color = "grey20") +  # Loess smoothing line
  labs(
    title = "Proportion of Actual Kusko vs. Confidence Bins (Test Set)",
    x = "Upper Bound of Confidence Bin",
    y = "Proportion of Actual Kusko"
  ) +
  theme_grey() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12)
  )

filename<- paste("Scores vs. Proportions Kusko.pdf")

# Export to the "Figures" folder 
ggsave(file.path("Figures", filename), width = 8, height = 6, units = "in")


idScores_filtered<- idScores %>% 
  filter(Actual == "Kusko" & Correct == "FALSE") 











##################### For the Training Dataset #####################
# Step 1: Make predictions on the training set (both class labels and probabilities)
train_predictions <- predict(model, traindata)
train_probabilities <- predict(model, traindata, type = "prob")

# Step 2: Compute confidence scores (highest probability for each prediction)
train_confidence_scores <- apply(train_probabilities, 1, max)

# Step 3: Create results dataframe for the training set
train_results <- data.frame(
  Predicted = train_predictions,
  Actual = traindata$Watershed,
  Confidence = train_confidence_scores
)

# Step 4: Convert Predicted and Actual to factors
train_results$Predicted <- as.factor(train_results$Predicted)
train_results$Actual <- as.factor(train_results$Actual)

# Step 5: Convert labels to binary ("Kusko" vs. "nonKusko") for the training set
train_results_modified <- train_results %>%
  mutate(Actual = ifelse(Actual == "Kusko", "Kusko", "nonKusko"))

# Step 6: Bin the confidence scores into intervals from 0.0 to 1.0 by 0.1
train_results_modified$ConfidenceBin <- cut(
  train_results_modified$Confidence, 
  breaks = seq(0, 1, by = 0.1), 
  include.lowest = TRUE
)

# Step 7: Calculate the number of Kusko vs. nonKusko in each bin for Actual
train_confidence_table_actual <- table(train_results_modified$ConfidenceBin, train_results_modified$Actual)

# Step 8: Convert the table to a data frame
train_confidence_table_actual_df <- as.data.frame(train_confidence_table_actual)

# Step 9: Rename columns for clarity
colnames(train_confidence_table_actual_df) <- c("ConfidenceBin", "Actual", "Count")

# Step 10: Extract the upper bound of each bin
train_confidence_table_actual_df <- train_confidence_table_actual_df %>%
  mutate(ConfidenceUpper = as.numeric(sub(".+,(.+)]", "\\1", ConfidenceBin)))

# Step 11: Calculate the proportion of Kusko in each bin for the training set
train_confidence_table_actual_summary <- train_confidence_table_actual_df %>%
  group_by(ConfidenceUpper) %>%
  summarize(
    TotalCount = sum(Count), # Total number of samples in the bin
    KuskoCount = sum(Count[Actual == "Kusko"]), # Number of "Kusko" in the bin
    KuskoProportion = KuskoCount / TotalCount # Proportion of "Kusko"
  )

# Step 12: Display the summary table for the training set
print(train_confidence_table_actual_summary)

# Step 13: Plot the proportion of Kusko vs. confidence bins for the training set
ggplot(train_confidence_table_actual_summary, aes(x = ConfidenceUpper, y = KuskoProportion)) +
  geom_point(size = 3, color = "blue") +  # Scatterplot points
  geom_smooth(method = "loess", se = TRUE, color = "red", linetype = "dashed") +  # Smooth line
  labs(
    title = "Proportion of Actual Kusko vs. Confidence Bins (Training Set)",
    x = "Upper Bound of Confidence Bin",
    y = "Proportion of Actual Kusko"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12)
  )



#####################################################################################



####################### ML LOOP 
# Load necessary libraries
library(tidyverse)
library(here)
library(caret)
library(ggplot2)

################################################################################
# Step 1: Run Classification for All Datasets (RAW, MA, GAM)
################################################################################

# Function to run classification and record results
run_classification <- function(data, metadata, dataset_name) {
  # Add "Watershed" back into the data
  ModelData <- as.data.frame(data)
  ModelData$Watershed <- metadata$Watershed
  
  # Split the data into training and testing sets
  set.seed(123)
  trainIndex <- createDataPartition(ModelData$Watershed, p = 0.8, list = FALSE)
  traindata <- ModelData[trainIndex, ]
  testdata <- ModelData[-trainIndex, ]
  testmetadata <- metadata[-trainIndex, ]
  
  # Train the model
  control <- trainControl(method = "cv", number = 5)
  model <- train(Watershed ~ ., data = traindata, method = "rf", trControl = control)
  
  # Make predictions
  predictions <- predict(model, testdata)
  predictions <- as.factor(predictions)
  testdata$Watershed <- as.factor(testdata$Watershed)
  
  # Create a results dataframe
  results <- data.frame(
    Fish_id = testmetadata$Fish_id,
    Dataset = dataset_name,
    Predicted_Watershed = predictions,
    Actual_Watershed = testdata$Watershed,
    Correct_Classified = ifelse(predictions == testdata$Watershed, "Yes", "No")
  )
  
  return(results)
}

# List of datasets and their names
datasets <- list(
  RAW = iso_data_raw[, 6:ncol(iso_data_raw)],
  MA = iso_data_MA[, 6:ncol(iso_data_MA)],
  GAM = iso_data_GAM[, 6:ncol(iso_data_GAM)]
)

# Metadata (same for all datasets)
metadata <- iso_data_raw[, 1:5]

# Run classification for each dataset and store results
all_results <- data.frame()
for (dataset_name in names(datasets)) {
  dataset <- datasets[[dataset_name]]
  results <- run_classification(dataset, metadata, dataset_name)
  all_results <- rbind(all_results, results)
}

# Save the results to a CSV file
write.csv(all_results, file = here("Data/Model Results/classification_comparison.csv"), row.names = FALSE)


#### Read in the results 
all_results <- read.csv(here("Data/Model Results/classification_comparison.csv"))

################################################################################
# Step 2: Summarize Classification Accuracy by Dataset
################################################################################

# Summarize the classification accuracy for RAW, MA, and GAM datasets
accuracy_summary <- all_results %>%
  group_by(Dataset) %>%
  summarize(
    Total_Fish = n(),  # Total number of fish in each dataset
    Correct_Classified = sum(Correct_Classified == "Yes"),  # Number of correctly classified fish
    Accuracy = Correct_Classified / Total_Fish  # Classification accuracy
  )

# Print the accuracy summary
print(accuracy_summary)

################################################################################
# Step 3: Identify Fish Correctly Classified in GAM but Not in RAW
################################################################################

# Find fish that were correctly classified in GAM but incorrectly classified in RAW
correct_gam_incorrect_raw <- all_results %>%
  filter(Dataset == "GAM" & Correct_Classified == "Yes") %>%  # Correct in GAM
  inner_join(
    all_results %>% filter(Dataset == "RAW" & Correct_Classified == "No"),  # Incorrect in RAW
    by = "Fish_id"
  ) %>%
  select(Fish_id)  # Keep only the Fish_id column

# Print the list of selected fish IDs
print(correct_gam_incorrect_raw)

# Extract the Fish IDs for further analysis
selected_fish_ids_correct <- correct_gam_incorrect_raw$Fish_id

################################################################################
# Step 4: Identify Fish NOT Correctly Classified by GAM
################################################################################

# Find fish that were NOT correctly classified by GAM
incorrect_gam <- all_results %>%
  filter(Dataset == "GAM" & Correct_Classified == "No") %>%  # Incorrect in GAM
  select(Fish_id)  # Keep only the Fish_id column

# Print the list of selected fish IDs
print(incorrect_gam)

# Extract the Fish IDs for further analysis
selected_fish_ids_incorrect <- incorrect_gam$Fish_id

################################################################################
# Step 5: Extract and Clean Time Series Data for Selected Fish
################################################################################

# Function to extract and clean time series data
extract_and_clean_data <- function(selected_fish_ids, iso_data_raw, iso_data_GAM, reasonable_max) {
  # Extract RAW and GAM data for the selected fish IDs
  raw_data_selected <- iso_data_raw %>%
    filter(Fish_id %in% selected_fish_ids) %>%  # Filter for selected fish
    pivot_longer(cols = starts_with("X"), names_to = "Distance", values_to = "Iso") %>%  # Reshape to long format
    mutate(Distance = as.numeric(gsub("X", "", Distance)),  # Convert Distance to numeric
           Dataset = "RAW" )  # Add a column to indicate the dataset
  
  gam_data_selected <- iso_data_GAM %>%
    filter(Fish_id %in% selected_fish_ids) %>%  # Filter for selected fish
    pivot_longer(cols = starts_with("X"), names_to = "Distance", values_to = "Iso") %>%  # Reshape to long format
    mutate(Distance = as.numeric(gsub("X", "", Distance)),  # Convert Distance to numeric
           Dataset = "GAM" )  # Add a column to indicate the dataset
  
  # Cap Iso values at the reasonable maximum for RAW and GAM datasets
  raw_data_selected <- raw_data_selected %>%
    mutate(Iso = ifelse(Iso > reasonable_max, reasonable_max, Iso))  # Cap RAW data
  
  gam_data_selected <- gam_data_selected %>%
    mutate(Iso = ifelse(Iso > reasonable_max, reasonable_max, Iso))  # Cap GAM data
  
  # Combine RAW and GAM data for plotting
  combined_data <- bind_rows(raw_data_selected, gam_data_selected)
  
  # Remove the first row (if necessary)
  combined_data <- combined_data[-1, ]
  
  return(combined_data)
}

# Define a reasonable maximum value for Iso to cap outliers
reasonable_max <- 0.7150  # Adjust this based on your data

# Extract and clean data for correctly classified by GAM but not RAW
combined_data_correct <- extract_and_clean_data(selected_fish_ids_correct, iso_data_raw, iso_data_GAM, reasonable_max)

# Extract and clean data for NOT correctly classified by GAM
combined_data_incorrect <- extract_and_clean_data(selected_fish_ids_incorrect, iso_data_raw, iso_data_GAM, reasonable_max)

################################################################################
# Step 6: Visualize Time Series for Both Groups
################################################################################

# Function to plot time series
plot_time_series <- function(combined_data, title) {
  ggplot(combined_data, aes(x = Distance, y = Iso, color = Dataset)) +
    # Plot RAW data with transparency
    geom_point(
      data = filter(combined_data, Dataset == "RAW"),
      size = 1, alpha = 0.7, color = "gray"  # Gray with transparency
    ) +
    # Plot GAM data with no transparency and bright orange
    geom_line(
      data = filter(combined_data, Dataset == "GAM"),
      size = 1, alpha = 1, color = "navyblue"  # Bright orange, fully opaque
    ) +
    facet_wrap(~ Fish_id, scales = "free_y") +  # Separate plots for each fish
    labs(
      title = title,
      x = "Distance",
      y = "Iso"
    ) +
    theme_minimal() +  # Use a minimal theme
    theme(
      strip.text = element_text(size = 10, face = "bold"),  # Customize facet labels
      legend.position = "bottom"  # Move legend to the bottom
    )
}

# Plot for individuals correctly classified by GAM but not RAW
plot_time_series(combined_data_correct, "Individuals Correctly Classified Using GAM but Not RAW")

# Plot for individuals NOT correctly classified by GAM
plot_time_series(combined_data_incorrect, "Individuals NOT Correctly Classified Using GAM")

################################################################################

##### Organize the results of the classification into a dataframe

fish_ids<- testmetadata$Fish_id

create_classification_results <- function(predictions, actuals, fish_ids) {
  result <- data.frame(
    ID = fish_ids,
    Correct_Classified = ifelse(predictions == actuals, "Yes", "No")
  )
  return(result)
}

# Create DF with rf results by fish ID 
rf_results <- create_classification_results(predictions, testmetadata$Watershed, fish_ids)
rf_results$Natal_iso <- testmetadata$Natal_iso[match(rf_results$fish_id, testmetadata$Fish_id)] # Add Natal_iso
write.csv(rf_results, "Data/Model Results/testing/RF_classification_results.csv", row.names = FALSE)

# Read in rf results 
rf_results <- read.csv("Data/Model Results/testing/RF_classification_results.csv")


# Merge PCA data with classification results
PCA_full <- left_join(PCA_full, rf_results, by = c("Fish_id" = "ID"))

# Create classification color mapping
PCA_full$Classified_Color <- ifelse(PCA_full$Correct_Classified == "Yes", "green", "red")
PCA_full$Classified_Color[is.na(PCA_full$Correct_Classified)] <- "grey"






################################################################################
################################################################################
################# R Shiny exploration plot #####################################
################################################################################


# UI
ui <- fluidPage(
  titlePanel("PCA Analysis Viewer"),
  
  sidebarLayout(
    sidebarPanel(
      width = 6,
      helpText("Click on a point in the PCA plot to view Iso vs. Distance for that Fish ID."),
      selectInput("xComp", "X Component:", choices = names(PCA_full), selected = "PC1"),
      selectInput("yComp", "Y Component:", choices = names(PCA_full), selected = "PC2"),
      actionButton("resetZoom", "Reset Zoom"),
      actionButton("toggleColor", "Random Forest Classification")  # Button to toggle coloring
    ),
    mainPanel(
      width = 6, 
      plotOutput("pcaPlot", click = "pcaClick", brush = brushOpts(id = "pcaBrush", resetOnNew = TRUE)),
      plotOutput("isoPlot")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive values for zoom regions
  zoomRegion <- reactiveValues(x = NULL, y = NULL)
  
  # Reactive value to track the current coloring scheme
  coloringScheme <- reactiveVal("Watershed")  # Default to Watershed
  
  # Toggle coloring scheme when the button is clicked
  observeEvent(input$toggleColor, {
    if (coloringScheme() == "Watershed") {
      coloringScheme("Classification")
    } else {
      coloringScheme("Watershed")
    }
  })
  
  observeEvent(input$resetZoom, {
    zoomRegion$x <- NULL
    zoomRegion$y <- NULL
  })
  
  observeEvent(input$pcaBrush, {
    brush <- input$pcaBrush
    if (!is.null(brush)) {
      zoomRegion$x <- c(brush$xmin, brush$xmax)
      zoomRegion$y <- c(brush$ymin, brush$ymax)
    }
  })
  
  # PCA plot with dynamic coloring
  # PCA plot with dynamic coloring
  output$pcaPlot <- renderPlot({
    if (coloringScheme() == "Watershed") {
      ggplot(PCA_full, aes_string(x = input$xComp, y = input$yComp, color = "Watershed")) +
        geom_point(size = 2, alpha = 0.4) +
        theme_classic() +
        labs(title = "PCA of Iso Values by Watershed",
             x = input$xComp,
             y = input$yComp) +
        theme(legend.title = element_blank()) +
        coord_cartesian(xlim = zoomRegion$x, ylim = zoomRegion$y)
    } else {
      # Create a new alpha column based on the Classified_Color
      PCA_full <- PCA_full %>%
        mutate(alpha = ifelse(Classified_Color == "grey", 0, 1))
      
      ggplot(PCA_full, aes_string(x = input$xComp, y = input$yComp, color = "Classified_Color")) +
        geom_point(size = 2, alpha = PCA_full$alpha) +
        scale_color_identity() +
        theme_classic() +
        labs(title = "PCA of Iso Values by Classification Accuracy",
             x = input$xComp,
             y = input$yComp) +
        theme(legend.position = "none") +
        coord_cartesian(xlim = zoomRegion$x, ylim = zoomRegion$y)
    }
  })
  
  # Reactive value to store the selected Fish ID
  selectedFish <- reactiveVal(NULL)
  
  # Update selectedFish when clicking in the PCA plot
  observeEvent(input$pcaClick, {
    nearPoint <- nearPoints(PCA_full, input$pcaClick, threshold = 5, maxpoints = 1)
    if (nrow(nearPoint) > 0) {
      selectedFish(nearPoint$Fish_id[1])
    }
  })
  
  # Iso plot for selected Fish ID
  output$isoPlot <- renderPlot({
    req(selectedFish())  # Ensure a Fish ID is selected
    
    # Find the index of the selected Fish ID
    fishIndex <- which(selected_metadata$Fish_id == selectedFish())
    
    if (length(fishIndex) == 0) return(NULL)  # If no valid index, exit
    
    # Extract Iso data
    isoData <- tibble(
      Distance = seq_along(selected_data[fishIndex, ]),
      Iso = selected_data[fishIndex, ]
    ) %>%
      mutate(MovingAvg = zoo::rollapply(Iso, width = 60, FUN = mean, fill = NA, align = "center"))
    
    # Plot
    ggplot(isoData, aes(x = Distance, y = Iso)) +
      geom_point(alpha = 0.5) +
      geom_line(aes(y = MovingAvg), color = "blue", size = 1) +
      geom_hline(yintercept = 0.7092, color = "gold", size = 2) +
      theme_grey() +
      labs(title = paste("Iso vs. Distance for Fish ID:", selectedFish()),
           x = "Distance",
           y = "Iso")
  })
}



# Run the application
shinyApp(ui, server)


