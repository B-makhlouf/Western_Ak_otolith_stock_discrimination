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

### This script contains the function which preprocesses all of the raw data into a form that can be used for PCA/ML/Etc.  
source(here("/Users/benjaminmakhlouf/Research_repos/Western_Ak_otolith_stock_discrimination/Code/Helper Code/Raw_Data_Preprocessing.R"))
### This script contains helper functions to run PCA and a few important figures
source(here("/Users/benjaminmakhlouf/Research_repos/Western_Ak_otolith_stock_discrimination/Code/Helper Code/PCA_functions.R"))

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
processed_data<-process_trimmed_data() 

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
iso_data_GAM<- processed_data$gam_smoothed_array # GAM smoothed data

iso_data_MA <- iso_data_MA[, colSums(is.na(iso_data_MA)) == 0] # MA has tails of NA, remove

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

### Feature importance visualized along the timeseries 
## Changing the plot type will change the visualization (options are "line" or "bar")
feature_figure<- plot_pca_loadings(PCA_raw, plot_type = "line")
plot(feature_figure)

################################################################################
################################################################################
################# Machine Learning Classifier ##################################
################################################################################

# Add "Watershed" back into selected_data and call it ModelData 
ModelData <- as.data.frame(selected_data) # Convert matrix to data frame
ModelData$Watershed <- selected_metadata$Watershed # Add Watershed column

# Split the data into training and testing sets
library(caret)
set.seed(123)
trainIndex <- createDataPartition(ModelData$Watershed, p = 0.8, list = FALSE)
traindata<- ModelData[trainIndex,]
trainmetadata<- selected_metadata[trainIndex,]
testdata<- ModelData[-trainIndex,]
testmetadata<- selected_metadata[-trainIndex,]

### Set parameters, # of cross validation, method, and train the model
control <- trainControl(method = "cv", number = 5) # n fold cross validation 
model <- train(Watershed ~ ., data = traindata, method = "rf", trControl = control) # Train the model (long step)
predictions <- predict(model, testdata) # Make predictions on the test set 
predictions <- as.factor(predictions) # Convert to factor for confusion matrix
testdata$Watershed <- as.factor(testdata$Watershed) # Convert to factor for confusion matrix
conf_matrix <- confusionMatrix(predictions, testdata$Watershed) # Create confusion matrix
conf_matrix

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


