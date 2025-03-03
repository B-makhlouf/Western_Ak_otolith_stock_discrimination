library(viridis)
library(patchwork)
library(plotly)
library(tidyverse)
library(here)
library(caret)
library(shiny)

######################

source(here("/Users/benjaminmakhlouf/Research_repos/Western_Ak_otolith_stock_discrimination/Code/Helper Code/PCA_functions.R"))### This script contains helper functions to run PCA and a few important figures
source(here("/Users/benjaminmakhlouf/Research_repos/Western_Ak_otolith_stock_discrimination/Code/Helper Code/Raw_Data_Preprocessing.R"))
All_Metadata<- read.csv(here("Data/Final/Metadata_and_QC.csv"))

########### Read in the most recent processed data 
################################################################################

data_type <- "RAW"  # Choose from "RAW", "MA", or "GAM"
landmark_filter <- c("Core", "Fw")  # Specify landmarks used during processing

# Load the processed data
processed_data <- load_processed_data(data_type, landmark_filter)

# Merge iso_data_raw with All_Metadata based on Fish_id and Fish_ID
AnalysisDataAll <- processed_data %>%
  left_join(All_Metadata %>% select(-Year), by = c("Fish_id" = "Fish_ID")) %>%
  select((ncol(.)-12):ncol(.), everything()) 

#############################
#### SELECT WHICH DATA TO RUN 
#############################
Analysis_metadata<- AnalysisDataAll[,1:17] #Seperate Metadata 
Analysis_ts_data<- AnalysisDataAll[,18:length(AnalysisDataAll)] #Seperate Isotope ts data 



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
combined_plot <- natalIsoPCAPlot / (scree_plot_1 | feature_figure)
print(combined_plot)

# save as PCA dashboard 
ggsave(here("Figures/PCA_dashboard.pdf"), plot = combined_plot, width = 30, height = 30, units = "in", dpi = 300)

# ####
#3D plot
# plot_ly(
#   x = PCA_full$PC1,
#   y = PCA_full$PC2,
#   z = PCA_full$PC3,
#   type = "scatter3d",
#   mode = "markers",
#   marker = list(
#     size = 2,  # Adjust size
#     opacity = 0.7  # Adjust transparency (0 = fully transparent, 1 = fully opaque)
#   ),
#   color = PCA_full$Watershed
# )
}

###############################
##############################


### RF 

# Ensure selected_data is a dataframe and add Watershed
ModelData <- Analysis_ts_data %>% as.data.frame() %>% mutate(Watershed = Analysis_metadata$Watershed)

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

# Convert the confusion matrix to a tidy format
conf_matrix_df <- as.data.frame(conf_matrix$table)

# Add metadata to idScored by Fish_ID 
idScores <- left_join(idScores, All_Metadata, by = c("Fish_id" = "Fish_ID"))

############################### 
########## Shiny App 
##############################
library(shiny)
library(ggplot2)
library(dplyr)
library(here)
library(plotly)
library(zoo)

dataset_description <- reactive({
  paste("Data Type:", data_type, "| Landmarks:", paste(landmark_filter, collapse = ", "))
})

# UI
ui <- fluidPage(
  titlePanel("PCA Analysis Viewer"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,  # Reduce sidebar width to maximize plot area
      helpText("Click on a point in the PCA plot to view Iso vs. Distance for that Fish ID."),
      selectInput("xComp", "X Component:", choices = names(PCA_full), selected = "PC1"),
      selectInput("yComp", "Y Component:", choices = names(PCA_full), selected = "PC2"),
      actionButton("resetZoom", "Reset Zoom"),
      hr(),
      textOutput("datasetInfo")  # Display dataset information
    ),
    mainPanel(
      width = 9,  # Increase main panel width for larger plots
      tabsetPanel(
        id = "tabs",  # Assign an ID to tabsetPanel for conditional logic
        tabPanel("2D PCA", 
                 plotOutput("pcaPlot", click = "pcaClick", 
                            brush = brushOpts(id = "pcaBrush", resetOnNew = TRUE))
        ),
        tabPanel("3D PCA", 
                 plotlyOutput("pcaPlot3D",  height = "800px")  # Interactive 3D PCA plot
        ),
        tabPanel("Feature Loadings", 
                 plotOutput("featurePlot")
        ),
        tabPanel("Classification Diagnostics",
                 verbatimTextOutput("confMatrixText"),                 
                 plotOutput("yearPlot"),
                 plotOutput("qcPlot"),
                 plotOutput("corePlot")
        )
      ),
      
      conditionalPanel(
        condition = "input.tabs == '2D PCA'",
        plotOutput("isoPlot")
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  output$datasetInfo <- renderText({
    dataset_description()
  })
  
  zoomRegion <- reactiveValues(x = NULL, y = NULL)
  
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
  
  output$pcaPlot <- renderPlot({
    req(PCA_full)
    ggplot(PCA_full, aes_string(x = input$xComp, y = input$yComp, color = "Watershed")) +
      geom_point(size = 2, alpha = 0.4) +
      theme_classic() +
      labs(title = "PCA of Iso Values by Watershed",
           x = input$xComp,
           y = input$yComp) +
      theme(legend.title = element_blank()) +
      coord_cartesian(xlim = zoomRegion$x, ylim = zoomRegion$y)
  })
  output$confMatrixText <- renderPrint({
    req(conf_matrix)
    print(conf_matrix)
  })
  output$pcaPlot3D <- renderPlotly({
    plot_ly(
      x = PCA_full$PC1,
      y = PCA_full$PC2,
      z = PCA_full$PC3,
      type = "scatter3d",
      mode = "markers",
      marker = list(
        size = 3,  
        opacity = 0.5  
      ),
      color = PCA_full$Watershed
    ) %>%
      layout(
        title = "3D PCA Plot",
        scene = list(
          xaxis = list(title = "PC1"),
          yaxis = list(title = "PC2"),
          zaxis = list(title = "PC3")
        )
      )
  })
  
  selectedFish <- reactiveVal(NULL)
  
  observeEvent(input$pcaClick, {
    nearPoint <- nearPoints(PCA_full, input$pcaClick, threshold = 5, maxpoints = 1)
    if (nrow(nearPoint) > 0) {
      selectedFish(nearPoint$Fish_id[1])
    }
  })
  
  output$isoPlot <- renderPlot({
    req(selectedFish(), Analysis_metadata, Analysis_ts_data)
    
    fishIndex <- which(Analysis_metadata$Fish_id == selectedFish())
    if (length(fishIndex) == 0) return(NULL)
    
    isoData <- tibble(
      Distance = seq_along(Analysis_ts_data[fishIndex, ]),
      Iso = as.numeric(Analysis_ts_data[fishIndex, ])
    ) %>%
      mutate(MovingAvg = zoo::rollapply(Iso, width = 60, FUN = mean, fill = NA, align = "center"))
    
    ggplot(isoData, aes(x = Distance, y = Iso)) +
      geom_point(alpha = 0.5) +
      geom_line(aes(y = MovingAvg), color = "blue", size = 1) +
      geom_hline(yintercept = 0.7092, color = "gold", size = 2) +
      theme_grey() +
      labs(title = paste("Iso vs. Distance for Fish ID:", selectedFish()),
           x = "Distance",
           y = "Iso")
  })
  
  output$featurePlot <- renderPlot({
    print(feature_figure)  
  })
  
  output$yearPlot <- renderPlot({ year_proportion })
  output$qcPlot <- renderPlot({ qc_proportion })
  output$corePlot <- renderPlot({ core_proportion })
}

shinyApp(ui = ui, server = server)



















## Confidence Scores 
confidence_scores<-ggplot(idScores, aes(x = Confidence, fill = Correct)) +
  geom_density(alpha = 0.6) +
  scale_fill_manual(values = c("red", "green")) +
  theme_minimal() +
  labs(title = "Confidence Score Distribution", x = "Confidence Score", y = "Density")

# Plot the proportion of year for incorrect vs correctly identified as a stacked bar plot 
year_proportion <- idScores %>%
  group_by(Year, Actual, Correct) %>%
  summarise(Count = n(), .groups = "drop") %>%  # Count occurrences
  group_by(Year, Actual) %>%  # Group again by Year and Actual only
  mutate(Proportion = Count / sum(Count)) %>%  # Compute proportion within each group
  ggplot(aes(x = Year, y = Proportion, fill = Correct)) +
  geom_bar(stat = "identity", position = "stack", alpha = 0.8) +  # Adjust transparency
  scale_fill_manual(values = c("firebrick", "seagreen")) +
  facet_wrap(~Actual, ncol = 1, scales = "free_y") +  # Display in one column
  theme_grey() +
  labs(
    title = "Year and Watershed",
    x = "Year",
    y = "Proportion"
  )

# Correctly vs incorrect by QC_score 

qc_proportion <- idScores %>%
  filter(!QC_Grade %in% c("Good", NA)) %>%  # Exclude "Good" and NA values
  group_by(QC_Grade, Correct) %>%  # Group by QC_Grade and Correct first
  summarise(Count = n(), .groups = "drop") %>%  # Count occurrences
  group_by(QC_Grade) %>%  # Group again by QC_Grade only
  mutate(Proportion = Count / sum(Count)) %>%  # Compute proportion within QC_Grade
  ggplot(aes(x = QC_Grade, y = Proportion, fill = Correct)) +
  geom_bar(stat = "identity", position = "stack", alpha = 0.8) +  # Ensure stacking
  scale_fill_manual(values = c("firebrick", "seagreen")) +
  theme_grey() +
  labs(
    title = "QC_Grade",
    x = "QC_Grade",
    y = "Proportion"
  )

## Same thing but by Core_status

core_proportion <- idScores %>%
  filter(!Core_Status %in% c("Good", NA)) %>%  # Exclude "Good" and NA values
  group_by(Core_Status, Correct) %>%  # Group by Core_status and Correct first
  summarise(Count = n(), .groups = "drop") %>%  # Count occurrences
  group_by(Core_Status) %>%  # Group again by Core_status only
  mutate(Proportion = Count / sum(Count)) %>%  # Compute proportion within Core_status
  ggplot(aes(x = Core_Status, y = Proportion, fill = Correct)) +
  geom_bar(stat = "identity", position = "stack", alpha = 0.8) +  # Ensure stacking
  scale_fill_manual(values = c("firebrick", "seagreen")) +
  theme_grey() +
  labs(
    title = "Core_status",
    x = "Core_status",
    y = "Proportion"
  )


# Filter idScores to be only "Actual" == Yukon 
yukon_idScores <- idScores %>%
  filter(Actual == "Yukon")

# Plot the proportion of correct vs incorrecy by "gen_Likely" 

gen_proportion <- yukon_idScores %>%
  group_by(likely_gen, Correct) %>%  # Group by likely_gen and Correct first
  summarise(Count = n(), .groups = "drop") %>%  # Count occurrences
  group_by(likely_gen) %>%  # Group again by likely_gen only
  mutate(Proportion = Count / sum(Count)) %>%  # Compute proportion within each likely_gen
  ggplot(aes(x = likely_gen, y = Proportion, fill = Correct)) +
  geom_bar(stat = "identity", position = "stack", alpha = 0.8) +  # Adjust transparency
  scale_fill_manual(values = c("firebrick", "seagreen")) +
  theme_grey() +
  labs(
    title = "Genetic Groups",
    x = "Gen_Likely",
    y = "Proportion"
  )

# Arrange the plots in one big figure using patchwork
final_plot <- (Heatmap + confidence_scores) / 
  (year_proportion + qc_proportion) / 
  (core_proportion + gen_proportion)

ggsave("figures/RF_Dashboard.pdf", plot = final_plot, width = 30, height = 30, units = "cm")





