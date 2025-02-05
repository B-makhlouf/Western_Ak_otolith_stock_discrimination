# Load necessary libraries
library(tidyverse)
library(here)
library(shiny)
library(viridis)
library(shiny)


### Source in the function which does the data preprocessing. 
source(here("/Users/benjaminmakhlouf/Research_repos/Western_Ak_otolith_stock_discrimination/Code/Helper Code/Raw_Data_Preprocessing.R"))
source(here("/Users/benjaminmakhlouf/Research_repos/Western_Ak_otolith_stock_discrimination/Code/Helper Code/PCA_functions.R"))

# Run the function 
processed_data<-process_trimmed_data()


###############################################################################

# Pull out the metadata and measurement array from the processed data
metadata <- tibble(
  Fish_id = processed_data$ids,
  Watershed = processed_data$watersheds,
  Natal_Iso = processed_data$natal_origins,
  Year = processed_data$Year
)

iso_data<- processed_data$measurement_array

all_data_combined<- cbind(metadata, iso_data)

# save as a .csv 
write.csv(all_data_combined, file = here("Data/Processed/all_data_combined.csv"))

#FILTER 

#if you dont want to filter anything, just set selected_indices to 1:nrow(metadata)
selected_indices <- 1:nrow(metadata)

# otherwise you can filter here
#selected_indices <- which(metadata$Year != )

# Filter both
metadata_filtered <- metadata[selected_indices,]
measurement_array_filtered <- iso_data[selected_indices,]


#############################################################################
#############################################################################
#################### PCA Exploration ########################################
#############################################################################
PCA_raw <- prcomp(measurement_array_filtered, scale. = TRUE) #run the pca 
PCA_full<- run_pca(measurement_array_filtered, metadata_filtered) #add all the metadata

### figures 

#### PCA data with natal origin
natalIsoPCAPlot<-pca_natal_plot(PCA_full,1,2) # make a plot of the PCA's colored by watershed AND natal origin
print(natalIsoPCAPlot)

### Feature importance
feature_figure<- plot_pca_loadings(PCA_raw, plot_type = "line")
plot(feature_figure)


###########################################################

### Read in most recent ML classifier success 
rf_results<- read.csv(here("/Users/benjaminmakhlouf/Research_repos/Western_Ak_otolith_stock_discrimination/Data/Model Results/testing/rf_results.csv"))






################# R Shiny exploration plot 

library(shiny)
library(ggplot2)
library(dplyr)
library(zoo)

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
      actionButton("toggleColor", "Toggle Coloring Scheme")  # Button to toggle coloring
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
  
  # Merge PCA data with classification results
  PCA_full <- left_join(PCA_full, rf_results, by = c("Fish_id" = "ID"))
  
  # Create classification color mapping
  PCA_full$Classified_Color <- ifelse(PCA_full$Correct_Classified == "Yes", "green", "red")
  PCA_full$Classified_Color[is.na(PCA_full$Correct_Classified)] <- "grey"
  
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
      ggplot(PCA_full, aes_string(x = input$xComp, y = input$yComp, color = "Classified_Color")) +
        geom_point(size = 2, alpha = 0.6) +
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
    fishIndex <- which(metadata_filtered$Fish_id == selectedFish())
    
    if (length(fishIndex) == 0) return(NULL)  # If no valid index, exit
    
    # Extract Iso data
    isoData <- tibble(
      Distance = seq_along(measurement_array_filtered[fishIndex, ]),
      Iso = measurement_array_filtered[fishIndex, ]
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

