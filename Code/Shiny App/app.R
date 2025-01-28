# UI
library(shiny)
library(ggplot2)
library(dplyr)

# UI
ui <- fluidPage(
  titlePanel("PCA Analysis Viewer"),
  # Define the landing page section with an image
  div(
    class = "landing-page",
    tags$img(src = "landing_image.jpg", height = "80%", width = "80%"),
    tags$h2("Welcome to the PCA Analysis Viewer"),
    tags$p("Explore the PCA plot and click to view detailed Iso vs. Distance data for each Fish ID."),
    style = "text-align: center; margin-top: 100px;"
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 6,
      helpText("Click on a point in the PCA plot to view Iso vs. Distance for that Fish ID."),
      helpText("Drag to zoom in on the PCA plot."),
      selectInput("xComp", "X Component:", choices = names(pca_results), selected = "PC1"),
      selectInput("yComp", "Y Component:", choices = names(pca_results), selected = "PC2"),
      actionButton("resetZoom", "Reset Zoom"),
      tags$div(style = "margin-top: 130px;", plotOutput("featurePlot"))  # Add space with margin-top
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
  
  # Reactive values for filtered PCA data and zoom region
  filteredPCA <- reactiveVal(pca_results)
  zoomRegion <- reactiveValues(x = NULL, y = NULL)
  
  # Reset zoom when the reset button is clicked
  observeEvent(input$resetZoom, {
    zoomRegion$x <- NULL
    zoomRegion$y <- NULL
  })
  
  # Observe brush input for zoom
  observeEvent(input$pcaBrush, {
    brush <- input$pcaBrush
    if (!is.null(brush)) {
      zoomRegion$x <- c(brush$xmin, brush$xmax)
      zoomRegion$y <- c(brush$ymin, brush$ymax)
    } else {
      zoomRegion$x <- NULL
      zoomRegion$y <- NULL
    }
  })
  
  # Render PCA Plot with filtering and zoom
  output$pcaPlot <- renderPlot({
    ggplot(filteredPCA(), aes_string(x = input$xComp, y = input$yComp, color = "Watershed")) +
      geom_point(size = 2, alpha = 0.4) +
      theme_classic() +
      labs(title = "PCA of Iso Values by Watershed",
           x = input$xComp,
           y = input$yComp) +
      theme(legend.title = element_blank()) +
      coord_cartesian(
        xlim = zoomRegion$x,
        ylim = zoomRegion$y
      )
  })
  
  output$featurePlot <- renderPlot({
    feature_plot  # Render the saved ggplot object
  })
  
  # Reactive value to store the selected Fish ID
  selectedFish <- reactiveVal()
  
  # Update selected Fish ID based on click
  observeEvent(input$pcaClick, {
    nearPoint <- nearPoints(filteredPCA(), input$pcaClick, threshold = 5, maxpoints = 1)
    if (nrow(nearPoint) > 0) {
      selectedFish(nearPoint$Fish_id[1])
    }
  })
  
  # Render Iso Plot for selected Fish ID
  output$isoPlot <- renderPlot({
    req(selectedFish())
    
    # Find the index of the selected Fish ID
    fishIndex <- which(ids == selectedFish())
    
    # Retrieve Iso data for the selected Fish ID
    isoData <- tibble(
      Distance = seq_along(measurement_array[fishIndex, ]),
      Iso = measurement_array[fishIndex, ]
    )
    
    # Calculate moving average
    isoData <- isoData %>%
      mutate(MovingAvg = zoo::rollapply(Iso, width = 60, FUN = mean, fill = NA, align = "center"))
    
    # Combine all PC feature values and assign colors
    # Combine all PC feature values and assign colors
    pc_features <- c(PC1_features, PC2_features, PC3_features, PC4_features, PC5_features)
    feature_colors <- c(rep(plotcolors[1], length(PC1_features)), 
                        rep(plotcolors[2], length(PC2_features)), 
                        rep(plotcolors[3], length(PC3_features)),
                        rep(plotcolors[4], length(PC4_features)),
                        rep(plotcolors[5], length(PC5_features)))
    
    # Create a dataframe for PC feature lines to map colors to labels
    pc_df <- tibble(
      Feature = factor(c(rep("PC1", length(PC1_features)),
                         rep("PC2", length(PC2_features)),
                         rep("PC3", length(PC3_features)),
                         rep("PC4", length(PC4_features)),
                         rep("PC5", length(PC5_features)))),
      Value = c(PC1_features, PC2_features, PC3_features, PC4_features, PC5_features),
      Color = c(rep(plotcolors[1], length(PC1_features)),
                rep(plotcolors[2], length(PC2_features)),
                rep(plotcolors[3], length(PC3_features)),
                rep(plotcolors[4], length(PC4_features)),
                rep(plotcolors[5], length(PC5_features)))
    )
    
    
    # Plot Iso vs Distance with moving average and horizontal lines
    ggplot(isoData, aes(x = Distance, y = Iso)) +
      geom_point(alpha = 0.5) +
      geom_line(aes(y = MovingAvg), color = "blue", size = 1) +
      geom_vline(data = pc_df, aes(xintercept = Value, color = Feature)) +
      geom_hline(yintercept = 0.7092, color = "gold") +
      theme_grey() +
      labs(title = paste("Iso vs. Distance for Fish ID:", selectedFish()),
           x = "Distance",
           y = "Iso") +
      scale_color_manual(values = setNames(plotcolors, c("PC1", "PC2", "PC3", "PC4", "PC5")),
                         name = "PC Features") +
      theme(legend.title = element_text(size = 12), 
            legend.text = element_text(size = 10)) +
      guides(color = guide_legend(override.aes = list(size = 5, shape = 16)))  # Change shape to point
  })
}

# Run the App
shinyApp(ui, server)
