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
  
  # Reset zoom region
  observeEvent(input$resetZoom, {
    zoomRegion$x <- NULL
    zoomRegion$y <- NULL
  })
  
  # Update zoom region based on brush input
  observeEvent(input$pcaBrush, {
    brush <- input$pcaBrush
    if (!is.null(brush)) {
      zoomRegion$x <- c(brush$xmin, brush$xmax)
      zoomRegion$y <- c(brush$ymin, brush$ymax)
    }
  })
  
  # PCA plot with dynamic coloring
  output$pcaPlot <- renderPlot({
    req(PCA_full)  # Ensure PCA_full is available
    
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
      # Ensure Classified_Color column exists
      if (!"Classified_Color" %in% colnames(PCA_full)) {
        PCA_full$Classified_Color <- "grey"  # Default to grey if column doesn't exist
      }
      
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
    req(selectedFish(), Analysis_metadata, Analysis_ts_data)  # Ensure required data is available
    
    # Find the index of the selected Fish ID
    fishIndex <- which(Analysis_metadata$Fish_id == selectedFish())
    
    if (length(fishIndex) == 0) return(NULL)  # If no valid index, exit
    
    # Extract Iso data
    isoData <- tibble(
      Distance = seq_along(Analysis_ts_data[fishIndex, ]),
      Iso = Analysis_ts_data[fishIndex, ]
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

# Run the Shiny app
shinyApp(ui = ui, server = server)