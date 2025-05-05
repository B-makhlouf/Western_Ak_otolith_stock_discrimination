# app.R - Optimized PCA Viewer using precomputed data
# Uses precomputed PCA data for faster reactivity

library(shiny)
library(tidyverse)
library(ggplot2)
library(here)

# UI
ui <- fluidPage(
  titlePanel("PCA Analysis Viewer"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      # Select data type
      selectInput("dataType", "Data Type:",
                  choices = c("GAM", "MA", "RAW", "Sr88", "Combined"),
                  selected = "GAM"),
      
      # PCA components selection
      selectInput("xComp", "X Component:", choices = paste0("PC", 1:5), selected = "PC1"),
      selectInput("yComp", "Y Component:", choices = paste0("PC", 1:5), selected = "PC2"),
      
      # Zoom control
      actionButton("resetZoom", "Reset Zoom"),
      
      hr(),
      
      # Display variance explained
      uiOutput("varianceUI"),
      
      hr(),
      
      helpText("Click on a point in the PCA plot to view the corresponding timeseries data.")
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("PCA Plot", 
                 plotOutput("pcaPlot", 
                            click = "pcaClick", 
                            brush = brushOpts(id = "pcaBrush", resetOnNew = TRUE),
                            height = "400px"),
                 plotOutput("timeseriesPlot", height = "300px"),
                 verbatimTextOutput("fishInfo")
        ),
        tabPanel("Variance Explained",
                 plotOutput("variancePlot", height = "400px")
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive values to store data
  raw_data <- reactiveVal(NULL)
  pca_data <- reactiveVal(NULL)
  selected_fish <- reactiveVal(NULL)
  
  # Reactive value for zoom regions
  zoom_region <- reactiveValues(x = NULL, y = NULL)
  
  # Load precomputed PCA data based on selected type
  observe({
    # Get data type
    data_type <- tolower(input$dataType)
    
    # Construct file path
    filepath <- here("data/pca_precomputed", paste0("pca_", data_type, ".rds"))
    
    # Check if file exists
    if (file.exists(filepath)) {
      # Load precomputed data
      precomputed <- readRDS(filepath)
      pca_data(precomputed)
      
      # Update component choices based on available PCs
      pc_columns <- grep("^PC", names(precomputed$pca_data), value = TRUE)
      updateSelectInput(session, "xComp", 
                        choices = pc_columns,
                        selected = "PC1")
      updateSelectInput(session, "yComp", 
                        choices = pc_columns,
                        selected = "PC2")
      
      # Also load raw timeseries data for plotting
      # Construct filename based on data type
      filename <- paste0("preprocessed_", input$dataType, ".csv")
      
      # Try multiple possible locations
      possible_paths <- c(
        here("data/preprocessed_matrices", filename),
        here("Data/preprocessed_matrices", filename),
        here("Data/Processed/Preprocessed_ts_matrices", filename),
        here("Data/02_Preprocessed_ts_matrices", filename)
      )
      
      # Find first existing file
      data_path <- NULL
      for (path in possible_paths) {
        if (file.exists(path)) {
          data_path <- path
          break
        }
      }
      
      if (!is.null(data_path)) {
        # Load raw data (only needed for timeseries plot)
        raw_data(read.csv(data_path))
      }
    } else {
      # Handle error if file not found
      showNotification(
        paste("Precomputed PCA data not found for", input$dataType, "type."), 
        type = "error"
      )
      pca_data(NULL)
    }
  })
  
  # Reset zoom when button is clicked
  observeEvent(input$resetZoom, {
    zoom_region$x <- NULL
    zoom_region$y <- NULL
  })
  
  # Update zoom region based on brush
  observeEvent(input$pcaBrush, {
    brush <- input$pcaBrush
    if (!is.null(brush)) {
      zoom_region$x <- c(brush$xmin, brush$xmax)
      zoom_region$y <- c(brush$ymin, brush$ymax)
    }
  })
  
  # Update selected fish when clicking on PCA plot
  observeEvent(input$pcaClick, {
    req(pca_data())
    
    data <- pca_data()$pca_data
    
    # Find nearest point
    click <- input$pcaClick
    nearPoints <- nearPoints(data, click, threshold = 10, maxpoints = 1)
    
    if (nrow(nearPoints) > 0) {
      selected_fish(nearPoints$Fish_id[1])
    }
  })
  
  # PCA plot
  output$pcaPlot <- renderPlot({
    req(pca_data())
    
    data <- pca_data()$pca_data
    
    # Create PCA plot
    ggplot(data, aes_string(x = input$xComp, y = input$yComp, color = "Watershed")) +
      geom_point(size = 3, alpha = 0.6) +
      theme_classic() +
      labs(title = paste("PCA of", input$dataType, "Values"),
           x = input$xComp, 
           y = input$yComp) +
      coord_cartesian(xlim = zoom_region$x, ylim = zoom_region$y)
  })
  
  # Timeseries plot
  output$timeseriesPlot <- renderPlot({
    req(selected_fish(), raw_data())
    
    data <- raw_data()
    fish_id <- selected_fish()
    
    # Find the selected fish in the data
    fish_row <- which(data$Fish_id == fish_id)
    
    if (length(fish_row) > 0) {
      # Get numeric columns (timeseries data)
      numeric_cols <- grep("^X", names(data), value = TRUE)
      
      # Extract timeseries values for this fish
      timeseries_values <- as.numeric(data[fish_row, numeric_cols])
      
      # Create a data frame for plotting
      plot_data <- data.frame(
        Index = 1:length(timeseries_values),
        Value = timeseries_values
      )
      
      # Create the plot
      ggplot(plot_data, aes(x = Index, y = Value)) +
        geom_line(color = "blue", size = 1) +
        geom_point(alpha = 0.3, size = 1) +
        theme_classic() +
        labs(
          title = paste0(
            "Timeseries for Fish ID: ", fish_id, 
            " (", data$Watershed[fish_row], ")"
          ),
          x = "Index",
          y = ifelse(input$dataType == "Sr88", "Sr88 Value", "Sr87/86 Value")
        )
    } else {
      # Show message if fish not found
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, 
                 label = paste("Fish ID", fish_id, "not found in the dataset")) +
        theme_void() +
        xlim(0, 1) + ylim(0, 1)
    }
  })
  
  # Variance explained plot
  output$variancePlot <- renderPlot({
    req(pca_data())
    
    var_data <- pca_data()$var_data
    # Limit to first 15 components for better visibility
    var_data <- var_data[1:min(15, nrow(var_data)),]
    
    # Create plot
    ggplot(var_data) +
      geom_col(aes(x = PC, y = Variance), fill = "steelblue") +
      geom_line(aes(x = PC, y = CumulativeVariance, group = 1), 
                color = "red", size = 1) +
      geom_point(aes(x = PC, y = CumulativeVariance), 
                 color = "red", size = 3) +
      scale_y_continuous(
        labels = scales::percent,
        sec.axis = sec_axis(~., labels = scales::percent)
      ) +
      theme_classic() +
      labs(
        title = paste("Variance Explained by Principal Components -", input$dataType, "Data"),
        x = "Principal Component",
        y = "Proportion of Variance Explained"
      ) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Display variance explained for selected components
  output$varianceUI <- renderUI({
    req(pca_data())
    
    var_data <- pca_data()$var_data
    
    # Get variance explained by selected components
    x_comp_var <- var_data$Variance[var_data$PC == input$xComp]
    y_comp_var <- var_data$Variance[var_data$PC == input$yComp]
    
    # Format as percentages
    x_var_pct <- scales::percent(x_comp_var, accuracy = 0.1)
    y_var_pct <- scales::percent(y_comp_var, accuracy = 0.1)
    combined_var_pct <- scales::percent(x_comp_var + y_comp_var, accuracy = 0.1)
    
    # Create UI
    tagList(
      h4("Variance Explained:"),
      tags$ul(
        tags$li(paste(input$xComp, ":", x_var_pct)),
        tags$li(paste(input$yComp, ":", y_var_pct)),
        tags$li(paste("Combined:", combined_var_pct))
      )
    )
  })
  
  # Fish information
  output$fishInfo <- renderPrint({
    req(selected_fish(), pca_data())
    
    fish_id <- selected_fish()
    pca_info <- pca_data()$pca_data
    
    # Find the selected fish
    fish_data <- pca_info %>% filter(Fish_id == fish_id)
    
    if (nrow(fish_data) > 0) {
      # Display fish metadata
      cat("Fish ID:", fish_data$Fish_id, "\n")
      cat("Watershed:", fish_data$Watershed, "\n")
      
      if ("Natal_Iso" %in% names(fish_data)) {
        cat("Natal Isotope Value:", format(fish_data$Natal_Iso, digits = 5), "\n")
      }
      
      if ("Year" %in% names(fish_data)) {
        cat("Year:", fish_data$Year, "\n")
      }
      
      # Add PCA coordinates
      cat("\nPCA Coordinates:\n")
      cat(input$xComp, ":", format(fish_data[[input$xComp]], digits = 4), "\n")
      cat(input$yComp, ":", format(fish_data[[input$yComp]], digits = 4), "\n")
    } else {
      cat("No information available for Fish ID:", fish_id)
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)
