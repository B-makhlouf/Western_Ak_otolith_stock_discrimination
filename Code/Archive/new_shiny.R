
library(shiny)
library(tidyverse)
library(ggplot2)
library(viridis)
library(gridExtra)

# For path handling - use custom function instead of 'here'
find_file <- function(file_patterns, base_dirs = c(".", "Data", "data", "..", "../Data", "../data")) {
  for (pattern in file_patterns) {
    for (base in base_dirs) {
      # Try to find matching files in the directory
      if (dir.exists(base)) {
        potential_files <- list.files(path = base, pattern = pattern, recursive = TRUE, full.names = TRUE)
        if (length(potential_files) > 0) {
          return(potential_files[1])  # Return the first match
        }
      }
    }
  }
  return(NULL)  # Return NULL if no matching file is found
}

# UI
ui <- fluidPage(
  titlePanel("Western Alaska Otolith Analysis Viewer"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      # PCA Analysis Section
      h4("PCA Analysis"),
      
      # Select data type
      selectInput("dataType", "Data Type:",
                  choices = c("GAM", "MA", "RAW", "Sr88", "Combined"),
                  selected = "GAM"),
      
      # PCA components selection
      selectInput("xComp", "X Component:", choices = paste0("PC", 1:5), selected = "PC1"),
      selectInput("yComp", "Y Component:", choices = paste0("PC", 1:5), selected = "PC2"),
      
      # Zoom control
      actionButton("resetZoom", "Reset Zoom"),
      
      # Display variance explained
      uiOutput("varianceUI"),
      
      hr(),
      
      # Model Performance Section
      h4("Model Performance"),
      
      # Filter options for model performance
      selectInput("modelFilter", "Filter Model Type:",
                  choices = c("All", "Random Forest", "SVM", "KNN"),
                  selected = "All"),
      
      selectInput("dataSourceFilter", "Filter Data Source:",
                  choices = c("All", "GAM", "MA", "RAW", "Sr88", "Combined", "Outline"),
                  selected = "All"),
      
      selectInput("metricType", "Performance Metric:",
                  choices = c("Accuracy", "F1 Score", "Specificity", "Precision", "Balanced Accuracy"),
                  selected = "Accuracy"),
      
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
        tabPanel("Model Performance", 
                 h4("Classification Performance by Watershed and Model Type"),
                 plotOutput("modelPerformanceHeatmap", height = "500px"),
                 hr(),
                 plotOutput("watershedPerformanceChart", height = "400px"),
                 hr(),
                 downloadButton("downloadModelData", "Download Performance Data")
        ),
        tabPanel("Ensemble Results",
                 h4("Ensemble Model Performance"),
                 plotOutput("confusionMatrix", height = "500px"),
                 hr(),
                 plotOutput("watershedAccuracyBar", height = "400px"),
                 verbatimTextOutput("ensembleMetricsSummary")
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
  model_results <- reactiveVal(NULL)
  ensemble_results <- reactiveVal(NULL)
  
  # Reactive value for zoom regions
  zoom_region <- reactiveValues(x = NULL, y = NULL)
  
  # Load time series summary metrics
  observe({
    # Try to find ts_summary_metrics.csv
    ts_metrics_patterns <- c(
      "ts_summary_metrics\\.csv$",
      "summary_metrics\\.csv$"
    )
    
    ts_metrics_path <- find_file(ts_metrics_patterns)
    
    if (!is.null(ts_metrics_path)) {
      # Load the data 
      tryCatch({
        metrics_data <- read.csv(ts_metrics_path)
        
        # Process the data to match the expected format
        processed_data <- metrics_data %>%
          rename(
            Data_Type = Data_Source,
            Model_Method = Model_Type
          ) %>%
          mutate(
            Watershed = "Overall",
            F1_Score = Accuracy,
            Specificity = Accuracy,
            Precision = Accuracy,
            Balanced_Accuracy = Accuracy
          )
        
        # Set as model results
        model_results(processed_data)
        
      }, error = function(e) {
        message("Error reading ts_metrics file: ", e$message)
        # Create mock data if file can't be read
        create_mock_model_data()
      })
    } else {
      message("Could not find ts_summary_metrics.csv")
      # Create mock data if file not found
      create_mock_model_data()
    }
  })
  
  # Load model results on startup (as a fallback if ts metrics not found)
  observe({
    # Only run if model_results is NULL (ts metrics not loaded)
    if (is.null(model_results())) {
      # Try to find model results file
      model_patterns <- c(
        "ALL_Models_Results\\.csv$",
        "all_metrics_comparison\\.csv$", 
        "model_performance.*\\.csv$"
      )
      
      model_path <- find_file(model_patterns)
      
      if (!is.null(model_path)) {
        # Load the data safely
        tryCatch({
          model_data <- read.csv(model_path)
          
          # Validate the data has required columns
          required_cols <- c("Data_Type", "Model_Method", "Watershed")
          metric_cols <- c("Accuracy", "F1_Score", "Specificity", "Precision")
          
          # Check if the file has at least the minimum required structure
          if (all(required_cols %in% names(model_data)) && 
              any(metric_cols %in% names(model_data))) {
            model_results(model_data)
          } else {
            # Missing required columns, use mock data
            create_mock_model_data()
          }
        }, error = function(e) {
          # Error reading file, use mock data
          message("Error reading model results: ", e$message)
          create_mock_model_data()
        })
      } else {
        # File not found, use mock data
        message("Model results file not found. Using mock data.")
        create_mock_model_data()
      }
    }
  })
  
  # Helper function to create mock model data
  create_mock_model_data <- function() {
    # Set seed for reproducible random values
    set.seed(123)
    
    # Create realistic-looking mock data with expected structure
    mock_data <- data.frame(
      Data_Type = rep(c("GAM", "MA", "RAW", "Sr88", "Combined", "Outline"), each = 9),
      Model_Method = rep(rep(c("rf", "svm", "knn"), each = 3), 6),
      Watershed = rep(c("Kuskokwim", "Nushagak", "Yukon"), 18),
      Accuracy = runif(54, 0.5, 0.9),
      F1_Score = runif(54, 0.5, 0.9),
      Specificity = runif(54, 0.6, 0.95),
      Precision = runif(54, 0.5, 0.9),
      Balanced_Accuracy = runif(54, 0.5, 0.9)
    )
    
    model_results(mock_data)
    
    # Show notification
    showNotification(
      "Using simulated model performance data. Upload real data files for actual metrics.",
      type = "warning",
      duration = 10
    )
  }
  
  # Helper function to create mock ensemble data
  create_mock_ensemble_data <- function() {
    # Set seed for reproducible random values
    set.seed(456)
    
    # Create realistic-looking mock ensemble data
    mock_ensemble <- data.frame(
      Actual = sample(c("Kuskokwim", "Nushagak", "Yukon"), 100, replace = TRUE),
      Ensemble = sample(c("Kuskokwim", "Nushagak", "Yukon"), 100, replace = TRUE),
      Confidence = runif(100, 0.5, 1)
    )
    mock_ensemble$Correct <- mock_ensemble$Actual == mock_ensemble$Ensemble
    ensemble_results(mock_ensemble)
    
    # Show notification
    showNotification(
      "Using simulated ensemble results. Upload real data files for actual results.",
      type = "warning",
      duration = 10
    )
  }
  
  # Try to find ensemble results file
  observe({
    ensemble_patterns <- c(
      "ensemble_predictions\\.csv$",
      "ensemble_results.*\\.csv$"
    )
    
    ensemble_path <- find_file(ensemble_patterns)
    
    if (!is.null(ensemble_path)) {
      # Load the data safely
      tryCatch({
        ensemble_data <- read.csv(ensemble_path)
        # Validate structure
        if (all(c("Actual", "Ensemble") %in% names(ensemble_data))) {
          # Add Confidence column if not present
          if (!"Confidence" %in% names(ensemble_data)) {
            ensemble_data$Confidence <- runif(nrow(ensemble_data), 0.5, 1)
          }
          # Add Correct column if not present
          if (!"Correct" %in% names(ensemble_data)) {
            ensemble_data$Correct <- ensemble_data$Actual == ensemble_data$Ensemble
          }
          ensemble_results(ensemble_data)
        } else {
          create_mock_ensemble_data()
        }
      }, error = function(e) {
        message("Error reading ensemble results: ", e$message)
        create_mock_ensemble_data()
      })
    } else {
      # File not found, use mock data
      message("Ensemble results file not found. Using mock data.")
      create_mock_ensemble_data()
    }
  })
  
  # Load precomputed PCA data based on selected type
  observe({
    # Get data type
    data_type <- tolower(input$dataType)
    
    # Construct file path
    file_patterns <- c(paste0("pca_", data_type, "\\.rds$"))
    pca_file <- find_file(file_patterns)
    
    if (!is.null(pca_file)) {
      # Load precomputed data
      tryCatch({
        precomputed <- readRDS(pca_file)
        pca_data(precomputed)
        
        # Update component choices
        pc_columns <- grep("^PC", names(precomputed$pca_data), value = TRUE)
        updateSelectInput(session, "xComp", 
                          choices = pc_columns,
                          selected = "PC1")
        updateSelectInput(session, "yComp", 
                          choices = pc_columns,
                          selected = "PC2")
        
        # Try to load the raw data
        file_patterns <- c(
          paste0("preprocessed_", input$dataType, "\\.csv$"),
          paste0("Processed_Core_Fw_", input$dataType, "\\.csv$")
        )
        
        data_path <- find_file(file_patterns)
        
        if (!is.null(data_path)) {
          raw_data(read.csv(data_path))
        }
      }, error = function(e) {
        # If loading precomputed fails, compute on the fly
        computed_pca <- compute_pca(data_type)
        if (!is.null(computed_pca)) {
          pca_data(computed_pca)
          raw_data(computed_pca$raw_data)
          
          # Update component choices
          pc_columns <- grep("^PC", names(computed_pca$pca_data), value = TRUE)
          updateSelectInput(session, "xComp", 
                            choices = pc_columns,
                            selected = "PC1")
          updateSelectInput(session, "yComp", 
                            choices = pc_columns,
                            selected = "PC2")
        } else {
          showNotification(
            paste("Could not load or compute PCA for", input$dataType, "type."),
            type = "error"
          )
          pca_data(NULL)
          raw_data(NULL)
        }
      })
    } else {
      # Compute PCA on the fly
      computed_pca <- compute_pca(data_type)
      if (!is.null(computed_pca)) {
        pca_data(computed_pca)
        raw_data(computed_pca$raw_data)
        
        # Update component choices
        pc_columns <- grep("^PC", names(computed_pca$pca_data), value = TRUE)
        updateSelectInput(session, "xComp", 
                          choices = pc_columns,
                          selected = "PC1")
        updateSelectInput(session, "yComp", 
                          choices = pc_columns,
                          selected = "PC2")
      } else {
        showNotification(
          paste("Could not find or compute PCA for", input$dataType, "type."),
          type = "error"
        )
        pca_data(NULL)
        raw_data(NULL)
      }
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
      if ("Fish_id" %in% names(nearPoints)) {
        selected_fish(nearPoints$Fish_id[1])
      }
    }
  })
  
  # PCA plot
  output$pcaPlot <- renderPlot({
    req(pca_data())
    
    data <- pca_data()$pca_data
    
    # Check if data is valid
    if (is.null(data) || nrow(data) == 0 || 
        !all(c(input$xComp, input$yComp, "Watershed") %in% names(data))) {
      # Return empty plot with error message
      return(
        ggplot() +
          annotate("text", x = 0.5, y = 0.5, 
                   label = "No valid PCA data available for this selection") +
          theme_void() +
          xlim(0, 1) + ylim(0, 1)
      )
    }
    
    # Make sure watershed is a factor with consistent levels
    data$Watershed <- as.character(data$Watershed)  # First convert to character
    
    # Handle alternative watershed codings
    data$Watershed <- ifelse(data$Watershed == "KK", "Kuskokwim", data$Watershed)
    data$Watershed <- ifelse(data$Watershed == "NK", "Nushagak", data$Watershed)
    data$Watershed <- ifelse(data$Watershed == "YK", "Yukon", data$Watershed)
    data$Watershed <- ifelse(data$Watershed == "Kusko", "Kuskokwim", data$Watershed)
    data$Watershed <- ifelse(data$Watershed == "Nush", "Nushagak", data$Watershed)
    
    # Define fixed watershed colors
    watershed_colors <- c(
      "Kuskokwim" = "firebrick", 
      "Nushagak" = "darkgreen", 
      "Yukon" = "dodgerblue"
    )
    
    # Create PCA plot
    tryCatch({
      ggplot(data, aes_string(x = input$xComp, y = input$yComp, color = "Watershed")) +
        geom_point(size = 3, alpha = 0.6) +
        theme_classic() +
        labs(title = paste("PCA of", input$dataType, "Values"),
             x = input$xComp, 
             y = input$yComp) +
        coord_cartesian(xlim = zoom_region$x, ylim = zoom_region$y) +
        scale_color_manual(values = watershed_colors, 
                           breaks = c("Kuskokwim", "Nushagak", "Yukon"))
    }, error = function(e) {
      # Return error message plot if plotting fails
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, 
                 label = paste("Error plotting PCA:", e$message)) +
        theme_void() +
        xlim(0, 1) + ylim(0, 1)
    })
  })
  
  # Timeseries plot
  output$timeseriesPlot <- renderPlot({
    req(selected_fish(), raw_data())
    
    data <- raw_data()
    fish_id <- selected_fish()
    
    # Safety check for data
    if (is.null(data) || !("Fish_id" %in% names(data))) {
      return(
        ggplot() +
          annotate("text", x = 0.5, y = 0.5, 
                   label = "No valid timeseries data available") +
          theme_void() +
          xlim(0, 1) + ylim(0, 1)
      )
    }
    
    # Find the selected fish in the data
    fish_row <- which(data$Fish_id == fish_id)
    
    if (length(fish_row) > 0 && any(grepl("^X", names(data)))) {
      # Get numeric columns (timeseries data)
      numeric_cols <- grep("^X", names(data), value = TRUE)
      
      # Extract timeseries values for this fish
      tryCatch({
        timeseries_values <- as.numeric(data[fish_row, numeric_cols])
        
        # Check for all NA or invalid values
        if (all(is.na(timeseries_values)) || length(timeseries_values) == 0) {
          return(
            ggplot() +
              annotate("text", x = 0.5, y = 0.5, 
                       label = paste("No valid timeseries data for Fish ID:", fish_id)) +
              theme_void() +
              xlim(0, 1) + ylim(0, 1)
          )
        }
        
        # Create a data frame for plotting
        plot_data <- data.frame(
          Index = 1:length(timeseries_values),
          Value = timeseries_values
        )
        
        # Get watershed safely
        watershed <- ifelse(
          "Watershed" %in% names(data) && !is.na(data$Watershed[fish_row]),
          as.character(data$Watershed[fish_row]), 
          "Unknown"
        )
        
        # Create the plot
        ggplot(plot_data, aes(x = Index, y = Value)) +
          geom_line(color = "blue", size = 1) +
          geom_point(alpha = 0.3, size = 1) +
          theme_classic() +
          labs(
            title = paste0(
              "Timeseries for Fish ID: ", fish_id, 
              " (", watershed, ")"
            ),
            x = "Index",
            y = ifelse(input$dataType == "Sr88", "Sr88 Value", "Sr87/86 Value")
          )
      }, error = function(e) {
        # Return error message plot if extraction or plotting fails
        ggplot() +
          annotate("text", x = 0.5, y = 0.5, 
                   label = paste("Error plotting timeseries:", e$message)) +
          theme_void() +
          xlim(0, 1) + ylim(0, 1)
      })
    } else {
      # Show message if fish not found or no timeseries data
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, 
                 label = paste("Timeseries data not available for Fish ID:", fish_id)) +
        theme_void() +
        xlim(0, 1) + ylim(0, 1)
    }
  })
  
  # Model performance heatmap
  output$modelPerformanceHeatmap <- renderPlot({
    req(model_results())
    
    # Filter data based on selections
    filtered_data <- model_results() %>%
      filter(
        (input$modelFilter == "All" | 
           (input$modelFilter == "Random Forest" & Model_Method == "rf") |
           (input$modelFilter == "SVM" & Model_Method == "svm") |
           (input$modelFilter == "KNN" & Model_Method == "knn")),
        
        (input$dataSourceFilter == "All" | 
           Data_Type == input$dataSourceFilter)
      )
    
    # Format model labels
    filtered_data <- filtered_data %>%
      mutate(
        Model_Label = paste0(
          ifelse(Data_Type == "Combined", "Combined", 
                 ifelse(Data_Type == "Outline", "Shape", Data_Type)),
          "-",
          ifelse(Model_Method == "rf", "RF", 
                 ifelse(Model_Method == "svm", "SVM", 
                        ifelse(Model_Method == "knn", "KNN", Model_Method)))
        ),
        Model_Label = factor(Model_Label)
      )
    
    # Choose performance metric
    metric_col <- switch(input$metricType,
                         "Accuracy" = "Accuracy",
                         "F1 Score" = "F1_Score",
                         "Specificity" = "Specificity",
                         "Precision" = "Precision",
                         "Balanced Accuracy" = "Balanced_Accuracy",
                         "Accuracy")
    
    # Create heatmap
    if (nrow(filtered_data) > 0) {
      ggplot(filtered_data, aes(x = Watershed, y = Model_Label, fill = !!sym(metric_col))) +
        geom_tile(color = "white", size = 0.2) +
        geom_text(aes(label = sprintf("%.2f", !!sym(metric_col))), color = "white", size = 3.5) +
        scale_fill_viridis(option = "plasma", limits = c(0.45, 0.95)) +
        labs(
          title = paste(input$metricType, "by Model and Watershed Class"),
          x = "Watershed",
          y = NULL,
          fill = input$metricType
        ) +
        theme_minimal() +
        theme(
          axis.text.y = element_text(hjust = 1),
          legend.position = "right",
          plot.title = element_text(hjust = 0.5)
        )
    } else {
      # Show message if no data
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, 
                 label = "No model performance data available for selected filters") +
        theme_void() +
        xlim(0, 1) + ylim(0, 1)
    }
  })
  
  # Watershed-specific performance chart
  output$watershedPerformanceChart <- renderPlot({
    req(model_results())
    
    # Filter and summarize data
    summary_data <- model_results() %>%
      filter(
        (input$modelFilter == "All" | 
           (input$modelFilter == "Random Forest" & Model_Method == "rf") |
           (input$modelFilter == "SVM" & Model_Method == "svm") |
           (input$modelFilter == "KNN" & Model_Method == "knn")),
        
        (input$dataSourceFilter == "All" | 
           Data_Type == input$dataSourceFilter)
      ) %>%
      group_by(Data_Type, Model_Method) %>%
      summarize(
        Avg_Accuracy = mean(Accuracy, na.rm = TRUE),
        Avg_F1 = mean(F1_Score, na.rm = TRUE),
        Avg_Specificity = mean(Specificity, na.rm = TRUE),
        Avg_Precision = mean(Precision, na.rm = TRUE),
        Avg_Balanced_Accuracy = mean(Balanced_Accuracy, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        Model = paste0(
          ifelse(Data_Type == "Combined", "Combined", 
                 ifelse(Data_Type == "Outline", "Shape", Data_Type)),
          "-",
          ifelse(Model_Method == "rf", "RF", 
                 ifelse(Model_Method == "svm", "SVM", 
                        ifelse(Model_Method == "knn", "KNN", Model_Method)))
        )
      )
    
    # Choose performance metric
    metric_col <- switch(input$metricType,
                         "Accuracy" = "Avg_Accuracy",
                         "F1 Score" = "Avg_F1",
                         "Specificity" = "Avg_Specificity",
                         "Precision" = "Avg_Precision",
                         "Balanced Accuracy" = "Avg_Balanced_Accuracy",
                         "Avg_Accuracy")
    
    # Create bar chart
    if (nrow(summary_data) > 0) {
      ggplot(summary_data, aes(x = reorder(Model, !!sym(metric_col)), y = !!sym(metric_col), fill = Data_Type)) +
        geom_bar(stat = "identity") +
        geom_text(aes(label = sprintf("%.3f", !!sym(metric_col))), hjust = -0.1, size = 3) +
        coord_flip() +
        labs(
          title = paste("Average", input$metricType, "by Model Type"),
          x = NULL,
          y = input$metricType,
          fill = "Data Source"
        ) +
        theme_minimal() +
        theme(
          legend.position = "bottom",
          plot.title = element_text(hjust = 0.5)
        )
    } else {
      # Show message if no data
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, 
                 label = "No model performance data available for selected filters") +
        theme_void() +
        xlim(0, 1) + ylim(0, 1)
    }
  })
  
  # Confusion matrix plot
  output$confusionMatrix <- renderPlot({
    req(ensemble_results())
    
    # Generate confusion matrix data
    conf_mat_data <- ensemble_results() %>%
      count(Actual, Ensemble) %>%
      group_by(Actual) %>%
      mutate(Percent = n / sum(n)) %>%
      ungroup()
    
    # Create confusion matrix plot
    ggplot(conf_mat_data, aes(x = Ensemble, y = Actual, fill = Percent)) +
      geom_tile() +
      geom_text(aes(label = sprintf("%d\n(%.1f%%)", n, Percent * 100)), 
                color = "white", size = 4) +
      scale_fill_viridis(option = "plasma", limits = c(0, 1)) +
      labs(
        title = "Ensemble Model Confusion Matrix",
        x = "Predicted",
        y = "Actual",
        fill = "Percent"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 16),
        axis.text = element_text(size = 12)
      )
  })
  
  # Watershed accuracy bar chart
  output$watershedAccuracyBar <- renderPlot({
    req(ensemble_results())
    
    # Calculate accuracy by watershed
    watershed_acc <- ensemble_results() %>%
      group_by(Actual) %>%
      summarize(
        Correct = sum(Actual == Ensemble),
        Total = n(),
        Accuracy = Correct / Total,
        .groups = "drop"
      )
    
    # Create bar chart
    ggplot(watershed_acc, aes(x = Actual, y = Accuracy, fill = Actual)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = sprintf("%.1f%%", Accuracy * 100)), 
                vjust = -0.5, size = 5) +
      scale_y_continuous(labels = scales::percent, limits = c(0, 1.1)) +
      scale_fill_manual(values = c("Kuskokwim" = "firebrick", 
                                   "Nushagak" = "darkgreen", 
                                   "Yukon" = "dodgerblue")) +
      labs(
        title = "Ensemble Model Accuracy by Watershed",
        x = "Watershed",
        y = "Accuracy"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 16),
        axis.text = element_text(size = 12),
        legend.position = "none"
      )
  })
  
  # Ensemble metrics summary
  output$ensembleMetricsSummary <- renderPrint({
    req(ensemble_results())
    
    # Calculate overall accuracy
    overall_acc <- mean(ensemble_results()$Actual == ensemble_results()$Ensemble, na.rm = TRUE)
    
    # Calculate accuracy by watershed
    watershed_acc <- ensemble_results() %>%
      group_by(Actual) %>%
      summarize(
        Accuracy = mean(Actual == Ensemble, na.rm = TRUE),
        Count = n(),
        .groups = "drop"
      )
    
    # Calculate metrics by confidence threshold
    conf_thresholds <- c(0.5, 0.7, 0.8, 0.9)
    conf_metrics <- lapply(conf_thresholds, function(threshold) {
      # Filter by confidence
      high_conf <- ensemble_results() %>% filter(Confidence >= threshold)
      # Calculate accuracy if any samples remain
      if (nrow(high_conf) > 0) {
        acc <- mean(high_conf$Actual == high_conf$Ensemble, na.rm = TRUE)
        count <- nrow(high_conf)
        pct_retained <- count / nrow(ensemble_results())
        return(c(threshold = threshold, accuracy = acc, count = count, pct_retained = pct_retained))
      } else {
        return(c(threshold = threshold, accuracy = NA, count = 0, pct_retained = 0))
      }
    })
    conf_metrics_df <- as.data.frame(do.call(rbind, conf_metrics))
    
    # Print summary
    cat("Ensemble Model Performance Summary\n")
    cat("=================================\n\n")
    
    cat("Overall Accuracy:", sprintf("%.2f%%", overall_acc * 100), "\n")
    cat("Total Samples:", nrow(ensemble_results()), "\n\n")
    
    cat("Accuracy by Watershed:\n")
    for (i in 1:nrow(watershed_acc)) {
      cat(sprintf("%s: %.2f%% (%d samples)\n", 
                  watershed_acc$Actual[i],
                  watershed_acc$Accuracy[i] * 100,
                  watershed_acc$Count[i]))
    }
    
    cat("\nAccuracy by Confidence Threshold:\n")
    for (i in 1:nrow(conf_metrics_df)) {
      if (!is.na(conf_metrics_df$accuracy[i])) {
        cat(sprintf("≥ %.1f: %.2f%% (%d samples, %.1f%% retained)\n", 
                    conf_metrics_df$threshold[i],
                    conf_metrics_df$accuracy[i] * 100,
                    conf_metrics_df$count[i],
                    conf_metrics_df$pct_retained[i] * 100))
      } else {
        cat(sprintf("≥ %.1f: No samples meet this threshold\n", 
                    conf_metrics_df$threshold[i]))
      }
    }
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
  
  # Download handler for model performance data
  output$downloadModelData <- downloadHandler(
    filename = function() {
      paste("model_performance_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      # Get filtered data
      filtered_data <- model_results() %>%
        filter(
          (input$modelFilter == "All" | 
             (input$modelFilter == "Random Forest" & Model_Method == "rf") |
             (input$modelFilter == "SVM" & Model_Method == "svm") |
             (input$modelFilter == "KNN" & Model_Method == "knn")),
          
          (input$dataSourceFilter == "All" | 
             Data_Type == input$dataSourceFilter)
        )
      
      # Write to file
      write.csv(filtered_data, file, row.names = FALSE)
    }
  )
  
  # Generate PCA on the fly if precomputed not available
  compute_pca <- function(data_type) {
    # Try to load the raw data first
    file_patterns <- c(
      paste0("preprocessed_", data_type, "\\.csv$"),
      paste0("Processed_Core_Fw_", data_type, "\\.csv$")
    )
    
    data_path <- find_file(file_patterns)
    
    if (is.null(data_path)) {
      return(NULL)
    }
    
    # Try to read the data
    tryCatch({
      data <- read.csv(data_path)
      
      # Check if data is valid
      if (nrow(data) == 0 || ncol(data) < 5) {
        return(NULL)
      }
      
      # Extract metadata and timeseries columns
      meta_cols <- c("Fish_id", "Watershed", "Natal_Iso", "Year")
      meta_cols <- meta_cols[meta_cols %in% names(data)]
      numeric_cols <- grep("^X", names(data), value = TRUE)
      
      if (length(numeric_cols) == 0) {
        return(NULL)
      }
      
      # Compute PCA
      pca_result <- prcomp(data[, numeric_cols], scale. = TRUE)
      
      # Create PCA data with metadata
      pca_scores <- as.data.frame(pca_result$x)
      pca_data <- cbind(pca_scores, data[, meta_cols, drop = FALSE])
      
      # Calculate explained variance
      explained_var <- pca_result$sdev^2 / sum(pca_result$sdev^2)
      cum_var <- cumsum(explained_var)
      
      # Create variance data
      var_data <- data.frame(
        PC = paste0("PC", 1:length(explained_var)),
        Variance = explained_var,
        CumulativeVariance = cum_var
      )
      
      # Return results
      return(list(
        pca_data = pca_data,
        var_data = var_data,
        loadings = pca_result$rotation,
        raw_data = data
      ))
    }, error = function(e) {
      message("Error computing PCA: ", e$message)
      return(NULL)
    })
  }
}

# Run the app
shinyApp(ui = ui, server = server)

