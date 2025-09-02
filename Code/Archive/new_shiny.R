library(shiny)
library(tidyverse)
library(ggplot2)

# Function to find RAW data file
find_raw_file <- function() {
  # Same search directories as GAM
  base_dirs <- c(
    ".", 
    "data", 
    "Data",
    "./data", 
    "./Data",
    "../data", 
    "../Data",
    "./data/LA_Data/Preprocessed_ts_matrices",
    "../data/LA_Data/Preprocessed_ts_matrices",
    "data/LA_Data/Preprocessed_ts_matrices",
    "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/data/LA_Data/Preprocessed_ts_matrices",
    "Data/Processed/Preprocessed_ts_matrices",
    "./Data/Processed/Preprocessed_ts_matrices",
    "../Data/Processed/Preprocessed_ts_matrices"
  )
  
  # RAW file patterns
  file_patterns <- c(
    "NatalToMarine_Processed_RAW\\.csv$",
    "Processed_Core_Fw_RAW\\.csv$",
    ".*_RAW\\.csv$"
  )
  
  for (base in base_dirs) {
    if (dir.exists(base)) {
      # Try each file pattern
      for (pattern in file_patterns) {
        potential_files <- list.files(path = base, pattern = pattern, 
                                      recursive = TRUE, full.names = TRUE)
        if (length(potential_files) > 0) {
          return(potential_files[1])  # Return the first match
        }
      }
    }
  }
  
  return(NULL)  # Return NULL if no matching file is found
}
find_gam_file <- function() {
  # Print current working directory for debugging
  cat("Current working directory:", getwd(), "\n")
  
  # Common locations for the GAM data file
  base_dirs <- c(
    ".", 
    "data", 
    "Data",
    "./data", 
    "./Data",
    "../data", 
    "../Data",
    "./data/LA_Data/Preprocessed_ts_matrices",
    "../data/LA_Data/Preprocessed_ts_matrices",
    "data/LA_Data/Preprocessed_ts_matrices",
    "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/data/LA_Data/Preprocessed_ts_matrices",
    # Additional patterns from your project
    "Data/Processed/Preprocessed_ts_matrices",
    "./Data/Processed/Preprocessed_ts_matrices",
    "../Data/Processed/Preprocessed_ts_matrices"
  )
  
  # Also try different filename patterns
  file_patterns <- c(
    "NatalToMarine_Processed_GAM\\.csv$",
    "Processed_Core_Fw_GAM\\.csv$",
    ".*_GAM\\.csv$"
  )
  
  cat("Searching for GAM file in the following directories:\n")
  
  for (base in base_dirs) {
    cat("Checking directory:", base, "\n")
    if (dir.exists(base)) {
      cat("  Directory exists\n")
      # Try each file pattern
      for (pattern in file_patterns) {
        potential_files <- list.files(path = base, pattern = pattern, 
                                      recursive = TRUE, full.names = TRUE)
        if (length(potential_files) > 0) {
          cat("  Found file:", potential_files[1], "\n")
          return(potential_files[1])  # Return the first match
        }
      }
    } else {
      cat("  Directory does not exist\n")
    }
  }
  
  # If nothing found, list all CSV files for debugging
  cat("\nNo GAM file found. Listing all CSV files in current directory:\n")
  all_csv <- list.files(path = ".", pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
  if (length(all_csv) > 0) {
    cat("CSV files found:\n")
    for (csv in all_csv) {
      cat("  ", csv, "\n")
    }
  } else {
    cat("  No CSV files found\n")
  }
  
  return(NULL)  # Return NULL if no matching file is found
}

# UI
ui <- fluidPage(
  titlePanel("GAM Data - PCA Viewer"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      # PCA components selection
      selectInput("xComp", "X Component:", choices = paste0("PC", 1:10), selected = "PC1"),
      selectInput("yComp", "Y Component:", choices = paste0("PC", 1:10), selected = "PC2"),
      
      # Zoom control
      actionButton("resetZoom", "Reset Zoom"),
      
      # Display variance explained
      uiOutput("varianceUI"),
      
      hr(),
      
      # Dataset information
      h4("Dataset Info"),
      verbatimTextOutput("datasetInfo"),
      
      hr(),
      
      helpText("Click on a point in the PCA plot to view the corresponding timeseries data."),
      helpText("Use brush (click and drag) to zoom into regions of the PCA plot.")
    ),
    
    mainPanel(
      width = 9,
      
      # PCA Plot
      plotOutput("pcaPlot", 
                 click = "pcaClick", 
                 brush = brushOpts(id = "pcaBrush", resetOnNew = TRUE),
                 height = "500px"),
      
      # Timeseries Plot
      plotOutput("timeseriesPlot", height = "350px"),
      
      # Fish Information
      verbatimTextOutput("fishInfo")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive values to store data
  gam_data <- reactiveVal(NULL)
  pca_results <- reactiveVal(NULL)
  selected_fish <- reactiveVal(NULL)
  
  # Reactive value for zoom regions
  zoom_region <- reactiveValues(x = NULL, y = NULL)
  
  # Reactive values to store RAW data as well
  raw_data <- reactiveVal(NULL)
  
  # Load GAM data and RAW data on startup
  observe({
    # Find the GAM data file
    gam_path <- find_gam_file()
    
    # Also find RAW data file
    raw_path <- find_raw_file()
    
    if (!is.null(gam_path)) {
      tryCatch({
        # Load GAM data
        gam_data_df <- read.csv(gam_path)
        
        # Check if data is valid
        if (nrow(gam_data_df) == 0 || ncol(gam_data_df) < 5) {
          showNotification("GAM data file is empty or invalid", type = "error")
          return()
        }
        
        # Store the GAM data
        gam_data(gam_data_df)
        
        # Load RAW data if available
        if (!is.null(raw_path)) {
          raw_data_df <- read.csv(raw_path)
          raw_data(raw_data_df)
          showNotification("GAM and RAW data loaded successfully!", type = "message")
        } else {
          showNotification("GAM data loaded (RAW data not found)", type = "message")
        }
        
        # Compute PCA on GAM data
        computed_pca <- compute_pca_from_data(gam_data_df)
        if (!is.null(computed_pca)) {
          pca_results(computed_pca)
          
          # Update component choices based on available PCs
          pc_columns <- paste0("PC", 1:min(10, ncol(computed_pca$pca_scores)))
          updateSelectInput(session, "xComp", 
                            choices = pc_columns,
                            selected = "PC1")
          updateSelectInput(session, "yComp", 
                            choices = pc_columns,
                            selected = "PC2")
        } else {
          showNotification("Could not compute PCA from GAM data", type = "error")
        }
      }, error = function(e) {
        showNotification(paste("Error loading data:", e$message), type = "error")
      })
    } else {
      showNotification("Could not find GAM data file (NatalToMarine_Processed_GAM.csv)", type = "error")
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
    req(pca_results())
    
    pca_data <- pca_results()$pca_data
    
    # Find nearest point
    click <- input$pcaClick
    nearPoints <- nearPoints(pca_data, click, threshold = 10, maxpoints = 1)
    
    if (nrow(nearPoints) > 0) {
      if ("Fish_id" %in% names(nearPoints)) {
        selected_fish(nearPoints$Fish_id[1])
      }
    }
  })
  
  # PCA plot
  output$pcaPlot <- renderPlot({
    req(pca_results())
    
    pca_data <- pca_results()$pca_data
    
    # Check if data is valid
    if (is.null(pca_data) || nrow(pca_data) == 0 || 
        !all(c(input$xComp, input$yComp, "Watershed") %in% names(pca_data))) {
      return(
        ggplot() +
          annotate("text", x = 0.5, y = 0.5, 
                   label = "No valid PCA data available",
                   size = 6) +
          theme_void() +
          xlim(0, 1) + ylim(0, 1)
      )
    }
    
    # Standardize watershed names
    pca_data$Watershed <- as.character(pca_data$Watershed)
    pca_data$Watershed <- case_when(
      pca_data$Watershed %in% c("KK", "Kusko") ~ "Kuskokwim",
      pca_data$Watershed %in% c("NK", "Nush") ~ "Nushagak", 
      pca_data$Watershed %in% c("YK", "Yukon") ~ "Yukon",
      TRUE ~ pca_data$Watershed
    )
    
    # Define watershed colors
    watershed_colors <- c(
      "Kuskokwim" = "firebrick", 
      "Nushagak" = "darkgreen", 
      "Yukon" = "dodgerblue"
    )
    
    # Create PCA plot
    p <- ggplot(pca_data, aes_string(x = input$xComp, y = input$yComp, color = "Watershed")) +
      geom_point(size = 3, alpha = 0.7) +
      theme_classic(base_size = 14) +
      labs(title = "PCA of GAM-Smoothed Otolith Data",
           subtitle = paste("Showing", input$xComp, "vs", input$yComp),
           x = input$xComp, 
           y = input$yComp) +
      coord_cartesian(xlim = zoom_region$x, ylim = zoom_region$y) +
      scale_color_manual(values = watershed_colors) +
      theme(legend.position = "bottom",
            plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5))
    
    # Highlight selected fish if any
    if (!is.null(selected_fish())) {
      selected_data <- pca_data[pca_data$Fish_id == selected_fish(), ]
      if (nrow(selected_data) > 0) {
        p <- p + 
          geom_point(data = selected_data, 
                     aes_string(x = input$xComp, y = input$yComp),
                     color = "black", size = 5, shape = 1, stroke = 2)
      }
    }
    
    p
  })
  
  # Timeseries plot
  output$timeseriesPlot <- renderPlot({
    req(selected_fish(), gam_data())
    
    gam_data_df <- gam_data()
    raw_data_df <- raw_data()  # May be NULL
    fish_id <- selected_fish()
    
    # Find the selected fish in GAM data
    gam_fish_row <- which(gam_data_df$Fish_id == fish_id)
    
    if (length(gam_fish_row) == 0) {
      return(
        ggplot() +
          annotate("text", x = 0.5, y = 0.5, 
                   label = paste("Fish ID not found:", fish_id)) +
          theme_void() +
          xlim(0, 1) + ylim(0, 1)
      )
    }
    
    # Get timeseries columns (X1, X2, X3, etc.)
    numeric_cols <- grep("^X", names(gam_data_df), value = TRUE)
    
    if (length(numeric_cols) == 0) {
      return(
        ggplot() +
          annotate("text", x = 0.5, y = 0.5, 
                   label = "No timeseries data columns found") +
          theme_void() +
          xlim(0, 1) + ylim(0, 1)
      )
    }
    
    # Extract GAM timeseries values
    gam_values <- as.numeric(gam_data_df[gam_fish_row, numeric_cols])
    
    # Check for all NA values
    if (all(is.na(gam_values))) {
      return(
        ggplot() +
          annotate("text", x = 0.5, y = 0.5, 
                   label = paste("No valid timeseries data for Fish ID:", fish_id)) +
          theme_void() +
          xlim(0, 1) + ylim(0, 1)
      )
    }
    
    # Create base plot data with GAM values
    plot_data <- data.frame(
      Time_Point = 1:length(gam_values),
      GAM_Values = gam_values
    )
    
    # Get metadata
    watershed <- case_when(
      gam_data_df$Watershed[gam_fish_row] %in% c("KK", "Kusko") ~ "Kuskokwim",
      gam_data_df$Watershed[gam_fish_row] %in% c("NK", "Nush") ~ "Nushagak", 
      gam_data_df$Watershed[gam_fish_row] %in% c("YK", "Yukon") ~ "Yukon",
      TRUE ~ as.character(gam_data_df$Watershed[gam_fish_row])
    )
    
    # Define watershed colors (same as PCA plot)
    watershed_colors <- c(
      "Kuskokwim" = "firebrick", 
      "Nushagak" = "darkgreen", 
      "Yukon" = "dodgerblue"
    )
    
    # Get the color for this watershed
    line_color <- watershed_colors[[watershed]]
    if (is.null(line_color)) line_color <- "steelblue"
    
    # Start building the plot
    p <- ggplot(plot_data, aes(x = Time_Point))
    
    # Add RAW data points in background if available
    if (!is.null(raw_data_df)) {
      # Find the same fish in RAW data
      raw_fish_row <- which(raw_data_df$Fish_id == fish_id)
      
      if (length(raw_fish_row) > 0) {
        # Extract RAW timeseries values
        raw_numeric_cols <- grep("^X", names(raw_data_df), value = TRUE)
        if (length(raw_numeric_cols) > 0) {
          raw_values <- as.numeric(raw_data_df[raw_fish_row, raw_numeric_cols])
          
          # Only add if we have valid RAW data
          if (!all(is.na(raw_values))) {
            # Adjust length if different between GAM and RAW
            min_length <- min(length(gam_values), length(raw_values))
            raw_plot_data <- data.frame(
              Time_Point = 1:min_length,
              RAW_Values = raw_values[1:min_length]
            )
            
            # Add RAW points in gray background
            p <- p + geom_point(data = raw_plot_data, 
                                aes(x = Time_Point, y = RAW_Values), 
                                color = "gray70", size = 1, alpha = 0.6)
          }
        }
      }
    }
    
    # Add GAM line on top
    p <- p + 
      geom_line(aes(y = GAM_Values), color = line_color, size = 2, alpha = 0.9) +
      theme_classic(base_size = 12) +
      labs(
        title = paste0("Fish ID: ", fish_id, " (", watershed, ")"),
        subtitle = "Gray points = RAW data, Colored line = GAM smoothed",
        x = "Time Point (Micron Position)",
        y = "Sr87/86 Ratio"
      ) +
      theme(plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5)) +
      ylim(0.702, 0.716)
    
    # Add additional metadata if available
    if ("Year" %in% names(gam_data_df) && !is.na(gam_data_df$Year[gam_fish_row])) {
      year_text <- paste("Year:", gam_data_df$Year[gam_fish_row])
      p <- p + labs(caption = year_text)
    }
    
    if ("Natal_Iso" %in% names(gam_data_df) && !is.na(gam_data_df$Natal_Iso[gam_fish_row])) {
      natal_text <- paste("Natal Isotope:", round(gam_data_df$Natal_Iso[gam_fish_row], 4))
      current_caption <- p$labels$caption
      if (is.null(current_caption)) {
        p <- p + labs(caption = natal_text)
      } else {
        p <- p + labs(caption = paste(current_caption, "|", natal_text))
      }
    }
    
    p
  })
  
  # Display variance explained
  output$varianceUI <- renderUI({
    req(pca_results())
    
    var_explained <- pca_results()$variance_explained
    
    # Get variance for selected components
    x_idx <- as.numeric(gsub("PC", "", input$xComp))
    y_idx <- as.numeric(gsub("PC", "", input$yComp))
    
    x_var_pct <- scales::percent(var_explained[x_idx], accuracy = 0.1)
    y_var_pct <- scales::percent(var_explained[y_idx], accuracy = 0.1)
    combined_var_pct <- scales::percent(var_explained[x_idx] + var_explained[y_idx], accuracy = 0.1)
    
    tagList(
      h4("Variance Explained:"),
      tags$ul(
        tags$li(paste(input$xComp, ":", x_var_pct)),
        tags$li(paste(input$yComp, ":", y_var_pct)),
        tags$li(paste("Combined:", combined_var_pct))
      )
    )
  })
  
  # Dataset information
  output$datasetInfo <- renderPrint({
    req(gam_data())
    
    data <- gam_data()
    
    cat("Dataset: GAM-Smoothed Otolith Data\n")
    cat("Total samples:", nrow(data), "\n")
    
    # Count timeseries features
    feature_cols <- grep("^X", names(data), value = TRUE)
    cat("Timeseries length:", length(feature_cols), "\n\n")
    
    # Watershed distribution
    if ("Watershed" %in% names(data)) {
      watershed_table <- table(data$Watershed)
      cat("Watershed distribution:\n")
      for (i in 1:length(watershed_table)) {
        cat(" ", names(watershed_table)[i], ":", watershed_table[i], "\n")
      }
    }
  })
  
  # Fish information
  output$fishInfo <- renderPrint({
    req(selected_fish(), pca_results(), gam_data())
    
    fish_id <- selected_fish()
    pca_data <- pca_results()$pca_data
    gam_data_df <- gam_data()
    
    # Find the fish in both datasets
    fish_pca <- pca_data[pca_data$Fish_id == fish_id, ]
    fish_gam <- gam_data_df[gam_data_df$Fish_id == fish_id, ]
    
    if (nrow(fish_pca) > 0) {
      cat("=== SELECTED FISH INFORMATION ===\n")
      cat("Fish ID:", fish_id, "\n")
      cat("Watershed:", fish_pca$Watershed[1], "\n")
      
      # Show metadata from GAM data
      if (nrow(fish_gam) > 0) {
        if ("Natal_Iso" %in% names(fish_gam) && !is.na(fish_gam$Natal_Iso[1])) {
          cat("Natal Isotope:", format(fish_gam$Natal_Iso[1], digits = 6), "\n")
        }
        
        if ("Year" %in% names(fish_gam) && !is.na(fish_gam$Year[1])) {
          cat("Year:", fish_gam$Year[1], "\n")
        }
      }
      
      # PCA coordinates
      cat("\nPCA Coordinates (from GAM data):\n")
      cat(input$xComp, ":", format(fish_pca[[input$xComp]][1], digits = 4), "\n")
      cat(input$yComp, ":", format(fish_pca[[input$yComp]][1], digits = 4), "\n")
      
      # Show data availability
      cat("\nData Availability:\n")
      cat("GAM data: Available\n")
      if (!is.null(raw_data())) {
        raw_fish <- raw_data()[raw_data()$Fish_id == fish_id, ]
        if (nrow(raw_fish) > 0) {
          cat("RAW data: Available\n")
        } else {
          cat("RAW data: Not available for this fish\n")
        }
      } else {
        cat("RAW data: Not loaded\n")
      }
      
    } else {
      cat("No information available for Fish ID:", fish_id)
    }
  })
  
  # Compute PCA from GAM data
  compute_pca_from_data <- function(data) {
    tryCatch({
      # Extract metadata and timeseries columns
      meta_cols <- grep("^X", names(data), value = TRUE, invert = TRUE)
      numeric_cols <- grep("^X", names(data), value = TRUE)
      
      if (length(numeric_cols) == 0) {
        return(NULL)
      }
      
      # Handle missing values
      numeric_data <- data[, numeric_cols]
      for (col in 1:ncol(numeric_data)) {
        na_idx <- is.na(numeric_data[, col])
        if (any(na_idx)) {
          numeric_data[na_idx, col] <- mean(numeric_data[, col], na.rm = TRUE)
        }
      }
      
      # Compute PCA
      pca_result <- prcomp(numeric_data, scale. = TRUE)
      
      # Create PCA data with metadata
      pca_scores <- as.data.frame(pca_result$x)
      
      # Add metadata
      required_meta <- c("Fish_id", "Watershed")
      available_meta <- intersect(required_meta, meta_cols)
      
      if (length(available_meta) > 0) {
        pca_data <- cbind(pca_scores, data[, available_meta, drop = FALSE])
      } else {
        pca_data <- pca_scores
        pca_data$Fish_id <- paste0("Fish_", 1:nrow(pca_data))
        pca_data$Watershed <- "Unknown"
      }
      
      # Calculate explained variance
      explained_var <- pca_result$sdev^2 / sum(pca_result$sdev^2)
      
      return(list(
        pca_data = pca_data,
        pca_scores = pca_scores,
        variance_explained = explained_var,
        loadings = pca_result$rotation
      ))
    }, error = function(e) {
      message("Error computing PCA: ", e$message)
      return(NULL)
    })
  }
}

# Run the app
shinyApp(ui = ui, server = server)