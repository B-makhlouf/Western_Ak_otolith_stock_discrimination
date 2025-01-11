# Load necessary libraries
library(tidyverse)
library(here)
# Function to process all files in a directory and filter by Fish_id
process_directory <- function(files, metadata) {
  files %>%
    map_df(~ {
      tryCatch({
        # Extract Fish_id from the filename (assuming the Fish_id is part of the filename)
        file_name <- basename(.x)
        Fish_id_from_filename <- tools::file_path_sans_ext(file_name)
        
        # Check if the Fish_id is in the metadata
        if (Fish_id_from_filename %in% metadata$Fish_id) {
          # Read individual file
          ind_data <- read_csv(.x) %>% select(Iso, Watershed, Fish_id)
          
          # Extract metadata
          watershed <- ind_data$Watershed[1]
          
          # Interpolate Iso
          interpolated <- interpolate_data(ind_data)
          
          tibble(
            Fish_id = Fish_id_from_filename,
            Watershed = watershed,
            Iso = list(interpolated)
          )
        } else {
          # If Fish_id does not match, return an empty tibble
          return(tibble(Fish_id = NA, Watershed = NA, Iso = list(rep(NA, 1000))))
        }
      }, error = function(e) {
        message("Error processing file: ", .x, " - ", e$message)
        return(tibble(Fish_id = NA, Watershed = NA, Iso = list(rep(NA, 1000))))
      })
    }, .id = "File_Index") # Add file index for debugging
}

# List all files in each directory
all_files <- list(
  Nush2014 = list.files(Nush2014_directory, full.names = TRUE),
  Yukon2015 = list.files(Yukon2015_directory, full.names = TRUE),
  Kusko2017 = list.files(Kusko2017_directory, full.names = TRUE)
)

# Process all datasets for each directory with corresponding metadata
results_list_Nush <- process_directory(all_files$Nush2014, Nush2014_metadata)
results_list_Yukon <- process_directory(all_files$Yukon2015, Yukon2015_metadata)
results_list_Kusko <- process_directory(all_files$Kusko2017, Kusko2017_metadata)

# Combine results into a single tibble
combined_results <- bind_rows(
  list(
    Nush2014 = results_list_Nush,
    Yukon2015 = results_list_Yukon,
    Kusko2017 = results_list_Kusko
  ),
  .id = "Dataset"
)

# Remove rows with all NA in the Iso column
filtered_results <- combined_results %>%
  filter(map_lgl(Iso, ~ !all(is.na(.))))











# Process all datasets
results_list <- map(all_files, process_directory)

# Combine results into a single tibble
combined_results <- bind_rows(results_list, .id = "Dataset")

# Remove rows with all NA in the Iso column
filtered_results <- combined_results %>%
  filter(map_lgl(Iso, ~ !all(is.na(.))))

# Transpose the Iso values to make each individual a row
measurement_array <- do.call(cbind, filtered_results$Iso)

# Transpose the matrix to get individuals as rows
measurement_array <- t(measurement_array)

# Check for missing and infinite values
if (anyNA(measurement_array) || any(is.infinite(measurement_array))) {
  measurement_array[is.infinite(measurement_array)] <- NA # Replace infinite values with NA
  measurement_array <- apply(measurement_array, 2, function(x) {
    if (any(is.na(x))) {
      x[is.na(x)] <- mean(x, na.rm = TRUE) # Replace NA with column mean
    }
    return(x)
  })
}

# Extract the metadata (ID and Watershed)
ids <- filtered_results$Fish_id
watersheds <- filtered_results$Watershed

# Run PCA
results <- prcomp(measurement_array, scale. = TRUE)

# Get the PCA scores (first two principal components)
pca_scores <- as.data.frame(results$x)

# Combine PCA scores with metadata
pca_results <- tibble(
  PC1 = pca_scores$PC1,
  PC2 = pca_scores$PC2,
  Fish_id = ids,
  Watershed = watersheds
)

# Plot the PCA results
ggplot(pca_results, aes(x = PC1, y = PC2, color = Watershed)) +
  geom_point(size = 1, alpha = .5) +
  theme_classic() +
  labs(title = "PCA of Iso Values by Watershed",
       x = "Principal Component 1",
       y = "Principal Component 2") +
  theme(legend.title = element_blank())


# outliers are those with a PCA score less than -5o or greater that 50 for either PCA 
outliers <- pca_results %>%
  filter(PC1 < -30 | PC1 > 30 | PC2 < -30 | PC2 > 30)

# Remove the outliers from measurment array 
outliers_index <- pca_results %>%
  filter(PC1 < -30 | PC1 > 30 | PC2 < -30 | PC2 > 30) %>%
  pull(Fish_id)

# Remove the outliers from the measurement array
filtered_measurement_array <- measurement_array[!(ids %in% outliers_index), ]

# Run PCA again without outliers
results_no_outliers <- prcomp(filtered_measurement_array, scale. = TRUE)

# Get the PCA scores (first two principal components)
pca_scores_no_outliers <- as.data.frame(results_no_outliers$x)

# Combine PCA scores with metadata
pca_results_no_outliers <- tibble(
  PC1 = pca_scores_no_outliers$PC1,
  PC2 = pca_scores_no_outliers$PC2,
  Fish_id = ids[!(ids %in% outliers_index)],
  Watershed = watersheds[!(ids %in% outliers_index)]
)

# Plot the PCA results without outliers
pca_plot<-ggplot(pca_results_no_outliers, aes(x = PC1, y = PC2, color = Watershed)) +
  geom_point(size = 1, alpha = .3) +
  theme_classic() +
  labs(title = "PCA of Iso Values by Watershed (No Outliers)",
       x = "Principal Component 1",
       y = "Principal Component 2") +
  theme(legend.title = element_blank())

pca_plot

######################################################
######### R Shiny app for interaction 
library(shiny)
library(tidyverse)

# Load your PCA data and measurement array
# Assume pca_results_no_outliers, ids, and measurement_array are preloaded

# Define UI
ui <- fluidPage(
  titlePanel("PCA Analysis Viewer"),
  sidebarLayout(
    sidebarPanel(
      helpText("Click on a point in the PCA plot to view Iso vs. Distance for that Fish ID."),
      helpText("Drag to zoom in on the PCA plot."),
      actionButton("resetZoom", "Reset Zoom")  # Add Reset Zoom button
    ),
    mainPanel(
      plotOutput("pcaPlot", 
                 click = "pcaClick", 
                 brush = brushOpts(id = "pcaBrush", resetOnNew = TRUE)),
      plotOutput("isoPlot")
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Reactive values for zoom region
  zoomRegion <- reactiveValues(x = NULL, y = NULL)
  
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
  
  # Reset zoom when the reset button is clicked
  observeEvent(input$resetZoom, {
    zoomRegion$x <- NULL
    zoomRegion$y <- NULL
  })
  
  # Render PCA Plot with zoom
  output$pcaPlot <- renderPlot({
    ggplot(pca_results_no_outliers, aes(x = PC1, y = PC2, color = Watershed)) +
      geom_point(size = 2, alpha = 0.8) +
      theme_classic() +
      labs(title = "PCA of Iso Values by Watershed (No Outliers)",
           x = "Principal Component 1",
           y = "Principal Component 2") +
      theme(legend.title = element_blank()) +
      coord_cartesian(
        xlim = zoomRegion$x,
        ylim = zoomRegion$y
      )
  })
  
  # Reactive value to store the selected Fish ID
  selectedFish <- reactiveVal()
  
  # Update selected Fish ID based on click
  observeEvent(input$pcaClick, {
    nearPoint <- nearPoints(pca_results_no_outliers, input$pcaClick, threshold = 5, maxpoints = 1)
    if (nrow(nearPoint) > 0) {
      selectedFish(nearPoint$Fish_id[1])
    }
  })
  
  # Render Iso Plot
  output$isoPlot <- renderPlot({
    req(selectedFish())
    
    # Find the index of the selected Fish ID
    fishIndex <- which(ids == selectedFish())
    
    # Retrieve Iso data
    isoData <- tibble(
      Distance = seq_along(measurement_array[fishIndex, ]),
      Iso = measurement_array[fishIndex, ]
    )
    
    # Calculate moving average
    isoData <- isoData %>%
      mutate(MovingAvg = zoo::rollapply(Iso, width = 60, FUN = mean, fill = NA, align = "center"))
    
    # Plot Iso vs Distance with moving average
    ggplot(isoData, aes(x = Distance, y = Iso)) +
      geom_point(alpha = 0.5) +
      geom_line(aes(y = MovingAvg), color = "blue", size = 1) +
      theme_classic() +
      labs(title = paste("Iso vs. Distance for Fish ID:", selectedFish()),
           x = "Distance",
           y = "Iso")
  })
}

# Run the App
shinyApp(ui, server)
