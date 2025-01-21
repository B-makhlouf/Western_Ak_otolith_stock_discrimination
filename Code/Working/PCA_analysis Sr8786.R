# Load necessary libraries
library(tidyverse)
library(here)
library(shiny)

if (T){
  
  # Define directories and their corresponding metadata files
  data_directories <- list(
    Yukon2015 = here("Data/Intermediate/Trimmed no core or marine/Yukon/LA Data"),
    Kusko2017 = here("Data/Intermediate/Trimmed no core or marine/Kusko/LA Data"), 
    Nush2014 = here("Data/Intermediate/Trimmed no core or marine/Nush/LA Data")
  )
  
  # Function to interpolate Iso values
  interpolate_data <- function(data) {
    if (nrow(data) < 2 || all(is.na(data$Iso))) {
      return(rep(NA, 1000))
    }
    
    approx(
      x = seq_along(data$Iso),
      y = data$Iso,
      xout = seq(1, nrow(data), length.out = 1000),
      method = "linear",
      rule = 2
    )$y
  }
  
  # Function to process all files in a directory and filter by Fish_id
  process_directory <- function(files, metadata) {
    files %>%
      map_df(~ {
        tryCatch({
          file_name <- basename(.x)
          
          # Read the data and select relevant columns
          ind_data <- read_csv(.x) %>% select(Iso, Sr88, Watershed, Fish_id, natal_iso)
          watershed <- ind_data$Watershed[1]
          natal_iso <- ind_data$natal_iso[1]  # Assuming this column exists in your data
          fish_id <- ind_data$Fish_id[1]
          
          # Debug message to check the assigned natal_iso
          message("Assigned natal_iso: ", natal_iso)
          
          # Interpolate Iso values
          interpolated <- interpolate_data(ind_data)
          
          # Return the result as a tibble
          tibble(
            Fish_id = fish_id,
            Watershed = watershed,
            Natal_Iso = natal_iso,  # Include Natal_Iso in the tibble
            Iso = list(interpolated)
          )
        }, error = function(e) {
          message("Error processing file: ", .x, " - ", e$message)
          tibble(Fish_id = NA, Watershed = NA, Natal_Iso = NA, Iso = list(rep(NA, 1200)))
        })
      })
  }
  
  # Process all datasets
  results_list <- map(data_directories, ~ {
    files <- list.files(.x, full.names = TRUE)
    process_directory(files)
  })
  
  # Combine results into a single tibble
  combined_results <- bind_rows(results_list, .id = "Dataset")
  
  # Debug message to check if Natal_Iso is in the final combined_results
  print(colnames(combined_results))
  print(head(combined_results))
  
  # Remove rows with all NA in the Iso column
  filtered_results <- combined_results %>%
    filter(map_lgl(Iso, ~ !all(is.na(.))))
  
  # Create a measurement array from the Iso values
  measurement_array <- do.call(cbind, filtered_results$Iso)
  measurement_array <- t(measurement_array)
  
  # Check for missing and infinite values
  if (anyNA(measurement_array) || any(is.infinite(measurement_array))) {
    measurement_array[is.infinite(measurement_array)] <- NA
    measurement_array <- apply(measurement_array, 2, function(x) {
      if (any(is.na(x))) {
        x[is.na(x)] <- mean(x, na.rm = TRUE)
      }
      return(x)
    })
  }
}



# Extract metadata
ids <- filtered_results$Fish_id
watersheds <- filtered_results$Watershed
natal_origins<- filtered_results$Natal_Iso



#############################################################################
#############################################################################
#################### PCA Exploration ########################################
#############################################################################


# Combine all of the metadata into one dataframe
metadata <- tibble(
  Fish_id = ids,
  Natal_iso = natal_origins,
  Watershed = watersheds
)

# Define a range of natal origins 
natal_origin_filtering<- c(.700,.750)

#Find the indices of the natal origins that are within the range
natal_origin_indices<- which(metadata$Natal_iso >= natal_origin_filtering[1] & metadata$Natal_iso <= natal_origin_filtering[2])

# Filter the metadata to only include the natal origins within the range
metadata_filtered<- metadata[natal_origin_indices,]

# Filter the measurement array to only include the natal origins within the range
measurement_array_filtered<- measurement_array[natal_origin_indices,]

# Add the metadata to the front of measurement_array and save as as .csv 

all_data<- cbind(metadata_filtered, measurement_array_filtered)

write.csv(all_data, "Data/Intermediate/PCA_data.csv")

# Run PCA
results <- prcomp(measurement_array_filtered, scale. = TRUE)
pca_scores <- as.data.frame(results$x)

# Combine PCA scores with metadata
pca_results <- tibble(
  PC1 = pca_scores$PC1,
  PC2 = pca_scores$PC2,
  PC3 = pca_scores$PC3,
  PC4 = pca_scores$PC4,
  Fish_id = metadata_filtered$Fish_id,
  Watershed = metadata_filtered$Watershed, 
  Natal_iso = metadata_filtered$Natal_iso
)


############################
# Define which components to visualize
pca_x <- "PC1"
pca_y <- "PC2"  

# Update ggplot figures
pca_plot <- ggplot(pca_results, aes_string(x = pca_x, y = pca_y, color = "Watershed")) +
  geom_point(size = 2, alpha = .2) +
  theme_classic() +
  labs(title = "PCA of Iso Values by Watershed",
       x = pca_x,
       y = pca_y) +
  theme(legend.title = element_blank())

pca_plot_natal_iso <- ggplot(pca_results, aes_string(x = pca_x, y = pca_y, color = "Natal_iso")) +
  geom_point(size = 2, alpha = .9) +
  theme_classic() +
  labs(title = "PCA of Iso Values by Natal Iso",
       x = pca_x,
       y = pca_y) +
  scale_color_viridis_c(option = "C") +
  theme(legend.title = element_blank())

cowplot::plot_grid(pca_plot, pca_plot_natal_iso, labels = c("A", "B"))


##############################
# How much does each component contribute to the variance? 

if (T){
# Calculate the explained variance for each component 
explained_variance <- results$sdev^2  # Eigenvalues (variance of each PC)
total_variance <- sum(explained_variance)
proportion_variance <- explained_variance / total_variance
cumulative_variance <- cumsum(proportion_variance)

# Summarize the relative contrbution of each of the top components 

variance_summary <- tibble(
  Principal_Component = paste0("PC", seq_along(proportion_variance)),
  Variance_Explained = proportion_variance,
  Cumulative_Variance = cumulative_variance
)
print(variance_summary)
}
########## determine the features loading on each of the PCAs 

if (T){
loadings <- as.data.frame(results$rotation)
loadings$Feature <- rownames(loadings)

# Identify top 10 features driving PC1
top_features_PC1 <- loadings %>%
  arrange(desc(abs(PC1))) %>%
  slice(1:10) %>%
  select(Feature, PC1)

# Identify top 10 features driving PC2
top_features_PC2 <- loadings %>%
  arrange(desc(abs(PC2))) %>%
  slice(1:10) %>%
  select(Feature, PC2)

# Identify top 10 features driving PC3
top_features_PC3 <- loadings %>%
  arrange(desc(abs(PC3))) %>%
  slice(1:10) %>%
  select(Feature, PC3)

# Print the top features for each PC
list(
  Top_10_PC1 = top_features_PC1,
  Top_10_PC2 = top_features_PC2,
  Top_10_PC3 = top_features_PC3
)
} 

# Extract the top 10 features into a vector

PC1_features<- as.numeric(top_features_PC1$Feature)
PC2_features<- as.numeric(top_features_PC2$Feature)
PC3_features<- as.numeric(top_features_PC3$Feature)

################# R Shiny exploration plot 
ui <- fluidPage(
  titlePanel("PCA Analysis Viewer"),
  sidebarLayout(
    sidebarPanel(
      helpText("Click on a point in the PCA plot to view Iso vs. Distance for that Fish ID."),
      helpText("Drag to zoom in on the PCA plot."),
      selectInput("xComp", "X Component:", choices = names(pca_results), selected = "PC1"),
      selectInput("yComp", "Y Component:", choices = names(pca_results), selected = "PC2"),
      sliderInput("natalOriginRange", "Filter Natal Origin Range:",
                  min = min(pca_results$Natal_iso), max = max(pca_results$Natal_iso),
                  value = range(pca_results$Natal_iso), step = 0.0001),
      actionButton("resetZoom", "Reset Zoom"),
      actionButton("applyFilter", "Apply Filter")
    ),
    mainPanel(
      plotOutput("pcaPlot", click = "pcaClick", brush = brushOpts(id = "pcaBrush", resetOnNew = TRUE)),
      plotOutput("isoPlot"),
      tabsetPanel(
        tabPanel("Variance Summary", tableOutput("varianceTable")),
        tabPanel("Top Features", tableOutput("topFeaturesTable"))
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive values for filtered PCA data and zoom region
  filteredPCA <- reactiveVal(pca_results)
  zoomRegion <- reactiveValues(x = NULL, y = NULL)
  
  # Apply natal origin filter
  observeEvent(input$applyFilter, {
    filtered <- pca_results %>%
      filter(Natal_iso >= input$natalOriginRange[1], Natal_iso <= input$natalOriginRange[2])
    filteredPCA(filtered)
  })
  
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
      geom_point(size = 2, alpha = 0.8) +
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
    pc_features <- c(PC1_features, PC2_features, PC3_features)
    feature_colors <- c(rep("red", length(PC1_features)), 
                        rep("green", length(PC2_features)), 
                        rep("blue", length(PC3_features)))
    
    # Plot Iso vs Distance with moving average and horizontal lines
    ggplot(isoData, aes(x = Distance, y = Iso)) +
      geom_point(alpha = 0.5) +
      geom_line(aes(y = MovingAvg), color = "blue", size = 1) +
      geom_vline(xintercept = pc_features, color = feature_colors) +
      geom_hline(yintercept = 0.7092, color = "gold") +
      theme_classic() +
      labs(title = paste("Iso vs. Distance for Fish ID:", selectedFish()),
           x = "Distance",
           y = "Iso")
  })
  
  # Render Variance Summary Table
  output$varianceTable <- renderTable({
    tibble(
      Principal_Component = paste0("PC", seq_along(proportion_variance)),
      Variance_Explained = round(proportion_variance, 3),
      Cumulative_Variance = round(cumulative_variance, 3)
    )
  })
  
  # Render Top Features Table
  output$topFeaturesTable <- renderTable({
    bind_rows(
      top_features_PC1 %>% mutate(PC = "PC1"),
      top_features_PC2 %>% mutate(PC = "PC2"),
      top_features_PC3 %>% mutate(PC = "PC3")
    )
  })
}

# Run the App
shinyApp(ui, server)
