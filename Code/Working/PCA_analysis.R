# Load necessary libraries
library(tidyverse)
library(here)

# Define directories with ablation TS data
Nush2014_directory <- here("Data/Intermediate/Cleaned and Trimmed/2014 Nush")
Nush2015_directory <- here("Data/Intermediate/Cleaned and Trimmed/2015 Nush")
Nush2011_directory <- here("Data/Intermediate/Cleaned and Trimmed/2011 Nush")
Yukon2015_directory <- here("Data/Intermediate/Cleaned and Trimmed/2015 Yukon")
Yukon2016_directory <- here("Data/Intermediate/Cleaned and Trimmed/2016 Yukon")
Yukon2017_directory <- here("Data/Intermediate/Cleaned and Trimmed/2017 Yukon")
Yukon2019_directory <- here("Data/Intermediate/Cleaned and Trimmed/2019 Yukon")
Yukon2021_directory <- here("Data/Intermediate/Cleaned and Trimmed/2021 Yukon")
Kusko2015_directory <- here("Data/Intermediate/Cleaned and Trimmed/2015 Kusko")
Kusko2017_directory <- here("Data/Intermediate/Cleaned and Trimmed/2017 Kusko")
Kusko2019_directory <- here("Data/Intermediate/Cleaned and Trimmed/2019 Kusko")
kusko2020_directory <- here("Data/Intermediate/Cleaned and Trimmed/2020 Kusko")
kusko2021_directory <- here("Data/Intermediate/Cleaned and Trimmed/2021 Kusko")

# List all files in each directory
all_files <- list(
  Nush2014 = list.files(Nush2014_directory, full.names = TRUE),
  Nush2015 = list.files(Nush2015_directory, full.names = TRUE),
  Nush2011 = list.files(Nush2011_directory, full.names = TRUE),
  Yukon2015 = list.files(Yukon2015_directory, full.names = TRUE),
  Yukon2016 = list.files(Yukon2016_directory, full.names = TRUE),
  Yukon2017 = list.files(Yukon2017_directory, full.names = TRUE),
  Yukon2019 = list.files(Yukon2019_directory, full.names = TRUE),
  Yukon2021 = list.files(Yukon2021_directory, full.names = TRUE),
  Kusko2015 = list.files(Kusko2015_directory, full.names = TRUE),
  Kusko2017 = list.files(Kusko2017_directory, full.names = TRUE),
  Kusko2019 = list.files(Kusko2019_directory, full.names = TRUE),
  Kusko2020 = list.files(kusko2020_directory, full.names = TRUE),
  Kusko2021 = list.files(kusko2021_directory, full.names = TRUE)
)

# Function to interpolate Iso data to 1000 points
interpolate_data <- function(data, num_points = 1700) {
  if (!"Iso" %in% names(data) || sum(!is.na(data$Iso)) < 2) {
    return(rep(NA, num_points)) # Return NA if not enough points for interpolation
  }
  
  data <- data %>%
    mutate(Point = seq(1, num_points, length.out = n()))
  
  interpolated <- tibble(
    Point = seq(1, num_points),
    Iso = approx(data$Point, data$Iso, xout = seq(1, num_points))$y
  )
  
  return(interpolated$Iso)
}

# Function to process all files in a directory
process_directory <- function(files) {
  files %>%
    map_df(~ {
      tryCatch({
        # Read individual file
        ind_data <- read_csv(.x) %>% select(Iso, Watershed, Fish_id)
        
        # Extract metadata
        id <- ind_data$Fish_id[1]
        watershed <- ind_data$Watershed[1]
        
        # Interpolate Iso
        interpolated <- interpolate_data(ind_data)
        
        tibble(
          Fish_id = id,
          Watershed = watershed,
          Iso = list(interpolated)
        )
      }, error = function(e) {
        message("Error processing file: ", .x, " - ", e$message)
        return(tibble(Fish_id = NA, Watershed = NA, Iso = list(rep(NA, 1000))))
      })
    }, .id = "File_Index") # Add file index for debugging
}

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

library(shiny)
library(tidyverse)

# Define UI
ui <- fluidPage(
  titlePanel("PCA Analysis of Iso Values by Watershed"),
  sidebarLayout(
    sidebarPanel(
      h4("Instructions:"),
      p("Click on a point in the PCA plot to display the Iso vs Distance plot for the selected Fish ID.")
    ),
    mainPanel(
      plotOutput("pcaPlot", click = "plot_click"),
      plotOutput("isoPlot")
    )
  )
)

# Define Server
server <- function(input, output) {
  # Render the PCA plot
  output$pcaPlot <- renderPlot({
    ggplot(pca_results_no_outliers, aes(x = PC1, y = PC2, color = Watershed)) +
      geom_point(size = 1, alpha = 0.5) +
      theme_classic() +
      labs(title = "PCA of Iso Values by Watershed (No Outliers)",
           x = "Principal Component 1",
           y = "Principal Component 2") +
      theme(legend.title = element_blank())
  })
  
  # Plot Iso vs Distance for selected Fish ID
  output$isoPlot <- renderPlot({
    # Get the Fish ID of the clicked point
    click <- input$plot_click
    if (is.null(click)) return(NULL)
    
    # Find the nearest point in the PCA plot
    nearest_point <- pca_results_no_outliers %>%
      mutate(distance = sqrt((PC1 - click$x)^2 + (PC2 - click$y)^2)) %>%
      slice_min(distance, n = 1)
    
    selected_fish_id <- nearest_point$Fish_id
    
    # Check if a valid Fish ID is found
    if (is.null(selected_fish_id) || is.na(selected_fish_id)) return(NULL)
    
    # Find the Iso values for the selected Fish ID
    fish_index <- which(ids == selected_fish_id)
    iso_values <- measurement_array[fish_index, ]
    
    # Create a plot of Iso vs Distance
    iso_data <- tibble(
      Distance = seq_along(iso_values),
      Iso = iso_values
    )
    
    ggplot(iso_data, aes(x = Distance, y = Iso)) +
      geom_line(color = "blue") +
      geom_point(size = 1, alpha = 0.7) +
      theme_classic() +
      labs(title = paste("Iso vs Distance for Fish ID:", selected_fish_id),
           x = "Distance",
           y = "Iso")
  })
}

# Run the app
shinyApp(ui = ui, server = server)


