# Load necessary libraries
library(tidyverse)
library(here)
library(shiny)

if (T){
  
# Define directories and their corresponding metadata files
data_directories <- list(
  Nush2014 = here("/Users/benjaminmakhlouf/Research_repos/Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/Data/Otolith Data/LA Data/Trimmed/2014 Nush"),
  Yukon2015 = here("/Users/benjaminmakhlouf/Research_repos/Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/Data/Otolith Data/LA Data/Trimmed/2015 Yukon"),
  Yukon2016 = here("/Users/benjaminmakhlouf/Research_repos/Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/Data/Otolith Data/LA Data/Trimmed/2016 Yukon"),
  Kusko2017 = here("/Users/benjaminmakhlouf/Research_repos/Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/Data/Otolith Data/LA Data/Trimmed/2017 Kusko"),
  Kusko2019 = here("/Users/benjaminmakhlouf/Research_repos/Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/Data/Otolith Data/LA Data/Trimmed/2019 Kusko")
)

metadata_files <- list(
  Nush2014 = "/Users/benjaminmakhlouf/Research_repos/Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/Data/Natal Origins/Cleaned/Nushugak_2014_Cleaned_Natal_Origins.csv",
  Yukon2015 = "/Users/benjaminmakhlouf/Research_repos/Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/Data/Final_QC_NatalOriginCPUE/ALL_DATA_2015_Yukon_Natal_Origins.csv",
  Yukon2016 = "/Users/benjaminmakhlouf/Research_repos/Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/Data/Final_QC_NatalOriginCPUE/ALL_DATA_2016_Yukon_Natal_Origins.csv",
  Kusko2017 = "/Users/benjaminmakhlouf/Research_repos/Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/Data/Final_QC_NatalOriginCPUE/ALL_DATA_2017_Kusko_Natal_Origins.csv",
  Kusko2019 = "/Users/benjaminmakhlouf/Research_repos/Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/Data/Final_QC_NatalOriginCPUE/ALL_DATA_2019_Kusko_Natal_Origins.csv"
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
        Fish_id_from_filename <- tools::file_path_sans_ext(file_name)
        
        if (Fish_id_from_filename %in% metadata$Fish_id) {
          ind_data <- read_csv(.x) %>% select(Iso, Watershed, Fish_id)
          watershed <- ind_data$Watershed[1]
          natal_iso<- ind_data$natal_iso[1]
          interpolated <- interpolate_data(ind_data)
          tibble(
            Fish_id = Fish_id_from_filename,
            Watershed = watershed,
            Iso = list(interpolated)
            
          )
        } else {
          tibble(Fish_id = NA, Watershed = NA,  Iso = list(rep(NA, 1000)))
        }
      }, error = function(e) {
        message("Error processing file: ", .x, " - ", e$message)
        tibble(Fish_id = NA, Watershed = NA,  Iso = list(rep(NA, 1000)))
      })
    })
}

# Process all datasets
results_list <- map2(data_directories, metadata_files, ~ {
  files <- list.files(.x, full.names = TRUE)
  metadata <- read_csv(.y)
  process_directory(files, metadata)
})

# Combine results into a single tibble
combined_results <- bind_rows(results_list, .id = "Dataset")

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

# Extract metadata
ids <- filtered_results$Fish_id
watersheds <- filtered_results$Watershed


Nush2014_metadata<- read.csv("/Users/benjaminmakhlouf/Research_repos/Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/Data/Natal Origins/Cleaned/Nushugak_2014_Cleaned_Natal_Origins.csv")
Yukon2015_metadata<- read.csv("/Users/benjaminmakhlouf/Research_repos/Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/Data/Final_QC_NatalOriginCPUE/ALL_DATA_2015_Yukon_Natal_Origins.csv")
Yukon2016_metadata<- read.csv("/Users/benjaminmakhlouf/Research_repos/Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/Data/Final_QC_NatalOriginCPUE/ALL_DATA_2016_Yukon_Natal_Origins.csv")
Kusko2017_metadata<- read.csv("/Users/benjaminmakhlouf/Research_repos/Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/Data/Final_QC_NatalOriginCPUE/ALL_DATA_2017_Kusko_Natal_Origins.csv")
Kusko2019_metadata<- read.csv("/Users/benjaminmakhlouf/Research_repos/Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/Data/Final_QC_NatalOriginCPUE/ALL_DATA_2019_Kusko_Natal_Origins.csv")

# select the natal origin and fish ID from the metadata 
Nush2014_metadata<- Nush2014_metadata %>% select(Fish_id, natal_iso)
Yukon2015_metadata<- Yukon2015_metadata %>% select(Fish_id, natal_iso)
Yukon2016_metadata<- Yukon2016_metadata %>% select(Fish_id, natal_iso)
Kusko2017_metadata<- Kusko2017_metadata %>% select(Fish_id, natal_iso)
Kusko2019_metadata<- Kusko2019_metadata %>% select(Fish_id, natal_iso)

# combine the metadata
metadata<- rbind(Nush2014_metadata, Yukon2015_metadata, Yukon2016_metadata, Kusko2017_metadata, Kusko2019_metadata)

# match the fish_id to the ids
metadata<- metadata %>% filter(Fish_id %in% ids)

#Put the iso values in order of their match between Fish_id in metadata and ids
metadata<- metadata[match(ids, metadata$Fish_id),]

#etract the ordered natal iso values
natal_iso<- metadata$natal_iso

}
#############################################################################
#############################################################################
#################### PCA Exploration ########################################
#############################################################################


# Combine all of the metadata into one dataframe
metadata <- tibble(
  Fish_id = ids,
  Watershed = watersheds,
  Natal_iso = natal_iso
)

# Define a range of natal origins 
natal_origin_filtering<- c(.703,.713)

#Find the indices of the natal origins that are within the range
natal_origin_indices<- which(metadata$Natal_iso >= natal_origin_filtering[1] & metadata$Natal_iso <= natal_origin_filtering[2])

# Filter the metadata to only include the natal origins within the range
metadata_filtered<- metadata[natal_origin_indices,]

# Filter the measurement array to only include the natal origins within the range
measurement_array_filtered<- measurement_array[natal_origin_indices,]

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
pca_x <- "PC2"
pca_y <- "PC3"  

# Update ggplot figures
pca_plot <- ggplot(pca_results, aes_string(x = pca_x, y = pca_y, color = "Watershed")) +
  geom_point(size = 2, alpha = .9) +
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


################# R Shiny exploration plot 
ui <- fluidPage(
  titlePanel("PCA Analysis Viewer"),
  sidebarLayout(
    sidebarPanel(
      helpText("Click on a point in the PCA plot to view Iso vs. Distance for that Fish ID."),
      helpText("Drag to zoom in on the PCA plot."),
      selectInput("xComp", "X Component:", choices = names(pca_results), selected = "PC2"),
      selectInput("yComp", "Y Component:", choices = names(pca_results), selected = "PC3"),
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
  
  # Render PCA Plot with dynamic components and zoom
  output$pcaPlot <- renderPlot({
    ggplot(pca_results, aes_string(x = input$xComp, y = input$yComp, color = "Watershed")) +
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
    nearPoint <- nearPoints(pca_results, input$pcaClick, threshold = 5, maxpoints = 1)
    if (nrow(nearPoint) > 0) {
      selectedFish(nearPoint$Fish_id[1])
    }
  })
  
  # Render Iso Plot
  output$isoPlot <- renderPlot({
    req(selectedFish())
    
    # Find the index of the selected Fish ID
    fishIndex <- which(ids == selectedFish())
    
    # Retrieve Iso data for the selected Fish ID
    isoData <- tibble(
      Distance = seq_along(measurement_array[fishIndex, ]),
      Iso = measurement_array[fishIndex, ]
    )
    
    # Calculate moving average (smooth the Iso data)
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
