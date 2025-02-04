# Load necessary libraries
library(tidyverse)
library(here)
library(shiny)
library(viridis)

### Source in the function which does the data preprocessing. 
source(here("/Users/benjaminmakhlouf/Research_repos/Western_Ak_otolith_stock_discrimination/Code/Helper Code/Raw_Data_Preprocessing.R"))

# Run the function 
processed_data<-process_trimmed_data()

###############################################################################

# Pull out the metadata and measurement array from the processed data
metadata <- tibble(
  Fish_id = processed_data$ids,
  Watershed = processed_data$watersheds,
  Natal_Iso = processed_data$natal_origins
)


iso_data<- processed_data$measurement_array











#############################################################################
#############################################################################
#################### PCA Exploration ########################################
#############################################################################




# Add the metadata to the front of measurement_array and save as as .csv 

all_data<- cbind(metadata_filtered, measurement_array_filtered)

write.csv(all_data, "Data/Processed/PCA_data.csv")


#############################################################################

# Read in the latest version of PCA_data.csv 
all_data<- read.csv("Data/Processed/PCA_data.csv")

# Extract metadata
metadata_filtered<- all_data[,1:4]
measurement_array_filtered<- all_data[-c(1:4)]

ids<- metadata_filtered$Fish_id
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




################################################################################
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


################################################################################
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

# Identify top 10 features driving PC4
top_features_PC4 <- loadings %>%
  arrange(desc(abs(PC4))) %>%
  slice(1:10) %>%
  select(Feature, PC4)

# Identify top 10 features driving PC5
top_features_PC5 <- loadings %>%
  arrange(desc(abs(PC5))) %>%
  slice(1:10) %>%
  select(Feature, PC5)

# Print the top features for each PC
list(
  Top_10_PC1 = top_features_PC1,
  Top_10_PC2 = top_features_PC2,
  Top_10_PC3 = top_features_PC3
)
} 




###############################################################
library(ggplot2)
library(viridis)
library(tidyr)

# Create a data frame from the matrix
feature_matrix <- matrix(1, nrow = 5, ncol = 1000)
feature_matrix[1, ] <- abs(loadings$PC1)
feature_matrix[2, ] <- abs(loadings$PC2)
feature_matrix[3, ] <- abs(loadings$PC3)
feature_matrix[4, ] <- abs(loadings$PC4)
feature_matrix[5, ] <- abs(loadings$PC5)

# Convert to long format for ggplot
plot_data <- data.frame(
  Index = rep(1:1000, times = 5),
  FeatureImportance = c(
    feature_matrix[1, ],
    feature_matrix[2, ],
    feature_matrix[3, ],
    feature_matrix[4, ],
    feature_matrix[5, ]
  ),
  Component = rep(c("PC1", "PC2", "PC3", "PC4", "PC5"), each = 1000),
  Y = rep(1, 5000) # Constant Y value for straight line
)

# Plot with facets
feature_plot_color <- ggplot(plot_data, aes(x = Index, y = Y, color = FeatureImportance)) +
  geom_point(size = 3) +
  scale_color_viridis(option = "plasma", direction = -1) +
  theme_grey() +
  labs(
    title = "Timeseries Loadings onto PCA variance",
    x = "Index",
    y = NULL, # Remove y-axis label
    color = "Loading (Abs. value)"
  ) +
  facet_wrap(~Component, nrow = 5) + # Separate panels for PC1 to PC5
  theme(
    axis.line.y = element_blank(), # Remove y-axis line
    axis.ticks.y = element_blank(), # Remove y-axis ticks
    axis.text.y = element_blank(), # Remove y-axis text
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8)
  )

feature_bar_plot <- ggplot(plot_data, aes(x = Index, y = FeatureImportance, color = FeatureImportance)) +
  geom_bar(stat = "identity", position = "identity", width = 1, alpha = 0.4) + 
  scale_color_viridis(option = "plasma", direction = -1) +
  theme_grey() +
  labs(
    title = "Timeseries Loadings onto PCA variance",
    x = "Index",
    y = NULL, # Remove y-axis label
    fill = "Loading (Abs. value)"
  ) +
  facet_wrap(~Component, nrow = 5) + # Separate panels for PC1 to PC5
  theme(
    axis.line.y = element_blank(), # Remove y-axis line
    axis.ticks.y = element_blank(), # Remove y-axis ticks
    axis.text.y = element_blank(), # Remove y-axis text
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8)
  )

feature_line_plot <- ggplot(plot_data, aes(x = Index, y = FeatureImportance, group = Component, color = FeatureImportance)) +
  geom_line(alpha = 0.9, size = 1) + 
  scale_color_viridis(option = "plasma", direction = -1) +
  theme_grey() +
  labs(
    title = "Timeseries Loadings onto PCA variance",
    x = "Index",
    y = NULL,  # Remove y-axis label
    color = "Loading (Abs. value)"
  ) +
  facet_wrap(~Component, nrow = 5) +  # Separate panels for PC1 to PC5
  theme(
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8)
  )

# Put all in a single figure using cowplot 
library(cowplot)

# Combine the plots
feature_plot <- plot_grid(feature_plot_color, feature_bar_plot, feature_line_plot, nrow = 3)

# Display the plot
print(feature_plot)

# Export as a long PDF
ggsave("Figures/feature_plot.pdf", feature_plot, width = 16, height = 11, units = "in") # Adjusted height for 5 panels


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
  
  # Identify top 10 features driving PC4
  top_features_PC4 <- loadings %>%
    arrange(desc(abs(PC4))) %>%
    slice(1:10) %>%
    select(Feature, PC4)
  
  # Identify top 10 features driving PC5
  top_features_PC5 <- loadings %>%
    arrange(desc(abs(PC5))) %>%
    slice(1:10) %>%
    select(Feature, PC5)
  
  # Print the top features for each PC
  list(
    Top_10_PC1 = top_features_PC1,
    Top_10_PC2 = top_features_PC2,
    Top_10_PC3 = top_features_PC3
  )
} 

# Extract the top 10 features into a vector

PC1_features<- top_features_PC1$Feature
PC2_features<- top_features_PC2$Feature
PC3_features<- top_features_PC3$Feature
PC4_features<- top_features_PC4$Feature
PC5_features<- top_features_PC5$Feature


library(NatParksPalettes)

NatParksPalettes

natparks.pals("Olympic")

#choose the colors from the palette
plotcolors <- natparks.pals("Saguaro", n = 6, type = "discrete")

# Select specific colors based on the desired indices (1, 4, 6, 8, 10)
plotcolors <- plotcolors[c(1, 2, 3, 4, 5)]


################# R Shiny exploration plot 
# UI
library(shiny)
library(ggplot2)
library(dplyr)

# UI
ui <- fluidPage(
  titlePanel("PCA Analysis Viewer"),
  # Define the landing page section with an image
  
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
      Iso = measurement_array_filtered[fishIndex, ]
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


