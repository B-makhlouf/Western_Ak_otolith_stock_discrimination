library(viridis)
library(viridisLite)
# Source 

source("/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/Code/Helper Code/PCA_functions.R")

# All Metadata 
All_Metadata <- read.csv(here("Data/Metadata and QC/Metadata_and_QC.csv"))

## Best Model(s)
Sr8786_Full_Gam<- read.csv(here("Data/Preprocessed_ts_matrices/Processed_Core_Fw_GAM.csv"))

# Current data 
data<- Sr8786_Full_Gam

# Split up the data into metadat and analysis ts 
metadata <- Sr8786_Full_Gam[, 1:4] #metadata
Analysis_ts_data <- Sr8786_Full_Gam[, -c(1:4)] #analysis ts


# Run a PCA, and provide visualizations of where on the timeseries is driving any separation 
# Remove the Watershed again 

Analysis_metadata<- metadata
PCA_raw <- prcomp(Analysis_ts_data, scale. = TRUE) #run the pca 
PCA_full<- run_pca(Analysis_ts_data, Analysis_metadata) #add all the metadata
natalIsoPCAPlot<-pca_plot(PCA_full,1,2) 
feature_figure<- plot_pca_loadings(PCA_raw, plot_type = "line")
scree_plot_1<-scree_plot(PCA_full)
combined_plot <- natalIsoPCAPlot / (scree_plot_1 | feature_figure)
print(combined_plot)



#### 

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggrepel)

# Extract PCA scores
pca_scores <- as.data.frame(PCA_raw$x)
pca_scores$watershed <- Analysis_metadata$Watershed  # or whatever column contains watershed info

# Define the zoom boundaries to focus on the densest area
zoom_min_x <- -75
zoom_max_x <- 100
zoom_min_y <- -75
zoom_max_y <- 75

# Filter data points for the zoomed region
pca_scores_zoomed <- pca_scores %>%
  filter(PC1 >= zoom_min_x & PC1 <= zoom_max_x & 
           PC2 >= zoom_min_y & PC2 <= zoom_max_y)



# Function to find representative time series for each region
find_representative_ts <- function(scores, data, center_pc1, center_pc2, radius) {
  # Find indices of points within radius of the center
  distances <- sqrt((scores$PC1 - center_pc1)^2 + (scores$PC2 - center_pc2)^2)
  
  # Find the closest point to the specified center
  closest_idx <- which.min(distances)
  
  # Extract the representative time series
  rep_ts <- data[closest_idx, ]
  
  # Return both the series and its PC coordinates
  return(list(
    series = rep_ts,
    pc1 = scores$PC1[closest_idx],
    pc2 = scores$PC2[closest_idx],
    idx = closest_idx
  ))
}

# Define PCA boundaries for the zoomed region
pc1_min <- zoom_min_x
pc1_max <- zoom_max_x
pc2_min <- zoom_min_y
pc2_max <- zoom_max_y

pc1_center <- mean(c(pc1_min, pc1_max))
pc1_range <- pc1_max - pc1_min
pc2_center <- mean(c(pc2_min, pc2_max))
pc2_range <- pc2_max - pc2_min

# Define points to sample in the zoomed PC space, with greater density in the middle
# We'll use a combination of grid and strategically placed points
sample_points <- list()

# Add a grid of points (8x8 for more detail in the zoomed area)
n_grid <- 5  # Increased from 6
for(i in 1:n_grid) {
  for(j in 1:n_grid) {
    pc1_val <- pc1_min + (i-0.5) * pc1_range/n_grid
    pc2_val <- pc2_min + (j-0.5) * pc2_range/n_grid
    sample_points[[length(sample_points) + 1]] <- c(pc1_val, pc2_val)
  }
}

# Add extra points in the central dense region (-10 to 10 on both axes)
dense_center_x <- 0
dense_center_y <- 0
dense_radius <- 10
n_dense_points <- 15  # More points in the densest area
for(i in 1:n_dense_points) {
  angle <- 2 * pi * i / n_dense_points
  # Distribute points at different distances from center
  for(r_factor in c(0.3, 0.6, 0.9)) {
    radius <- dense_radius * r_factor
    pc1_val <- dense_center_x + radius * cos(angle)
    pc2_val <- dense_center_y + radius * sin(angle)
    sample_points[[length(sample_points) + 1]] <- c(pc1_val, pc2_val)
  }
}

# Initialize list to store representative time series
rep_series_list <- list()
rep_points <- data.frame(
  pc1 = numeric(),
  pc2 = numeric(),
  region_id = character(),
  show_plot = logical()  # Flag to control which points show mini plots
)

# Find the closest actual data point to each sample point
# Use a slightly smaller search radius for the zoomed view
search_radius <- min(pc1_range, pc2_range) * 0.08
for(i in 1:length(sample_points)) {
  pc1_target <- sample_points[[i]][1]
  pc2_target <- sample_points[[i]][2]
  
  rep_ts <- find_representative_ts(pca_scores_zoomed, Analysis_ts_data, pc1_target, pc2_target, search_radius)
  
  # Check if this point is too close to existing points
  is_duplicate <- FALSE
  if(nrow(rep_points) > 0) {
    min_dist <- min(sqrt((rep_points$pc1 - rep_ts$pc1)^2 + (rep_points$pc2 - rep_ts$pc2)^2))
    if(min_dist < 0.05 * min(pc1_range, pc2_range)) {
      is_duplicate <- TRUE
    }
  }
  
  if(!is_duplicate) {
    region_id <- paste0("R", i)
    rep_series_list[[region_id]] <- rep_ts
    
    rep_points <- rbind(rep_points, data.frame(
      pc1 = rep_ts$pc1,
      pc2 = rep_ts$pc2,
      region_id = region_id,
      show_plot = TRUE  # Show all mini plots by default
    ))
  }
}

# Normalize time series for consistent visualization
normalize_ts <- function(ts) {
  ts_numeric <- as.numeric(ts)
  if(max(ts_numeric) == min(ts_numeric)) {
    return(rep(0.5, length(ts_numeric)))  # Handle flat lines
  }
  return((ts_numeric - min(ts_numeric)) / (max(ts_numeric) - min(ts_numeric)))
}

# Create mini plots for each representative time series
create_mini_plot <- function(ts, width = 100, height = 60) {
  ts_norm <- normalize_ts(ts)
  ts_df <- data.frame(
    TimePoint = 1:length(ts_norm),
    Value = ts_norm
  )
  
  p <- ggplot(ts_df, aes(x = TimePoint, y = Value)) +
    geom_line(color = "blue", size = 0.7) +
    theme_void() +
    theme(
      panel.background = element_rect(fill = "white", color = "black", size = 0.3),
      plot.margin = margin(2, 2, 2, 2)
    )
  
  # Convert to grob
  return(ggplotGrob(p))
}

# Generate mini plots
mini_plots <- list()
for(i in 1:length(rep_series_list)) {
  region_id <- names(rep_series_list)[i]
  ts <- as.numeric(rep_series_list[[region_id]]$series)
  mini_plots[[region_id]] <- create_mini_plot(ts)
}

# Create the main PCA plot with zoomed boundaries
main_plot <- ggplot(pca_scores_zoomed, aes(x = PC1, y = PC2, color = watershed)) +
  geom_point(alpha = 0.7, size = 2) +
  scale_color_manual(
    name = "Watershed",
    values = c(
      "Yukon" = "dodgerblue",
      "Nush" = "orange",  # Note: Correct spelling is "burntorange" (no 'n' in 'burnt')
      "Kusko" = "firebrick2"
    ),
    # This ensures the legend shows all categories even if some are missing in the zoomed view
    breaks = c("Yukon", "Nush", "Kusko")
  ) +
  theme_minimal() +
  labs(
    title = "PCA with Time Series Patterns",
    subtitle = paste0("Colored by Watershed | Focused on region: PC1 (", pc1_min, " to ", pc1_max, 
                      "), PC2 (", pc2_min, " to ", pc2_max, ")"),
    x = paste0("PC1 (", round(summary(PCA_raw)$importance[2, 1] * 100, 1), "%)"),
    y = paste0("PC2 (", round(summary(PCA_raw)$importance[2, 2] * 100, 1), "%)")
  ) +
  coord_cartesian(xlim = c(pc1_min, pc1_max), ylim = c(pc2_min, pc2_max)) +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10),
    legend.position = "right"
  )

# Calculate the radius to place mini plots - use smaller values for zoomed view
pc_radius_x <- (pc1_max - pc1_min) / 2
pc_radius_y <- (pc2_max - pc2_min) / 2
pc_radius <- sqrt(pc_radius_x^2 + pc_radius_y^2)

# Reduce size of mini plots for better fit in zoomed view
mini_width <- pc_radius * 0.07   # Adjusted for zoomed view
mini_height <- pc_radius * 0.07  # Adjusted for zoomed view

# Function to calculate positions for mini plots
calculate_positions <- function(points, center_x, center_y, plot_radius, mini_width, mini_height) {
  n_points <- nrow(points)
  positions <- data.frame(
    x = numeric(n_points),
    y = numeric(n_points),
    angle = numeric(n_points),
    xmin = numeric(n_points),
    xmax = numeric(n_points),
    ymin = numeric(n_points),
    ymax = numeric(n_points),
    region_id = character(n_points),
    show_plot = logical(n_points)
  )
  
  for(i in 1:n_points) {
    # Calculate angle from center to this point
    dx <- points$pc1[i] - center_x
    dy <- points$pc2[i] - center_y
    angle <- atan2(dy, dx)
    
    # Position for mini plot - closer to actual points for zoomed view
    x_pos <- points$pc1[i] + (plot_radius * 0.1) * cos(angle)
    y_pos <- points$pc2[i] + (plot_radius * 0.1) * sin(angle)
    
    # Box boundaries for the mini plot
    positions$x[i] <- x_pos
    positions$y[i] <- y_pos
    positions$angle[i] <- angle
    positions$xmin[i] <- x_pos - mini_width/2
    positions$xmax[i] <- x_pos + mini_width/2
    positions$ymin[i] <- y_pos - mini_height/2
    positions$ymax[i] <- y_pos + mini_height/2
    positions$region_id[i] <- points$region_id[i]
    positions$show_plot[i] <- points$show_plot[i]
  }
  
  return(positions)
}

# Reduce the radius multiplier for zoomed view
pc_radius <- sqrt(pc_radius_x^2 + pc_radius_y^2)
mini_width <- pc_radius * 0.05   # Smaller for zoomed view
mini_height <- pc_radius * 0.05  # Smaller for zoomed view

# Recalculate positions with smaller offset
positions <- calculate_positions(
  rep_points, 
  pc1_center, 
  pc2_center, 
  pc_radius * 0.2,  # Smaller radius for zoomed view
  mini_width, 
  mini_height
)

# Simpler position adjustment function
adjust_positions_simple <- function(positions) {
  # Just nudge overlapping plots slightly
  for(i in 1:nrow(positions)) {
    for(j in 1:nrow(positions)) {
      if(i != j) {
        # Check if plots overlap
        x_overlap <- (positions$xmin[i] < positions$xmax[j]) && 
          (positions$xmax[i] > positions$xmin[j])
        y_overlap <- (positions$ymin[i] < positions$ymax[j]) && 
          (positions$ymax[i] > positions$ymin[j])
        
        if(x_overlap && y_overlap) {
          # Calculate direction to move
          dx <- positions$x[i] - positions$x[j]
          dy <- positions$y[i] - positions$y[j]
          dist <- max(sqrt(dx^2 + dy^2), 0.001)
          
          # Move them apart
          move_dist <- mini_width * 0.6
          positions$x[i] <- positions$x[i] + dx/dist * move_dist * 0.5
          positions$y[i] <- positions$y[i] + dy/dist * move_dist * 0.5
          positions$x[j] <- positions$x[j] - dx/dist * move_dist * 0.5
          positions$y[j] <- positions$y[j] - dy/dist * move_dist * 0.5
        }
      }
    }
  }
  
  # Update boundaries
  positions$xmin <- positions$x - mini_width/2
  positions$xmax <- positions$x + mini_width/2
  positions$ymin <- positions$y - mini_height/2
  positions$ymax <- positions$y + mini_height/2
  
  return(positions)
}

positions <- adjust_positions_simple(positions)

# Adjust positions with a smaller minimum distance for zoomed view
positions <- adjust_positions(positions, 0.25)  # Adjusted for zoomed view

# Merge pc1 and pc2 values from rep_points into positions for easier reference
positions <- merge(positions, rep_points[, c("region_id", "pc1", "pc2")], by = "region_id")

# FIX: Make sure all highlighted points show mini plots
# Set all show_plot values to TRUE to ensure all highlighted points have mini plots
positions$show_plot <- TRUE

# Optional: In very crowded areas, we can still limit the number of plots
# using cluster-based selection, but with a higher density threshold
if(require(cluster) && nrow(rep_points) > 25) {  # Only for very high point counts
  # Calculate distance matrix between points
  dist_matrix <- dist(rep_points[, c("pc1", "pc2")])
  
  # Hierarchical clustering
  hc <- hclust(dist_matrix)
  
  # Use more clusters to keep more representative points
  n_clusters <- max(15, nrow(rep_points) %/% 3)  # Keep at least 1/3 of points
  clusters <- cutree(hc, k = n_clusters)
  
  # For each cluster, keep the most central point - but only for very close points
  for(cl in unique(clusters)) {
    members <- which(clusters == cl)
    if(length(members) > 1) {
      # Calculate the maximum distance within this cluster
      member_coords <- rep_points[members, c("pc1", "pc2")]
      max_internal_dist <- max(dist(member_coords))
      
      # Only filter if points are very close (within 5% of PC range)
      if(max_internal_dist < 0.05 * min(pc1_range, pc2_range)) {
        cluster_pc1 <- mean(rep_points$pc1[members])
        cluster_pc2 <- mean(rep_points$pc2[members])
        
        # Find closest point to center
        dists <- sqrt((rep_points$pc1[members] - cluster_pc1)^2 + 
                        (rep_points$pc2[members] - cluster_pc2)^2)
        central_idx <- members[which.min(dists)]
        
        # Only use this central point in dense regions
        positions$show_plot[match(rep_points$region_id[members], positions$region_id)] <- FALSE
        positions$show_plot[match(rep_points$region_id[central_idx], positions$region_id)] <- TRUE
      }
    }
  }
}

# Now create the final plot with mini plots and connecting lines
final_plot <- main_plot +
  # Add connecting lines first
  geom_segment(
    data = positions,
    aes(x = pc1, y = pc2, xend = x, yend = y),
    color = "darkgray",
    size = 0.5,
    linetype = "dashed")
  # ) +
  # # Highlight the PCA points
  # geom_point(
  #   data = positions,
  #   aes(x = pc1, y = pc2),
  #   color = "red",
  #   size = 3
  # )

# Add mini plots
for(i in 1:nrow(positions)) {
  region_id <- positions$region_id[i]
  final_plot <- final_plot +
    annotation_custom(
      mini_plots[[region_id]],
      xmin = positions$xmin[i],
      xmax = positions$xmax[i],
      ymin = positions$ymin[i],
      ymax = positions$ymax[i]
    )
}

# Print the final plot
print(final_plot)

# Add each mini plot as an annotation - only for points that should show mini plots
for(i in 1:nrow(positions)) {
  if(positions$show_plot[i]) {
    region_id <- positions$region_id[i]
    final_plot <- final_plot +
      annotation_custom(
        mini_plots[[region_id]],
        xmin = positions$xmin[i],
        xmax = positions$xmax[i],
        ymin = positions$ymin[i],
        ymax = positions$ymax[i]
      )
  }
}

# Print the final plot
print(final_plot)


############################### 
########## Shiny App 
##############################
library(shiny)
library(ggplot2)
library(dplyr)
library(here)
library(plotly)
library(zoo)

dataset_description <- reactive({
  paste("Data:", data_type, "| Landmarks:", paste(landmark_filter, collapse = ", "))
})

watershed_counts <- reactive({
  PCA_full %>%
    count(Watershed) %>%
    arrange(desc(n))
})

# UI
ui <- fluidPage(
  titlePanel("Timeseries Classification of AYK Otolith Isotopes"),
  
  sidebarPanel(
    width = 3,  
    
    
    # Display dataset info in bold
    strong(textOutput("datasetInfo")),  
    hr(),
    helpText("Click on a point in the PCA plot to view Iso vs. Distance for that Fish ID."),
    selectInput("xComp", "X Component:", choices = names(PCA_full), selected = "PC1"),
    selectInput("yComp", "Y Component:", choices = names(PCA_full), selected = "PC2"),
    
    actionButton("resetZoom", "Reset Zoom"),
    hr(),
    
    # Feature Loadings Plot inside Sidebar
    plotOutput("featurePlot", height = "300px")
  ),
  
  mainPanel(
    width = 9,  
    tabsetPanel(
      id = "tabs",  
      tabPanel("2D PCA", 
               plotOutput("pcaPlot", click = "pcaClick", height = "800px",
                          brush = brushOpts(id = "pcaBrush", resetOnNew = TRUE))
      ),
      tabPanel("3D PCA", 
               plotlyOutput("pcaPlot3D", height = "800px")  
      ),
      tabPanel("Classification Diagnostics",
               verbatimTextOutput("confMatrixText"),
               fluidRow(
                 column(4, plotOutput("yearPlot")),
                 column(4, plotOutput("corePlot"))
               )
      )
    ),
    conditionalPanel(
      condition = "input.tabs == '2D PCA' || input.tabs == '3D PCA'",
      plotOutput("isoPlot")
    )
  )
)


# Server
server <- function(input, output, session) {
  
  output$datasetInfo <- renderText({
    description <- dataset_description()
    counts <- watershed_counts()
    
    count_text <- paste(counts$Watershed, counts$n, sep = ": ", collapse = " | ")
    paste0(description, "\n\nSamples:\n", count_text)
    
  })
  
  zoomRegion <- reactiveValues(x = NULL, y = NULL)
  
  observeEvent(input$resetZoom, {
    zoomRegion$x <- NULL
    zoomRegion$y <- NULL
  })
  
  observeEvent(input$pcaBrush, {
    brush <- input$pcaBrush
    if (!is.null(brush)) {
      zoomRegion$x <- c(brush$xmin, brush$xmax)
      zoomRegion$y <- c(brush$ymin, brush$ymax)
    }
  })
  
  output$pcaPlot <- renderPlot({
    req(PCA_full)
    
    ggplot(PCA_full, aes_string(x = input$xComp, y = input$yComp, color = "Watershed")) +
      geom_point(size = 3, alpha = 0.6) +
      theme_classic() +
      labs(title = "PCA of Iso Values by Watershed",
           x = input$xComp,
           y = input$yComp) +
      theme(legend.title = element_blank()) +
      scale_color_manual(values = c("Yukon" = "dodgerblue",
                                    "Nush" = "darkorange",
                                    "Kusko" = "#74AC64")) +  # Custom colors
      coord_cartesian(xlim = zoomRegion$x, ylim = zoomRegion$y)
  })
  
  output$confMatrixText <- renderPrint({
    req(conf_matrix)
    print(conf_matrix)
  })
  
  output$pcaPlot3D <- renderPlotly({
    p <- plot_ly(
      data = PCA_full,
      x = ~PC1, y = ~PC2, z = ~PC3,
      type = "scatter3d",
      mode = "markers",
      marker = list(size = 3, opacity = 0.5),
      color = ~Watershed,
      source = "A"  # Assign source ID
    ) %>%
      layout(
        title = "3D PCA Plot",
        scene = list(
          xaxis = list(title = "PC1"),
          yaxis = list(title = "PC2"),
          zaxis = list(title = "PC3")
        )
      )
    
    event_register(p, "plotly_click")  # Register click event
    return(p)
  })
  
  selectedFish <- reactiveVal(NULL)
  
  # Select Fish ID from 2D PCA Click
  observeEvent(input$pcaClick, {
    nearPoint <- nearPoints(PCA_full, input$pcaClick, threshold = 5, maxpoints = 1)
    if (nrow(nearPoint) > 0) {
      selectedFish(nearPoint$Fish_id[1])
    }
  })
  
  # Select Fish ID from 3D PCA Click
  observeEvent(event_data("plotly_click", source = "A"), {
    clickData <- event_data("plotly_click", source = "A")
    if (!is.null(clickData)) {
      clickedIndex <- clickData$pointNumber + 1  # Convert from 0-based index
      selectedFish(PCA_full$Fish_id[clickedIndex])
    }
  })
  
  # Time Series Plot (Updates when Fish ID is selected)
  output$isoPlot <- renderPlot({
    req(selectedFish(), Analysis_metadata, Analysis_ts_data)
    
    fishIndex <- which(Analysis_metadata$Fish_id == selectedFish())
    if (length(fishIndex) == 0) return(NULL)
    
    isoData <- tibble(
      Distance = seq_along(Analysis_ts_data[fishIndex, ]),
      Iso = as.numeric(Analysis_ts_data[fishIndex, ])
    ) 
    
    ggplot(isoData, aes(x = Distance, y = Iso)) +
      geom_point(alpha = 0.7, color = "grey30", size = 2) +
      geom_hline(yintercept = 0.7092, color = "dodgerblue4", size = 2, linetype = "dashed") +
      theme_grey() +
      labs(title = paste("Iso vs. Distance for Fish ID:", selectedFish()),
           x = "Distance",
           y = "Iso")
  })
  
  output$featurePlot <- renderPlot({
    print(feature_figure)  
  })
  
  output$yearPlot <- renderPlot({ year_proportion })
  output$corePlot <- renderPlot({ core_proportion })
}

shinyApp(ui = ui, server = server)


