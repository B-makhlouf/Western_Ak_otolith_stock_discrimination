library(dplyr)
library(shapeR)
library(ggplot2)

shape = shapeR("/Users/benjaminmakhlouf/Research_repos/Western_Ak_otolith_stock_discrimination/ShapeAnalysis", "FISH.csv")

#### Detect outlines 
outlinesonly = detect.outline(shape, threshold = 0.2, write.outline.w.org = FALSE)


########### Take a look at the raw outlines all plotted together 

yk_outlines = outlinesonly@outline.list$YK
kk_outlines = outlinesonly@outline.list$KK
nk_outlines = outlinesonly@outline.list$NK


# Initialize an empty list to store the data
data_list <- list()

# Add yk_outlines data
for (i in seq_along(yk_outlines)) {
  outline <- yk_outlines[[i]]
  x <- as.numeric(outline[[1]])
  y <- as.numeric(outline[[2]])
  data_list[[length(data_list) + 1]] <- data.frame(x = x, y = y, watershed = "YK", individual = i)
}

# Add kk_outlines data
for (i in seq_along(kk_outlines)) {
  outline <- kk_outlines[[i]]
  x <- as.numeric(outline[[1]])
  y <- as.numeric(outline[[2]])
  data_list[[length(data_list) + 1]] <- data.frame(x = x, y = y, watershed = "KK", individual = i)
}

# Add nk_outlines data
for (i in seq_along(nk_outlines)) {
  outline <- nk_outlines[[i]]
  x <- as.numeric(outline[[1]])
  y <- as.numeric(outline[[2]])
  data_list[[length(data_list) + 1]] <- data.frame(x = x, y = y, watershed = "NK", individual = i)
}

# Combine all data into a single data frame
shape_data <- do.call(rbind, data_list)

# Create A plot of all shapes together... 
allShapesTogetherPlot<- ggplot(shape_data, aes(x = x, y = y, group = interaction(watershed, individual), color = watershed)) +
  geom_path(linewidth = .3, alpha =.1) +  # Use geom_path() to connect points in order
  scale_color_manual(values = c("YK" = "blue", "KK" = "red", "NK" = "green")) +  # Assign colors
  labs(x = "X", y = "Y", title = "All Outlines", color = "Watershed") +  # Add labels and title
  theme_grey() +  # Use a minimal theme
  theme(legend.position = "top")  

ggsave("Figures/allShapesTogetherPlot.png", allShapesTogetherPlot, width = 20, height = 15, units = "cm")


#########################################################################################################

coefficients = generateShapeCoefficients(outlinesonly) # Generate the RAW coeffients (not standardized)
coefShapesExtr = enrich.master.list(coefficients) #connect to the metadata


#MEAN reconstruction of shape for each class 
plotWaveletShape(coefShapesExtr, "Watershed", show.angle = TRUE, lwd = 2,lty = 1)


#########################################################################################################





