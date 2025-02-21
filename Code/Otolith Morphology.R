

library(dplyr)
library(shapeR)
library(ggplot2)
library(dplyr)


shape = shapeR("/Users/benjaminmakhlouf/Research_repos/Western_Ak_otolith_stock_discrimination/ShapeAnalysis", "FISH.csv")

#### Detect outlines 
outlinesonly = detect.outline(shape, threshold = 0.2, write.outline.w.org = FALSE)

### pull out all the outlines for each class 
yk_outlines = outlinesonly@outline.list$YK
kk_outlines = outlinesonly@outline.list$KK
nk_outlines = outlinesonly@outline.list$NK

# Function to align outlines using Procrustes analysis
align_outlines <- function(outlines) {
  # Convert list of outlines to a 3D array (required by geomorph)
  outlines_array <- simplify2array(outlines)
  
  # Perform Procrustes alignment
  aligned_outlines <- gpagen(outlines_array, ProcD = TRUE)
  
  return(aligned_outlines$coords)
}

# Align outlines for each class
yk_aligned <- align_outlines(yk_outlines)
kk_aligned <- align_outlines(kk_outlines)
nk_aligned <- align_outlines(nk_outlines)













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


shape_summary <- shape_data %>%
  group_by(watershed, x, y) %>%
  summarise(
    mean_x = mean(x, na.rm = TRUE),
    mean_y = mean(y, na.rm = TRUE),
    ci_lower_x = mean(x, na.rm = TRUE) - 1.96 * sd(x, na.rm = TRUE) / sqrt(n()),
    ci_upper_x = mean(x, na.rm = TRUE) + 1.96 * sd(x, na.rm = TRUE) / sqrt(n()),
    ci_lower_y = mean(y, na.rm = TRUE) - 1.96 * sd(y, na.rm = TRUE) / sqrt(n()),
    ci_upper_y = mean(y, na.rm = TRUE) + 1.96 * sd(y, na.rm = TRUE) / sqrt(n())
  ) %>%
  ungroup()


meanShapesPlot <- ggplot(shape_summary, aes(x = mean_x, y = mean_y, color = watershed)) +
  geom_path(linewidth = 0.5) +  # Plot the mean shape
  geom_ribbon(aes(ymin = ci_lower_y, ymax = ci_upper_y, fill = watershed), alpha = 0.2) +  # Add confidence interval for y
  geom_ribbon(aes(xmin = ci_lower_x, xmax = ci_upper_x, fill = watershed), alpha = 0.2) +  # Add confidence interval for x
  scale_color_manual(values = c("YK" = "blue", "KK" = "red", "NK" = "green")) +  # Assign colors
  scale_fill_manual(values = c("YK" = "blue", "KK" = "red", "NK" = "green")) +  # Assign fill colors
  labs(x = "X", y = "Y", title = "Mean Outlines with 95% Confidence Intervals", color = "Watershed", fill = "Watershed") +  # Add labels and title
  theme_grey() +  # Use a minimal theme
  theme(legend.position = "top")





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

est.list = estimate.outline.reconstruction(coefShapesExtr)

?estimate.outline.reconstruction
outline.reconstruction.plot(est.list, max.num.harmonics = 12)

?outline.reconstruction.plot


plotWavelet(coefShapesExtr, level = 5, class.name = "pop", useStdcoef = TRUE)


