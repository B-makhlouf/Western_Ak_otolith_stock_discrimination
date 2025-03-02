# Load required libraries
#install.packages("devtools")

#devtools::install_github("geomorphR/geomorph")
#install.packages("rgl")
#install.packages("devtools")
# devtools::install_github("MomX/Momocs")
#install.packages("shapeR")
#install.packages("tidyverse")
#install.packages("shapeR")
#install.packages("Momocs")
#remove.packages("Momocs")
# install.packages("ggbiplot")
# install.packages("pheatmap")
# install.packages("plotly")

library(tidyverse)
library(shapeR)
library(Momocs)
library(ggbiplot)
library(plotly)

# remove everythng but shape and outlines only 
rm (list = ls()[!ls() %in% c("shape", "outlinesonly")])

# Use ShapeR to extract outlines
shape <- shapeR("/Users/benjaminmakhlouf/Research_repos/Western_Ak_otolith_stock_discrimination/ShapeAnalysis", "FISH.csv")
outlinesonly <- detect.outline(shape, threshold = 0.2, write.outline.w.org = FALSE)

# Extract outlines for each class
yk_outlines <- outlinesonly@outline.list$YK
kk_outlines <- outlinesonly@outline.list$KK
nk_outlines <- outlinesonly@outline.list$NK

# Combine all outlines into one list
all_outlines <- c(kk_outlines, nk_outlines, yk_outlines)

# Extract picnames and watersheds
picnames <- outlinesonly@master.list.org$picname
watershed <- outlinesonly@master.list.org$Watershed

# bad outlines, from the diagnostic "outline" images  
badOtos<- c("2022_kk_302", "2022_kk_307", "2022_kk_309", "2022_kk_310", "2022_kk_313", "2022_kk_315","2022_kk_322","2022_kk_329","2022_kk_363","2023_kk_108","2023_kk_128","2023_kk_133","2023_kk_134","2023_kk_141","2015_nk_002","2015_nk_005","2015_nk_006","2015_nk_021","2015_nk_035","2015_nk_047","2015_nk_093","2015_nk_100","2017_nk_146","2019_nk_163","2020_yk_132","2020_yk_182","2023_yk_037","2023_yk_061")

# Find the indices of picnames which match badOtos
badOtos_indices <- which(picnames %in% badOtos)

# Remove bad outlines, picnames, and watershed
filtered_outlines <- all_outlines[-badOtos_indices]
picnames <- picnames[-badOtos_indices]
watershed <- watershed[-badOtos_indices]

table(watershed)

# Randomly choose 20 watershed indices from each watershed
indices <- c(
  sample(which(watershed == "Kuskokwim"), 80),
  sample(which(watershed == "Nushagak"), 80),
  sample(which(watershed == "Yukon"), 80)
)

### IF I want to select all indices 
indices <- c(
  which(watershed == "Kuskokwim"),
  which(watershed == "Nushagak"),
  which(watershed == "Yukon")
)


# Subsample all outlines, picnames, and watershed to these indices
filtered_outlines <- filtered_outlines[indices]
picnames <- picnames[indices]
watershed <- watershed[indices]

# Initialize lists to store coordinates and associated information
coo <- list()
fac <- data.frame(picname = picnames, watershed = watershed, stringsAsFactors = FALSE)

# Process each outline
for (i in seq_along(filtered_outlines)) {
  shape <- filtered_outlines[[i]]
  x <- shape$X
  y <- shape$Y
  coo[[i]] <- cbind(x, y)
}

# Create the "Coo" object
OtoOutlines <- Out(coo, fac)

################################################################################
################################################################################
################################################################################
# Analysis 

# Visualize raw outlines
stack(OtoOutlines)

# Perform elliptical Fourier transformation on an individual 
coo_oscillo(OtoOutlines[4],"efourier")

## Elliptical Fourier Analysis , with normalization being FALSE 
## nb.h is the # of harmonics
Oto.f<- efourier(OtoOutlines, nb.h = 10, norm = TRUE)


boxplot(Oto.f) #boxplot of harmonics 
Oto.p<- PCA(Oto.f) # run a PCA 
plot(Oto.p, ~watershed) #Plot the PCA 

## GGbiplot
ggbiplot(Oto.p, obs.scale = .5, var.scale = .5, groups = Oto.f$watershed, ellipse = TRUE ,ellipse.linewidth = .3, ellipse.alpha = .1, circle = FALSE) 

# 3d Plot 
plot_ly(
  x = Oto.p$x[,1], 
  y = Oto.p$x[,2], 
  z = Oto.p$x[,3], 
  type = "scatter3d", 
  mode = "markers", 
  marker = list(size = 3),  # Adjust size here
  color = Oto.f$watershed
)

# make watershed a factor 
Oto.f$watershed <- as.factor(Oto.f$watershed)

#### Try the PCA, without the first harmonic
Oto.f.2 <- rm_harm(Oto.f, 1)
Oto.p.2<- PCA(Oto.f.2)
plot(Oto.p.2, ~watershed)


## try to align 
#Oto.al<-fgProcrustes(OtoOutlines) ### Doesnt work because all of the outlines dont have the same size 


panel(OtoOutlines, fac = "watershed", names = FALSE)
scree_plot(Oto.p) #Scree plot, contribution of PC?
#boxplot(Oto.p, 1) # UNCLEAR, boxplot?

PCcontrib(Oto.p,1:8) #Visual contribution of PC


##### Linear Discrimination Analysis. 
oto.l<- LDA(Oto.f, ~watershed) #Linear Discrimination Analysis
oto.l
oto.l %>% summary
plot_CV(oto.l) #Confusion matrix


MANOVA(Oto.p, ~watershed) #MANOVA 
## Very significant difference in PCA between Watersheds
MANOVA_PW(Oto.p, ~watershed) #MANOVA pairwise
### No dif between Yukon and Kusko, but significant difference otherwise



### Heirarchial clustering 
CLUST(Oto.p, ~watershed) #Heirarchial clustering

## K means clustering 
KMEANS(Oto.p, centers = 10)

### Mean Shapes, individually 
Oto.f %>% MSHAPES %>% coo_plot() # MEan shape for all 
Oto.ms<- MSHAPES(Oto.f, ~watershed) #Mean shape for each watershed
Out(Oto.ms$shp) %>% panel(names = TRUE) # Mean shapes by watershed

### Put all three on one plot (hypothetically)
Nush<- Oto.ms$shp$Nushagak %>% coo_plot(border = "red")
Yuk<- Oto.ms$shp$Yukon %>% coo_draw(border = "dodgerblue2")
Kus<- Oto.ms$shp$Kuskokwim %>% coo_draw(border = "darkgreen")

# Direct comparison of them #Example 
#leaves <- shapes %>% slice(grep("leaf", names(shapes))) %$% coo

OtoOutlines %>% efourier(6) %>% MSHAPES(~watershed) %>% plot_MSHAPES()


################ Attempt at adding standard deviation to the mean shapes 

install.packages("shapes")
library(shapes)

# Load necessary libraries
library(tidyverse)
library(shapeR)
library(Momocs)
library(shapes)

# Convert OtoOutlines to a format compatible with the shapes package
shapes_data <- lapply(OtoOutlines$coo, function(x) as.matrix(x))
shapes_array <- array(unlist(shapes_data), dim = c(nrow(shapes_data[[1]]), 2, length(shapes_data)))

# Extract watershed information
watershed <- OtoOutlines$fac$watershed

# Group shapes by watershed
unique_watersheds <- unique(watershed)
grouped_shapes <- lapply(unique_watersheds, function(w) {
  shapes_array[,,watershed == w]
})

# Calculate mean shapes for each watershed
mean_shapes <- lapply(grouped_shapes, function(shapes) {
  apply(shapes, c(1, 2), mean)
})

# Calculate standard deviation for each watershed
std_dev_shapes <- lapply(1:length(unique_watersheds), function(i) {
  shapes <- grouped_shapes[[i]]
  mean_shape <- mean_shapes[[i]]
  deviations <- apply(shapes, 3, function(shape) {
    shape - mean_shape
  })
  sqrt(apply(deviations^2, c(1, 2), mean))
})

# Visualize mean shapes with standard deviation
par(mfrow = c(1, length(unique_watersheds)))  # Arrange plots in a row
for (i in 1:length(unique_watersheds)) {
  # Plot mean shape
  plot(mean_shapes[[i]], type = "l", col = "blue", lwd = 2, main = unique_watersheds[i],
       xlab = "X", ylab = "Y", xlim = range(shapes_array[,1,]), ylim = range(shapes_array[,2,]))
  
  # Add standard deviation as a shaded region
  polygon(
    x = c(mean_shapes[[i]][,1] + std_dev_shapes[[i]][,1], rev(mean_shapes[[i]][,1] - std_dev_shapes[[i]][,1])),
    y = c(mean_shapes[[i]][,2] + std_dev_shapes[[i]][,2], rev(mean_shapes[[i]][,2] - std_dev_shapes[[i]][,2])),
    col = rgb(0, 0, 1, 0.2), border = NA
  )
}


# 
# 
# # Add landmarks
# OtoOutlinesldk <- def_ldk(OtoOutlines, 2)
# OtoOutlinesldk <- add_ldk(OtoOutlinesldk,1)
# 
# # Align outlines using Procrustes analysis
# OtoOutlines_aligned <- fgProcrustes(OtoOutlinesldk)
# 
# # Visualize aligned outlines
# stack(OtoOutlines_aligned)
# 
# # Slide outlines to a common starting point
# OtoOutlines_aligned <- coo_slide(OtoOutlines_aligned, ldk = 1)
# 
# # Perform elliptical Fourier transformation
# OtoOutlines_aligned <- efourier(OtoOutlines, 10, norm = FALSE)
# 
# # Perform Principal Component Analysis
# OtoOutlines_pca <- PCA(OtoOutlines_aligned)
# 
# # Plot PCA results
# plot_PCA(OtoOutlines_pca)
# plot_PCA(OtoOutlines_pca, ~watershed)
# 
# # Perform Linear Discriminant Analysis
# OtoOutlines_lda <- LDA(OtoOutlines_pca, ~watershed)
# 
# # Plot LDA results
# plot_CV(OtoOutlines_lda)
# 
# #Harmonics contribution 
# hcontrib(OtoOutlines_aligned)
# 
# panel(OtoOutlines, fac = "watershed", names = FALSE)


