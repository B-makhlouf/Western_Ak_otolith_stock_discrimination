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
# indices <- c(
#   sample(which(watershed == "Kuskokwim"), 80),
#   sample(which(watershed == "Nushagak"), 80),
#   sample(which(watershed == "Yukon"), 80)
# )

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


num_points <- sapply(coo, nrow)

# Compute the mean number of points per outline
mean_num_points <- mean(num_points)

# Print the result
print(mean_num_points)


n_points <- 6000  # Adjust as needed

# Interpolate all outlines to have the same number of points
coo_interpolated <- lapply(coo, Momocs::coo_interpolate, n = n_points)

# Create the "Coo" object with interpolated outlines
OtoOutlines <- Out(coo_interpolated, fac)

OtoOutlines <- coo_center(OtoOutlines)

# Compute distances from center to each point in each outline
distances <- lapply(OtoOutlines$coo, Momocs::coo_centdist)

library(ggplot2)
library(dplyr)
library(tidyr)

# Convert distance data to a data frame
dist_df <- do.call(rbind, lapply(seq_along(distances), function(i) {
  data.frame(
    picname = fac$picname[i],
    watershed = fac$watershed[i],
    point_id = seq_along(distances[[i]]),
    distance = distances[[i]]
  )
}))

# Compute mean and SD of radial distances by watershed
summary_by_watershed <- dist_df %>%
  group_by(watershed, point_id) %>%
  summarize(
    mean_distance = mean(distance, na.rm = TRUE),
    sd_distance = sd(distance, na.rm = TRUE),
    .groups = "drop"
  )

library(tidyverse)
library(Momocs)

# Convert outlines into a data frame with X, Y coordinates
coo_df <- do.call(rbind, lapply(seq_along(OtoOutlines$coo), function(i) {
  data.frame(
    picname = fac$picname[i],
    watershed = fac$watershed[i],
    point_id = seq_along(OtoOutlines$coo[[i]][, 1]),
    X = OtoOutlines$coo[[i]][, 1],
    Y = OtoOutlines$coo[[i]][, 2]
  )
}))

# Compute mean shape (X, Y) for each watershed
mean_shape <- coo_df %>%
  group_by(watershed, point_id) %>%
  summarize(
    mean_X = mean(X, na.rm = TRUE),
    mean_Y = mean(Y, na.rm = TRUE),
    .groups = "drop"
  )

# Compute distances from the center for each shape
dist_df <- do.call(rbind, lapply(seq_along(OtoOutlines$coo), function(i) {
  data.frame(
    picname = fac$picname[i],
    watershed = fac$watershed[i],
    point_id = seq_along(OtoOutlines$coo[[i]][, 1]),
    distance = sqrt(OtoOutlines$coo[[i]][, 1]^2 + OtoOutlines$coo[[i]][, 2]^2)
  )
}))

# Compute mean and SD of distance for each point
summary_by_watershed <- dist_df %>%
  group_by(watershed, point_id) %>%
  summarize(
    mean_distance = mean(distance, na.rm = TRUE),
    sd_distance = sd(distance, na.rm = TRUE),
    .groups = "drop"
  )

# Merge mean shape and distance variability
shape_summary <- mean_shape %>%
  left_join(summary_by_watershed, by = c("watershed", "point_id"))

# Compute unit vectors (direction) for each point
shape_summary <- shape_summary %>%
  mutate(
    radius = sqrt(mean_X^2 + mean_Y^2),
    unit_X = mean_X / radius,
    unit_Y = mean_Y / radius
  )

# Compute upper and lower bounds by shifting along the radial direction
shape_summary <- shape_summary %>%
  mutate(
    upper_X = mean_X + (sd_distance * unit_X),
    upper_Y = mean_Y + (sd_distance * unit_Y),
    lower_X = mean_X - (sd_distance * unit_X),
    lower_Y = mean_Y - (sd_distance * unit_Y)
  )

# Plot mean shape and SD points
ggplot(shape_summary, aes(x = mean_X, y = mean_Y, color = watershed, group = watershed)) +
  geom_path(linewidth = .9) +  # Mean outline
  geom_point(aes(x = upper_X, y = upper_Y), shape = 16, size = .1, alpha = 0.1) +  # Upper SD points
  geom_point(aes(x = lower_X, y = lower_Y), shape = 16, size = .1, alpha = 0.1) +  # Lower SD points
  coord_equal() +  # Maintain aspect ratio
  facet_wrap(~ watershed, ncol = 1) +  # Facet by watershed
  labs(title = "Mean Otolith Shape with Variability by Watershed", 
       x = "X Coordinate", y = "Y Coordinate") +
  theme_minimal()

ggplot(shape_summary, aes(x = mean_X, y = mean_Y, color = watershed, group = watershed)) +
  geom_path(linewidth = .9) +  # Mean outline
  geom_point(aes(x = upper_X, y = upper_Y), shape = 16, size = 1, alpha = 1) +  # Upper SD points
  geom_point(aes(x = lower_X, y = lower_Y), shape = 16, size = 1, alpha = 1) +  # Lower SD points
  coord_equal() +  # Maintain aspect ratio
  labs(title = "Mean Otolith Shape with Variability by Watershed", 
       x = "X Coordinate", y = "Y Coordinate") +
  theme_minimal()


# Create the "Coo" object
#OtoOutlines <- Out(coo, fac)

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


panel(OtoOutlines, fac = "watershed", names = FALSE)  # ✅ Correct
#panel(Oto.al, fac = "watershed", names = FALSE)

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


