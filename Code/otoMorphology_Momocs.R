# Load required libraries
#install.packages("devtools")

#devtools::install_github("geomorphR/geomorph")
#install.packages("rgl")

# install.packages("devtools")
#devtools::install_github("MomX/Momocs")
#install.packages("shapeR")
#install.packages("tidyverse")
#install.packages("shapeR")
library(tidyverse)
library(shapeR)
library(Momocs)
install.packages("Momocs")


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

# Randomly choose 20 watershed indices from each watershed
indices <- c(
  sample(which(watershed == "Kuskokwim"), 10),
  sample(which(watershed == "Nushagak"), 10),
  sample(which(watershed == "Yukon"), 10)
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

# Ensure `fac` has the correct number of rows
fac <- fac[1:length(coo), ]

# Verify lengths
print(length(coo))  # Should match nrow(fac)
print(nrow(fac))    # Should match length(coo)

# Create the "Coo" object
OtoOutlines <- Out(coo, fac)

# Visualize raw outlines
stack(OtoOutlines)

# Add landmarks
OtoOutlinesldk <- def_ldk(OtoOutlines, 1)
OtoOutlinesldk <- add_ldk(OtoOutlinesldk,1)

# Visualize outlines with landmarks
stack(OtoOutlinesldk)

# Align outlines using Procrustes analysis
OtoOutlines_aligned <- fgProcrustes(OtoOutlinesldk)


# Visualize aligned outlines
stack(OtoOutlines_aligned)

# Slide outlines to a common starting point
OtoOutlines_aligned <- coo_slide(OtoOutlines_aligned, ldk = 2)

# Perform elliptical Fourier transformation
OtoOutlines_aligned <- efourier(OtoOutlines, 6, norm = FALSE)
coo_check(OtoOutlines)


# Perform Principal Component Analysis
OtoOutlines_pca <- PCA(OtoOutlines_aligned)

# Plot PCA results
plot_PCA(OtoOutlines_pca)
plot_PCA(OtoOutlines_pca, ~watershed)

# Perform Linear Discriminant Analysis
OtoOutlines_lda <- LDA(OtoOutlines_pca, ~watershed)

# Plot LDA results
plot_CV(OtoOutlines_lda)

#hpow(OtoOutlines_lda)
install.packages("FactoClass")
library(FactoClass)
library(ade4)

fourier<- efourier(OtoOutlines, 6, norm = FALSE)
dudi.plot(fourier, pos.shp = "circle", neighbors = TRUE)


dudi.plot()

dudi.plot(OtoOutlines)
#hqual(
#  OtoOutlines_aligned, 
#  method = "eFourier", 
#  id = 16, 
#  harm.range = 1:49,
#  palette = col.sari, 
#  plot.method = "panel"
#)


data(bot)
botF <- efourier(bot, nb.h=32)
botD <- pca(botF)
dudi.plot(botD)
dudi.plot(botD, 1, title="botD with no class but with ellipses")
dudi.plot(botD, fac=1, chull=TRUE, rug=FALSE, shape=FALSE, title="botD with convex hull")
dudi.plot(botD, fac=1, ellipses=FALSE, neighbors=TRUE, shapes=FALSE, star=FALSE,
          col.nei="black", title="botD with Gabriel's neighboring graph")
dudi.plot(botD, labels=TRUE, points=FALSE, boxes=FALSE, shapes=TRUE, pos.shp="li",
          title="botD with labels and reconstructed shapes")
dudi.plot(botD, 1, points=FALSE, labels=TRUE, boxes=FALSE, shapes=FALSE,
          title="botD with labels and ellipse")
dudi.plot(botD, 1, arrows=TRUE, dratio.arrow=0.2, shapes=FALSE,
          title="botD with harmonic correlations")
# With some fake factors
botD <- pca(botF)
dudi.plot(botD, "type", palette=col.gallus,
          rotate.shp=pi/2, title="botD with classes") # rotated shapes
dudi.plot(botD, "type", palette=col.gallus, eigen=TRUE, title="botD with eigen values")
dudi.plot(botD, "type", pos.shp="full", title="botD with shapes(1)")
dudi.plot(botD, "type", pos.shp="range", scale.shp=0.5, shapes=TRUE,
          border.shp="firebrick3", col.shp=NA, center.orig=TRUE, 
          zoom.plot=0.8, title="botD with shapes(2)")
dudi.plot(botD, "type", pos.shp="circle", center.orig=TRUE, title="botD with shapes(3)")
dudi.plot(botD, "type", pos.shp="range", scale.shp=0.5, title="botD with shapes(4)")
dudi.plot(botD, pos.shp=as.matrix(expand.grid(seq(-0.05, 0.05, 0.025),
                                              seq(-0.05, 0.05, 0.025)))) # an example with a matrix provided to pos.shp