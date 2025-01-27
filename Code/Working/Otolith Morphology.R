library(dplyr)
library(shapeR)

shape = shapeR("/Users/benjaminmakhlouf/Desktop/ShapeAnalysis", "FISH.csv")
shape = detect.outline(shape, threshold = 0.2, write.outline.w.org = TRUE)
shape = generateShapeCoefficients(shape)
shape = enrich.master.list(shape) #connect to the metadata

#remove individuals with NA from masterlist
masterlist<- masterlist(shape)

plotWaveletShape(shape, "Watershed", show.angle = TRUE, lwd = 2,lty = 1)
Watershed = factor(getMasterlist(shape)$Watershed) #Get factor of interest (Watershed)



####################### LDA classification
################################################################################


library(ipred)

#remove rows with NA from masterlist 
masterlist<- masterlist[complete.cases(masterlist),]


wavelet<- masterlist[10:118]
labels<- masterlist$Watershed

# run a PCA on the wavelet data
pca<- prcomp(wavelet, center = TRUE, scale = TRUE)
summary(pca)
pca_results<- as.data.frame(pca$x)

# Add labels
pca_results$Watershed<- labels

# Plot principal component 1 vs 2 and color by labels 
pca_plot <- ggplot(pca_results, aes_string(x = pca_results$PC2, y = pca_results$PC3, color = "Watershed")) +
  geom_point(size = 3, alpha = .8) +
  theme_classic() +
  labs(title = "PCA of Iso Values by Watershed")+
  theme(legend.title = element_blank())

pca_plot
