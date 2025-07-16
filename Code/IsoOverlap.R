# Read in the three isoscapes 
library(sf)
YukonIsoscape<- st_read("/Users/benjaminmakhlouf/Spatial Data/Yukon Cleaned Shapefile/Yukon_cleaned.shp")
KuskoIsoscape<- st_read("/Users/benjaminmakhlouf/Spatial Data/kusko_edges_20190805_Prod17_UPriSlp2_accProd17.shp")
NushIsoscape<- st_read("/Users/benjaminmakhlouf/Spatial Data/Shapefiles/AYK Shapefiles/Nushagak_cleaned.shp")
# From each, extract the natal origins 
Yk_iso<- YukonIsoscape$iso_pred
Kusko_iso<- KuskoIsoscape$iso_pred
Nush_iso<- NushIsoscape$iso_pred
library(ggplot2)
library(ggridges)
library(viridis)
library(GGally)
library(plotly)
library(dplyr)
# Filter data to minimum value of 0.70 and combine
iso_data <- data.frame(
  iso_pred = c(Yk_iso[Yk_iso >= 0.70], 
               Kusko_iso[Kusko_iso >= 0.70], 
               Nush_iso[Nush_iso >= 0.70]),
  Watershed = c(rep("Yukon", sum(Yk_iso >= 0.70)),
                rep("Kuskokwim", sum(Kusko_iso >= 0.70)),
                rep("Nushagak", sum(Nush_iso >= 0.70)))
)
# Set factor order: Yukon, Kuskokwim, Nushagak
iso_data$Watershed <- factor(iso_data$Watershed, levels = c("Yukon", "Kuskokwim", "Nushagak"))
# Print sample sizes after filtering
cat("Sample sizes after filtering (≥ 0.70):\n")
iso_data %>% count(Watershed)
# 1. RIDGE PLOT (Recommended) - WITH INCREASED SPACING
ridge_plot <- ggplot(iso_data, aes(x = iso_pred, y = Watershed, fill = Watershed)) +
  geom_density_ridges(alpha = 0.7, scale = 1.2, rel_min_height = 0.01) +
  scale_fill_viridis_d(option = "plasma") +
  scale_x_continuous(limits = c(0.70, 0.73)) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    legend.position = "none"
  ) +
  labs(x = "Isotope Prediction Values", y = "Watershed", 
       title = "Distribution of Isotope Predictions by Watershed (≥ 0.70)")
print(ridge_plot)

# what is the maximum value of the kusko? 
max_kusko <- max(Kusko_iso, na.rm = TRUE)




