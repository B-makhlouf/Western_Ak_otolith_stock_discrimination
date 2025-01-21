library(dplyr)
library(shapeR)

shape = shapeR("/Users/benjaminmakhlouf/Desktop/ShapeAnalysis", "FISH.csv")
shape = detect.outline(shape, threshold = 0.2, write.outline.w.org = TRUE)
