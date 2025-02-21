##### This script is to try to use the "Momocs" package to do otolith morphological 
# analysis 

install.packages("Momocs")
library(Momocs)
library(tidyverse)
library(shapeR)

### Use ShapeR to pull out the outline

shape = shapeR("/Users/benjaminmakhlouf/Research_repos/Western_Ak_otolith_stock_discrimination/ShapeAnalysis", "FISH.csv")
outlinesonly = detect.outline(shape, threshold = 0.2, write.outline.w.org = FALSE)

### pull out all the outlines for each class 
yk_outlines = outlinesonly@outline.list$YK
kk_outlines = outlinesonly@outline.list$KK
nk_outlines = outlinesonly@outline.list$NK

#########################################################################
# Combine all the outlines into one list
all_outlines <- c(kk_outlines, nk_outlines, yk_outlines)

# Extract picnames and watersheds
picnames <- outlinesonly@master.list.org$picname
watershed <- outlinesonly@master.list.org$Watershed

# Initialize an empty list to store the coordinates
coo <- list()

# Initialize an empty data frame to store the associated information
fac <- data.frame(picname = character(), watershed = character(), stringsAsFactors = FALSE)

# Loop through each outline and process it
for (i in seq_along(all_outlines)) {
  shape <- all_outlines[[i]]
  
  # Process the shape to extract x, y coordinates
  x <- shape$X
  y <- shape$Y
  coo[[i]] <- cbind(x, y)
  
  # Add the corresponding picname and watershed to the fac data frame
  fac <- rbind(fac, data.frame(picname = picnames[i], watershed = watershed[i], stringsAsFactors = FALSE))
}

# remove OtoOutlines if it exists 
rm(OtoOutlines)
# Create the "Coo" object, which MOMOCS needs to run analysis 
OtoOutlines <- Out(coo, fac)


################################################################################

### to simplify to get workflow, select 4 otoliths 
OtoOutlines <- slice(OtoOutlines, 1:4)

OtoOutlines %>%
  stack() #Take a "Family Picture" of raw outlines 

# add ldk to "coo" object


### Add landmarks, which will be the furthest point on the rostrum and anti rostrum. 
OtoOutlinesldk<- def_ldk(OtoOutlines, 2) #Define? 
OtoOutlinesldk<- add_ldk(OtoOutlinesldk, 2) # then add? 

#take a look 
stack(OtoOutlinesldk)

OtoOutlines_aligned<- OtoOutlinesldk %>%
  fgProcrustes()

stack(OtoOutlines_aligned)

### At this point, they should be aligned , I might consider a spot near the top and or/bottom

OtoOutlines_aligned <- OtoOutlines_aligned %>%
  coo_slide(ldk = 2) #Define the starting point, in this case the second landmark. 

### According to the Vignette: 
# no ldk passed and a single id is passed: all id-th points within the shapes will become the
#first points. $ldk will be slided accordingly.

#no ldk passed and a vector of ids matching the length of the Coo: for every shape, the id-th
#point will be used as the id-th point. $ldk will be slided accordingly.

#a single ldk is passed: the ldk-th ldk will be used to slide every shape. If an id is (also) passed,
#it is ignored with a message.

##### Run the elliptical fourier tranformation 
OtoOutlines_aligned<- OtoOutlines_aligned %>%
  efourier(6, norm=FALSE)  # Elliptical Fourier Transforms

#nb.h integer. The number of harmonics to use. If missing, 12 is used on shapes; 99
#percent of harmonic power on Out objects, both with messages

#norm : whether to normalize the coefficients using efourier_norm

OtoOutlined_aligned<- OtoOutlines_aligned %>%
  PCA() %>% # Principal Component Analysis 
  plot_PCA() # A PC1:2 plot



