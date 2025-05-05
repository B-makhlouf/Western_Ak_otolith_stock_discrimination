# 02_outline_preprocessing.R
# Standardized preprocessing for otolith outline morphology
# Extracts outlines and computes Fourier coefficients

library(Momocs)
library(tidyverse)
library(shapeR)
library(here)

# Function to process otolith outlines
process_otolith_outlines <- function(n_points = 200) {
  # Use ShapeR to extract outlines
  shape <- shapeR(here("ShapeAnalysis"), "FISH.csv")
  outlinesonly <- detect.outline(shape, threshold = 0.2, write.outline.w.org = FALSE)
  
  # Extract metadata
  picnames <- outlinesonly@master.list.org$picname
  watershed <- outlinesonly@master.list.org$Watershed
  
  # Extract outlines for each class
  all_outlines <- c(
    outlinesonly@outline.list$KK,
    outlinesonly@outline.list$NK,
    outlinesonly@outline.list$YK
  )
  
  # Filter bad samples (define this list based on your QC)
  badOtos <- read_csv(here("data/raw/bad_outlines.csv"))$picname
  badOtos_indices <- which(picnames %in% badOtos)
  
  # Remove bad outlines
  if (length(badOtos_indices) > 0) {
    filtered_outlines <- all_outlines[-badOtos_indices]
    picnames <- picnames[-badOtos_indices]
    watershed <- watershed[-badOtos_indices]
  } else {
    filtered_outlines <- all_outlines
  }
  
  # Process all outlines into coordinates
  coo <- list()
  for (i in seq_along(filtered_outlines)) {
    shape <- filtered_outlines[[i]]
    x <- shape$X
    y <- shape$Y
    coo[[i]] <- cbind(x, y)
  }
  
  # Create factor data
  fac <- data.frame(
    picname = picnames, 
    watershed = watershed, 
    stringsAsFactors = FALSE
  )
  
  # Interpolate all outlines to have the same number of points
  coo_interpolated <- lapply(coo, Momocs::coo_interpolate, n = n_points)
  
  # Create the "Coo" object
  OtoOutlines <- Out(coo_interpolated, fac)
  
  # Compute Fourier coefficients
  Oto.fourier <- efourier(OtoOutlines, nb.h = 10, norm = TRUE)
  
  # Extract coefficients as data frame
  coeff_df <- as.data.frame(Oto.fourier$coe)
  coeff_df$watershed <- Oto.fourier$fac$watershed
  coeff_df$picname <- Oto.fourier$fac$picname
  
  # Remove constant variables (if applicable)
  constant_vars <- c(1, 11, 21)  # Define constant variables to be removed
  coeff_df_filtered <- coeff_df[, -constant_vars]
  
  # Create output directory
  outlines_dir <- here("data/processed/outlines")
  dir.create(outlines_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Save processed data
  saveRDS(OtoOutlines, file.path(outlines_dir, "otolith_outlines.rds"))
  saveRDS(Oto.fourier, file.path(outlines_dir, "otolith_fourier.rds"))
  write_csv(coeff_df_filtered, file.path(outlines_dir, "fourier_coefficients.csv"))
  
  message("Outline data processed and saved to: ", outlines_dir)
  
  return(list(
    outlines = OtoOutlines,
    fourier = Oto.fourier,
    coefficients = coeff_df_filtered
  ))
}

# Run the outline processing
outline_data <- process_otolith_outlines()