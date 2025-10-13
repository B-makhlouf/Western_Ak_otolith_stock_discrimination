library(magick)

# File paths
pdf_path <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/Figures/BasinLayout.pdf"
jpg_path <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/Figures/Isoscape.jpg"
output_path <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/Figures/BasinLayout_Isoscape_Combined_Labeled.pdf"

# Read PDF at lower density to match typical JPG resolution
img_pdf <- image_read_pdf(pdf_path, density = 150, pages = 1)
img_jpg <- image_read(jpg_path)

# Get dimensions
info_pdf <- image_info(img_pdf)
info_jpg <- image_info(img_jpg)

# Decide on a target width (use the larger of the two, or set a specific value)
target_width <- max(info_pdf$width, info_jpg$width)

# Resize BOTH images to the exact same width
img_pdf <- image_resize(img_pdf, geometry_size_pixels(width = target_width))
img_jpg <- image_resize(img_jpg, geometry_size_pixels(width = target_width))

# Verify they're the same width now
info_pdf <- image_info(img_pdf)
info_jpg <- image_info(img_jpg)
print(paste("PDF width:", info_pdf$width, "height:", info_pdf$height))
print(paste("JPG width:", info_jpg$width, "height:", info_jpg$height))

# --- Simple function with absolute values for everything ---
add_label_absolute <- function(img, label, size = 120, x_offset = 50, y_offset = 50, color = "black") {
  image_annotate(
    img, label,
    gravity = "southwest",
    size = size,
    color = color,
    font = "Helvetica",
    weight = 700,
    location = paste0("+", x_offset, "+", y_offset)
  )
}

# Add labels with identical parameters
label_size <- 120
x_pos <- 50
y_pos <- 50

img_pdf_labeled <- add_label_absolute(img_pdf, "A", size = label_size, x_offset = x_pos, y_offset = y_pos)
img_jpg_labeled <- add_label_absolute(img_jpg, "B", size = label_size, x_offset = x_pos, y_offset = y_pos)

# Stack vertically
combined <- image_append(c(img_pdf_labeled, img_jpg_labeled), stack = TRUE)

# Save outputs
image_write(combined, path = output_path, format = "pdf")
image_write(combined, path = sub(".pdf", ".png", output_path), format = "png", quality = 100, density = 150)









#############################

library(magick)

# File paths (swapped order - PCA first)
pdf1_path <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/Figures/PCA/SAME_NO_7080_7085_2d_plots/SAME_NO_7080_7085_Combined_PCA_Views.pdf"
pdf2_path <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/Figures/PCA/SAME_NO_7080_7085_ts_loadings/Specific_Four_Individuals_SAME_NO_7080_7085_Comparison.pdf"
output_path <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/Figures/PCA_Combined_Horizontal.pdf"

# Read both PDFs at the same density
density <- 150
img1 <- image_read_pdf(pdf1_path, density = density, pages = 1)
img2 <- image_read_pdf(pdf2_path, density = density, pages = 1)

# Get dimensions
info1 <- image_info(img1)
info2 <- image_info(img2)

# Make them the same HEIGHT (so they align nicely horizontally)
target_height <- max(info1$height, info2$height)

img1 <- image_resize(img1, geometry_size_pixels(height = target_height))
img2 <- image_resize(img2, geometry_size_pixels(height = target_height))

# Verify dimensions
info1 <- image_info(img1)
info2 <- image_info(img2)
print(paste("Image 1 - width:", info1$width, "height:", info1$height))
print(paste("Image 2 - width:", info2$width, "height:", info2$height))

# Function to add label
add_label_absolute <- function(img, label, size = 80, x_offset = 30, y_offset = 30, color = "black") {
  image_annotate(
    img, label,
    gravity = "southwest",
    size = size,
    color = color,
    font = "Helvetica",
    weight = 700,
    location = paste0("+", x_offset, "+", y_offset)
  )
}

# Add labels with smaller size
label_size <- 60  # Reduced from 120
x_pos <- 30       # Reduced from 50
y_pos <- 30       # Reduced from 50

img1_labeled <- add_label_absolute(img1, "A", size = label_size, x_offset = x_pos, y_offset = y_pos)
img2_labeled <- add_label_absolute(img2, "B", size = label_size, x_offset = x_pos, y_offset = y_pos)

# Append HORIZONTALLY (stack = FALSE)
combined <- image_append(c(img1_labeled, img2_labeled), stack = FALSE)

# Save outputs
image_write(combined, path = output_path, format = "pdf")
image_write(combined, path = sub(".pdf", ".png", output_path), format = "png", quality = 100, density = 150)

print(paste("Combined figure saved to:", output_path))

