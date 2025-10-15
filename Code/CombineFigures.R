library(magick)

# File paths
pdf_path <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/Figures/BasinLayout_Isoscape.pdf"
jpg_path <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/Figures/BasinLayout.pdf"  # This is now a PDF
output_path <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/Figures/BasinLayout_Isoscape_Combined_Labeled.pdf"

# Read BOTH as PDFs at the same density
density <- 150
img_pdf <- image_read_pdf(pdf_path, density = density, pages = 1)
img_jpg <- image_read_pdf(jpg_path, density = density, pages = 1)  # Changed to image_read_pdf

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
label_size <- 75
x_pos <- 50
y_pos <- 50

img_pdf_labeled <- add_label_absolute(img_pdf, "A", size = label_size, x_offset = x_pos, y_offset = y_pos)
img_jpg_labeled <- add_label_absolute(img_jpg, "B", size = label_size, x_offset = x_pos, y_offset = y_pos)

# Stack vertically
combined <- image_append(c(img_pdf_labeled, img_jpg_labeled), stack = TRUE)

# Save outputs
image_write(combined, path = output_path, format = "pdf")
image_write(combined, path = sub(".pdf", ".png", output_path), format = "png", quality = 100, density = 150)
