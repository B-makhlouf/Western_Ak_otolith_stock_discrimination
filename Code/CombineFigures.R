library(magick)

# ===== FIRST FIGURE: Basin Layout and Isoscape (2 rows) =====
# File paths
pdf_path <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/Figures/BasinLayout_Isoscape.pdf"
jpg_path <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/Figures/BasinLayout.pdf"
output_path1 <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/Figures/BasinLayout_Isoscape_Combined_Labeled.pdf"

# Read BOTH as PDFs at the same density
density <- 150
img_pdf <- image_read_pdf(pdf_path, density = density, pages = 1)
img_jpg <- image_read_pdf(jpg_path, density = density, pages = 1)

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
combined1 <- image_append(c(img_pdf_labeled, img_jpg_labeled), stack = TRUE)

# Save outputs
image_write(combined1, path = output_path1, format = "pdf")
image_write(combined1, path = sub(".pdf", ".png", output_path1), format = "png", quality = 100, density = 150)
print(paste("First combined figure saved to:", output_path1))


# ===== SECOND FIGURE: PCA plots (1 row, 2 panels) =====
# File paths for PCA figures
pca_path1 <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/Figures/PCA/SAME_NO_7080_7085_2d_plots/SAME_NO_7080_7085_Combined_PCA_Views_Enhanced.pdf"
pca_path2 <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/Figures/PCA/SAME_NO_7080_7085_ts_loadings/Specific_Four_Individuals_SAME_NO_7080_7085_Comparison.pdf"
output_path2 <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/Figures/PCA_Combined_2Panel.pdf"

# Read BOTH PDFs at the same density
img_pca1 <- image_read_pdf(pca_path1, density = density, pages = 1)
img_pca2 <- image_read_pdf(pca_path2, density = density, pages = 1)

# Get dimensions
info_pca1 <- image_info(img_pca1)
info_pca2 <- image_info(img_pca2)

# Decide on a target HEIGHT (for horizontal stacking, match heights)
target_height <- max(info_pca1$height, info_pca2$height)

# Resize BOTH images to the exact same height
img_pca1 <- image_resize(img_pca1, geometry_size_pixels(height = target_height))
img_pca2 <- image_resize(img_pca2, geometry_size_pixels(height = target_height))

# Verify they're the same height now
info_pca1 <- image_info(img_pca1)
info_pca2 <- image_info(img_pca2)
print(paste("PCA1 width:", info_pca1$width, "height:", info_pca1$height))
print(paste("PCA2 width:", info_pca2$width, "height:", info_pca2$height))

# Add labels (reusing the same function from above)
img_pca1_labeled <- add_label_absolute(img_pca1, "A", size = label_size, x_offset = x_pos, y_offset = y_pos)
img_pca2_labeled <- add_label_absolute(img_pca2, "B", size = label_size, x_offset = x_pos, y_offset = y_pos)

# Stack HORIZONTALLY (stack = FALSE for side-by-side)
combined2 <- image_append(c(img_pca1_labeled, img_pca2_labeled), stack = FALSE)

# Save outputs
image_write(combined2, path = output_path2, format = "pdf")
image_write(combined2, path = sub(".pdf", ".png", output_path2), format = "png", quality = 100, density = 150)
print(paste("Second combined figure saved to:", output_path2))