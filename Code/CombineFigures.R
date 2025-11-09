library(magick)

# ===== FIRST FIGURE: Basin Layout and Isoscape (2 rows) =====
# File paths
png_path1 <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/Figures/isoscape.png"
png_path2 <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/Figures/Layout.png"
output_path1 <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/Figures/BasinLayout_Isoscape_Combined_Labeled.png"

# Read both PNG images
img_png1 <- image_read(png_path1)
img_png2 <- image_read(png_path2)

# Get dimensions
info_png1 <- image_info(img_png1)
info_png2 <- image_info(img_png2)

# Decide on a target width (use the larger of the two, or set a specific value)
target_width <- max(info_png1$width, info_png2$width)

# Resize BOTH images to the exact same width
img_png1 <- image_resize(img_png1, geometry_size_pixels(width = target_width))
img_png2 <- image_resize(img_png2, geometry_size_pixels(width = target_width))

# Verify they're the same width now
info_png1 <- image_info(img_png1)
info_png2 <- image_info(img_png2)
print(paste("PNG1 width:", info_png1$width, "height:", info_png1$height))
print(paste("PNG2 width:", info_png2$width, "height:", info_png2$height))

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

# Add labels with MUCH LARGER size
label_size <- 200  # Increased from 75 to 200
x_pos <- 100       # Increased offset slightly
y_pos <- 100       # Increased offset slightly

img_png1_labeled <- add_label_absolute(img_png1, "A", size = label_size, x_offset = x_pos, y_offset = y_pos)
img_png2_labeled <- add_label_absolute(img_png2, "B", size = label_size, x_offset = x_pos, y_offset = y_pos)

# Stack vertically
combined1 <- image_append(c(img_png1_labeled, img_png2_labeled), stack = TRUE)

# Save output as PNG
image_write(combined1, path = output_path1, format = "png", quality = 100)

print(paste("Combined figure saved to:", output_path1))






# ===== SECOND FIGURE: PCA plots (1 row, 2 panels) =====
# File paths for PCA figures

label_size <- 75  # Increased from 75 to 200
x_pos <- 100       # Increased offset slightly
y_pos <- 100       # Increased offset slightly

pca_path1 <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/Figures/PCA Figures/SAME_NO_7080_7085_Combined_PCA_Views_Enhanced.pdf"
pca_path2 <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/Figures/PCA Figures/Four_Panel_PCA_Loadings_Comparison.pdf"
output_path2 <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/Figures/PCA Figures/PCA_Combined_2Panel.pdf"

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
