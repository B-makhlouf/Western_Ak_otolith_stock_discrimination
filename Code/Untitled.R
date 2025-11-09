# Load required libraries
library(readxl)
library(ggplot2)
library(dplyr)

# Read the data
data <- read_excel("/Users/benjaminmakhlouf/Research_repos/Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/AYKEscapement.xlsx")

# View the structure of the data
str(data)
head(data)

# Create a 3-panel figure showing total returns per year for each watershed
p <- ggplot(data, aes(x = Year, y = Total_Return)) +
  geom_line(linewidth = 1, color = "steelblue") +
  geom_point(size = 2, color = "steelblue", alpha = 0.7) +
  facet_wrap(~ River, ncol = 1, scales = "free_y") +
  labs(
    title = "Chinook Salmon Total Returns by Watershed",
    x = "Year",
    y = "Total Return (number of fish)"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    strip.background = element_rect(fill = "lightgray"),
    strip.text = element_text(face = "bold", size = 12),
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(labels = scales::comma)

# Display the plot
print(p)

# Save the plot
ggsave(
  filename = "/Users/benjaminmakhlouf/Research_repos/Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/salmon_returns_3panel.png",
  plot = p,
  width = 10,
  height = 8,
  dpi = 300
)

# Optional: Create a version with all three watersheds on the same panel for comparison
p2 <- ggplot(data, aes(x = Year, y = Total_Return, color = River)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2, alpha = 0.7) +
  labs(
    title = "Chinook Salmon Total Returns Comparison",
    x = "Year",
    y = "Total Return (number of fish)",
    color = "Watershed"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(labels = scales::comma) +
  scale_color_brewer(palette = "Set1")

# Display the comparison plot
print(p2)

# Save the comparison plot
ggsave(
  filename = "/Users/benjaminmakhlouf/Research_repos/Schindler_GitHub/Arctic_Yukon_Kuskokwim_Data/salmon_returns_comparison.png",
  plot = p2,
  width = 10,
  height = 6,
  dpi = 300
)

# Print summary statistics by watershed
cat("\n=== Summary Statistics by Watershed ===\n\n")
summary_stats <- data %>%
  group_by(River) %>%
  summarize(
    Years = n(),
    Min_Return = min(Total_Return, na.rm = TRUE),
    Max_Return = max(Total_Return, na.rm = TRUE),
    Mean_Return = mean(Total_Return, na.rm = TRUE),
    Median_Return = median(Total_Return, na.rm = TRUE),
    SD_Return = sd(Total_Return, na.rm = TRUE)
  )

print(summary_stats)