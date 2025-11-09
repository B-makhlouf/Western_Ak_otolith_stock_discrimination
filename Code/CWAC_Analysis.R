################################################################################
# CWAK vs non-CWAK Classification Performance Analysis
################################################################################
#
# PURPOSE:
#   Test the classification accuracy of GAM Random Forest models for separating
#   CWAK from non-CWAK fish based on otolith chemistry.
#
# DEFINITIONS:
#   - CWAK: Lower Yukon + All Kuskokwim + All Nushagak
#   - non-CWAK: Middle Yukon + Upper Yukon
#
# INPUTS:
#   1. Metadata with genetic assignments (Lower_gen, Middle_gen, Upper_gen)
#   2. GAM Random Forest predictions (Total and Overlap analyses)
#
# OUTPUTS:
#   - Accuracy statistics for CWAK vs non-CWAK groups
#   - Component breakdown (Lower Yukon, Kuskokwim, Nushagak)
#   - Visualization plots
#
################################################################################

# Load required libraries
library(tidyverse)
library(ggplot2)

################################################################################
# CONFIGURATION
################################################################################

# File paths (UPDATE THESE FOR YOUR SYSTEM)
METADATA_PATH <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/Data/Final/Metadata_and_QC.csv"
PREDICTIONS_TOTAL_PATH <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/Output/ModelResultsPreCal/Total/GAM_RF_TOTAL_predictions.csv"
PREDICTIONS_OVERLAP_PATH <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/Output/ModelResultsPreCal/Filtered/GAM_RF_OVERLAP_predictions.csv"

# Output directory
OUTPUT_DIR <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/Figures/CWAK Analysis"

################################################################################
# STEP 1: LOAD AND PREPARE METADATA
################################################################################

cat(strrep("=", 80), "\n")
cat("CWAK vs non-CWAK CLASSIFICATION ANALYSIS\n")
cat(strrep("=", 80), "\n\n")

cat("STEP 1: Loading metadata with genetic information...\n")

metadata <- read.csv(METADATA_PATH)
cat("  Loaded", nrow(metadata), "fish from metadata\n\n")

################################################################################
# STEP 2: ASSIGN GENETIC GROUPS
################################################################################

cat("STEP 2: Assigning genetic groups based on highest probability...\n")

# For each fish, assign genetic group based on highest probability among
# Lower_gen, Middle_gen, and Upper_gen
metadata <- metadata %>%
  rowwise() %>%
  mutate(
    genetic_group = {
      probs <- c(Lower = Lower_gen, Middle = Middle_gen, Upper = Upper_gen)
      probs <- probs[!is.na(probs)]
      if(length(probs) > 0) {
        names(probs)[which.max(probs)]
      } else {
        NA_character_
      }
    }
  ) %>%
  ungroup()

# Standardize column names
metadata <- metadata %>%
  rename(
    Fish_id = Fish_ID,
    Natal_Iso = Natal_origins_iso
  )

cat("  Genetic group assignments:\n")
print(table(metadata$genetic_group, useNA = "ifany"))
cat("\n")

################################################################################
# STEP 3: LOAD PREDICTION RESULTS
################################################################################

cat("STEP 3: Loading GAM Random Forest predictions...\n")

# Load predictions from both analyses
predictions_total <- read.csv(PREDICTIONS_TOTAL_PATH) %>%
  mutate(analysis = "Total")

predictions_overlap <- read.csv(PREDICTIONS_OVERLAP_PATH) %>%
  mutate(analysis = "Overlap")

cat("  Total analysis:", nrow(predictions_total), "predictions\n")
cat("  Overlap analysis:", nrow(predictions_overlap), "predictions\n\n")

################################################################################
# STEP 4: MERGE PREDICTIONS WITH METADATA
################################################################################

cat("STEP 4: Merging predictions with genetic information...\n")

# Combine all predictions
all_predictions <- bind_rows(predictions_total, predictions_overlap)

# Merge with metadata to get genetic groups
all_predictions <- all_predictions %>%
  left_join(
    metadata %>% select(Fish_id, genetic_group, Lower_gen, Middle_gen, Upper_gen),
    by = "Fish_id"
  )

cat("  Merged dataset:", nrow(all_predictions), "predictions\n")
cat("  Fish with genetic data:", sum(!is.na(all_predictions$genetic_group)), "\n\n")

################################################################################
# STEP 5: CREATE CWAK GROUPINGS
################################################################################

cat("STEP 5: Creating CWAK vs non-CWAK groupings...\n")

all_predictions <- all_predictions %>%
  mutate(
    cwak_group = case_when(
      # CWAK group
      Watershed == "Kusko" ~ "CWAK",
      Watershed == "Nush" ~ "CWAK",
      Watershed == "Yukon" & genetic_group == "Lower" ~ "CWAK",
      
      # non-CWAK group
      Watershed == "Yukon" & genetic_group %in% c("Middle", "Upper") ~ "non-CWAK",
      
      # Unassigned (Yukon without genetic data)
      TRUE ~ NA_character_
    )
  )

cat("  CWAK grouping summary:\n")
print(table(all_predictions$cwak_group, useNA = "ifany"))
cat("\n")

# Filter to only fish with clear CWAK classification
cwak_data <- all_predictions %>%
  filter(!is.na(cwak_group))

cat("  Fish available for CWAK analysis:", nrow(cwak_data), "\n\n")

################################################################################
# STEP 6: CALCULATE OVERALL ACCURACY BY CWAK GROUP
################################################################################

cat("STEP 6: Calculating classification accuracy by CWAK group...\n")

cwak_accuracy <- cwak_data %>%
  group_by(analysis, cwak_group) %>%
  summarise(
    n_fish = n(),
    correct = sum(Watershed == .pred_class),
    accuracy = correct / n_fish,
    .groups = "drop"
  )

cat("\n")
cat(strrep("=", 80), "\n")
cat("CWAK vs non-CWAK ACCURACY RESULTS\n")
cat(strrep("=", 80), "\n")
print(cwak_accuracy)
cat("\n")

################################################################################
# STEP 7: BREAKDOWN BY CWAK COMPONENTS
################################################################################

cat("STEP 7: Breaking down CWAK components...\n")

# Analyze each component of the CWAK group separately
cwak_components <- cwak_data %>%
  filter(cwak_group == "CWAK") %>%
  mutate(
    cwak_component = case_when(
      Watershed == "Kusko" ~ "All Kuskokwim",
      Watershed == "Nush" ~ "All Nushagak",
      Watershed == "Yukon" & genetic_group == "Lower" ~ "Lower Yukon",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(cwak_component)) %>%
  group_by(analysis, cwak_component) %>%
  summarise(
    n_fish = n(),
    correct = sum(Watershed == .pred_class),
    accuracy = correct / n_fish,
    .groups = "drop"
  )

cat("\n")
cat(strrep("=", 80), "\n")
cat("CWAK COMPONENT ACCURACY\n")
cat(strrep("=", 80), "\n")
print(cwak_components)
cat("\n")

################################################################################
# STEP 8: CREATE VISUALIZATIONS
################################################################################

cat("STEP 8: Creating visualizations...\n\n")

# Create output directory if it doesn't exist
if(!dir.exists(OUTPUT_DIR)) {
  dir.create(OUTPUT_DIR, recursive = TRUE)
  cat("  Created output directory:", OUTPUT_DIR, "\n\n")
}

# Define color palette
colors <- c("Total" = "#2E86AB", "Overlap" = "#A23B72")

# -----------------------------------------------------------------------------
# FIGURE 1: CWAK vs non-CWAK Comparison (Both Analyses)
# -----------------------------------------------------------------------------

plot1 <- ggplot(cwak_accuracy, aes(x = cwak_group, y = accuracy, fill = analysis)) +
  geom_col(position = "dodge", alpha = 0.8, width = 0.6) +
  geom_text(
    aes(label = paste0(sprintf("%.1f%%", accuracy * 100), "\n(n=", n_fish, ")")),
    position = position_dodge(width = 0.6), 
    vjust = -0.2, 
    size = 4, 
    fontface = "bold"
  ) +
  scale_y_continuous(
    labels = scales::percent_format(), 
    limits = c(0, 1.1)
  ) +
  scale_fill_manual(values = colors) +
  labs(
    title = "Classification Accuracy: CWAK vs non-CWAK Groups",
    subtitle = "CWAK = Lower Yukon + All Kuskokwim + All Nushagak\nnon-CWAK = Middle Yukon + Upper Yukon",
    x = "Group",
    y = "Proportion Correctly Classified",
    fill = "Analysis Type"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 11, color = "gray50"),
    axis.title = element_text(face = "bold", size = 12),
    axis.text = element_text(size = 11),
    legend.position = "bottom",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

print(plot1)

# Save plot
ggsave(
  filename = file.path(OUTPUT_DIR, "CWAK_vs_nonCWAK_comparison.png"),
  plot = plot1,
  width = 10,
  height = 6,
  dpi = 300
)
cat("  Saved Figure 1: CWAK_vs_nonCWAK_comparison.png\n")

# -----------------------------------------------------------------------------
# FIGURE 2: Total Analysis Only (Simplified)
# -----------------------------------------------------------------------------

total_only_data <- cwak_accuracy %>% 
  filter(analysis == "Total")

if(nrow(total_only_data) > 0) {
  plot2 <- ggplot(total_only_data, aes(x = cwak_group, y = accuracy)) +
    geom_col(fill = "#2E86AB", alpha = 0.8, width = 0.5) +
    geom_text(
      aes(label = paste0(sprintf("%.1f%%", accuracy * 100), "\n(n=", n_fish, ")")),
      vjust = -0.2, 
      size = 5, 
      fontface = "bold", 
      color = "black"
    ) +
    scale_y_continuous(
      labels = scales::percent_format(), 
      limits = c(0, 1.1)
    ) +
    labs(
      title = "Classification Accuracy: CWAK vs non-CWAK Groups",
      subtitle = "Total Analysis Only\nCWAK = Lower Yukon + All Kuskokwim + All Nushagak",
      x = "Group",
      y = "Proportion Correctly Classified"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
      plot.subtitle = element_text(hjust = 0.5, size = 11, color = "gray50"),
      axis.title = element_text(face = "bold", size = 12),
      axis.text = element_text(size = 11),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank()
    )
  
  print(plot2)
  
  # Save plot
  ggsave(
    filename = file.path(OUTPUT_DIR, "CWAK_vs_nonCWAK_total_only.png"),
    plot = plot2,
    width = 8,
    height = 6,
    dpi = 300
  )
  cat("  Saved Figure 2: CWAK_vs_nonCWAK_total_only.png\n")
}

# -----------------------------------------------------------------------------
# FIGURE 3: CWAK Component Breakdown
# -----------------------------------------------------------------------------

if(nrow(cwak_components) > 0) {
  plot3 <- ggplot(
    cwak_components, 
    aes(x = reorder(cwak_component, accuracy), y = accuracy, fill = analysis)
  ) +
    geom_col(position = "dodge", alpha = 0.8, width = 0.7) +
    geom_text(
      aes(label = paste0(sprintf("%.1f%%", accuracy * 100), "\n(n=", n_fish, ")")),
      position = position_dodge(width = 0.7), 
      hjust = -0.1, 
      size = 3.5, 
      fontface = "bold"
    ) +
    scale_y_continuous(
      labels = scales::percent_format(), 
      limits = c(0, 1.2)
    ) +
    scale_fill_manual(values = colors) +
    coord_flip() +
    labs(
      title = "Classification Accuracy by CWAK Components",
      subtitle = "Individual Performance of CWAK Groups",
      x = "CWAK Component",
      y = "Proportion Correctly Classified",
      fill = "Analysis Type"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      plot.subtitle = element_text(hjust = 0.5, size = 11, color = "gray50"),
      axis.title = element_text(face = "bold"),
      legend.position = "bottom",
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank()
    )
  
  print(plot3)
  
  # Save plot
  ggsave(
    filename = file.path(OUTPUT_DIR, "CWAK_component_breakdown.png"),
    plot = plot3,
    width = 10,
    height = 6,
    dpi = 300
  )
  cat("  Saved Figure 3: CWAK_component_breakdown.png\n")
}

cat("\n")
cat("All plots saved to:", OUTPUT_DIR, "\n")

################################################################################
# STEP 9: PRINT SUMMARY STATISTICS
################################################################################

cat("\n")
cat(strrep("=", 80), "\n")
cat("FINAL SUMMARY STATISTICS (TOTAL ANALYSIS)\n")
cat(strrep("=", 80), "\n\n")

# CWAK group statistics
cwak_results <- cwak_accuracy %>% 
  filter(analysis == "Total", cwak_group == "CWAK")

if(nrow(cwak_results) > 0) {
  cat("CWAK GROUP:\n")
  cat("  Total fish:", cwak_results$n_fish, "\n")
  cat("  Correctly classified:", cwak_results$correct, "\n") 
  cat("  Accuracy:", sprintf("%.1f%%", cwak_results$accuracy * 100), "\n\n")
}

# non-CWAK group statistics
non_cwak_results <- cwak_accuracy %>% 
  filter(analysis == "Total", cwak_group == "non-CWAK")

if(nrow(non_cwak_results) > 0) {
  cat("non-CWAK GROUP:\n")
  cat("  Total fish:", non_cwak_results$n_fish, "\n")
  cat("  Correctly classified:", non_cwak_results$correct, "\n")
  cat("  Accuracy:", sprintf("%.1f%%", non_cwak_results$accuracy * 100), "\n\n")
}

# Component breakdown
if(nrow(cwak_components) > 0) {
  cat("CWAK COMPONENTS (TOTAL ANALYSIS):\n")
  total_components <- cwak_components %>% filter(analysis == "Total")
  
  for(i in 1:nrow(total_components)) {
    comp <- total_components[i, ]
    cat("  ", comp$cwak_component, ":\n")
    cat("    Fish:", comp$n_fish, "\n")
    cat("    Accuracy:", sprintf("%.1f%%", comp$accuracy * 100), "\n")
  }
}

cat("\n")
cat(strrep("=", 80), "\n")
cat("ANALYSIS COMPLETE\n")
cat(strrep("=", 80), "\n")

################################################################################
# STEP 10: SAVE RESULTS TO CSV
################################################################################

cat("\n")
cat(strrep("=", 80), "\n")
cat("SAVING RESULTS\n")
cat(strrep("=", 80), "\n\n")

# Save accuracy results
accuracy_file <- file.path(OUTPUT_DIR, "CWAK_accuracy_results.csv")
write.csv(cwak_accuracy, accuracy_file, row.names = FALSE)
cat("Saved accuracy results to:", accuracy_file, "\n")

# Save component breakdown
component_file <- file.path(OUTPUT_DIR, "CWAK_component_results.csv")
write.csv(cwak_components, component_file, row.names = FALSE)
cat("Saved component results to:", component_file, "\n")

# Save detailed predictions with CWAK groupings
detailed_file <- file.path(OUTPUT_DIR, "CWAK_detailed_predictions.csv")
write.csv(cwak_data, detailed_file, row.names = FALSE)
cat("Saved detailed predictions to:", detailed_file, "\n")

################################################################################
# END OF SCRIPT
################################################################################