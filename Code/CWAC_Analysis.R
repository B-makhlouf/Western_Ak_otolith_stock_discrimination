# CWAK vs non-CWAK Classification Performance Analysis
# Simple, step-by-step analysis without complex functions
# CWAK = Lower Yukon + All Kuskokwim + All Nushagak
# non-CWAK = Middle Yukon + Upper Yukon

library(tidyverse)
library(ggplot2)

# =============================================================================
# STEP 1: LOAD METADATA WITH GENETIC INFORMATION
# =============================================================================

cat("STEP 1: Loading metadata...\n")

metadata_path <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/Data/Final/Metadata_and_QC.csv"
metadata <- read.csv(metadata_path)

cat("Loaded", nrow(metadata), "fish from metadata\n")

# =============================================================================
# STEP 2: CREATE GENETIC GROUPS
# =============================================================================

cat("\nSTEP 2: Creating genetic groups...\n")

# Add genetic group based on highest probability
metadata$genetic_group <- NA

for(i in 1:nrow(metadata)) {
  lower <- metadata$Lower_gen[i]
  middle <- metadata$Middle_gen[i] 
  upper <- metadata$Upper_gen[i]
  
  if(!is.na(lower) | !is.na(middle) | !is.na(upper)) {
    probs <- c(Lower = lower, Middle = middle, Upper = upper)
    probs <- probs[!is.na(probs)]
    
    if(length(probs) > 0) {
      metadata$genetic_group[i] <- names(probs)[which.max(probs)]
    }
  }
}

# Clean up column names
metadata$Fish_id <- metadata$Fish_ID
metadata$Natal_Iso <- metadata$Natal_origins_iso

cat("Genetic groups assigned:\n")
print(table(metadata$genetic_group, useNA = "ifany"))

# =============================================================================
# STEP 3: LOAD PREDICTION RESULTS
# =============================================================================

cat("\nSTEP 3: Loading prediction results...\n")

# Load Total analysis predictions
total_predictions <- read.csv("/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/Output/ModelResultsPreCal/Total/GAM_RF_TOTAL_predictions.csv")

# Load Overlap analysis predictions  
overlap_predictions <- read.csv("/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/Output/ModelResultsPreCal/Filtered/GAM_RF_OVERLAP_predictions.csv")

cat("Total predictions:", nrow(total_predictions), "fish\n")
cat("Overlap predictions:", nrow(overlap_predictions), "fish\n")

# =============================================================================
# STEP 4: MERGE PREDICTIONS WITH GENETIC DATA
# =============================================================================

cat("\nSTEP 4: Merging predictions with genetic data...\n")

# For Total analysis - use all fish
total_with_genetics <- total_predictions
total_with_genetics$genetic_group <- metadata$genetic_group[1:nrow(total_predictions)]
total_with_genetics$Fish_id <- metadata$Fish_id[1:nrow(total_predictions)]
total_with_genetics$Natal_Iso <- metadata$Natal_Iso[1:nrow(total_predictions)]
total_with_genetics$analysis <- "Total"

# For Overlap analysis - use fish with Natal_Iso < 0.713
overlap_fish <- metadata[!is.na(metadata$Natal_Iso) & metadata$Natal_Iso < 0.713, ]
overlap_with_genetics <- overlap_predictions
overlap_with_genetics$genetic_group <- overlap_fish$genetic_group[1:nrow(overlap_predictions)]
overlap_with_genetics$Fish_id <- overlap_fish$Fish_id[1:nrow(overlap_predictions)]
overlap_with_genetics$Natal_Iso <- overlap_fish$Natal_Iso[1:nrow(overlap_predictions)]
overlap_with_genetics$analysis <- "Overlap"

# Combine both analyses
all_predictions <- bind_rows(total_with_genetics, overlap_with_genetics)

cat("Combined data:", nrow(all_predictions), "predictions\n")

# =============================================================================
# STEP 5: CREATE CWAK GROUPINGS
# =============================================================================

cat("\nSTEP 5: Creating CWAK vs non-CWAK groups...\n")

# Add CWAK grouping
all_predictions$cwak_group <- NA

for(i in 1:nrow(all_predictions)) {
  watershed <- all_predictions$Watershed[i]
  genetic <- all_predictions$genetic_group[i]
  
  if(watershed == "Kusko") {
    all_predictions$cwak_group[i] <- "CWAK"
  } else if(watershed == "Nush") {
    all_predictions$cwak_group[i] <- "CWAK"  
  } else if(watershed == "Yukon" & genetic == "Lower") {
    all_predictions$cwak_group[i] <- "CWAK"
  } else if(watershed == "Yukon" & genetic %in% c("Middle", "Upper")) {
    all_predictions$cwak_group[i] <- "non-CWAK"
  }
}

# Show CWAK group distribution
cat("CWAK group distribution:\n")
print(table(all_predictions$cwak_group, all_predictions$analysis, useNA = "ifany"))

# =============================================================================
# STEP 6: CALCULATE ACCURACY BY CWAK GROUP
# =============================================================================

cat("\nSTEP 6: Calculating accuracy by CWAK group...\n")

# Filter to only fish that can be assigned to CWAK or non-CWAK
cwak_data <- all_predictions[!is.na(all_predictions$cwak_group), ]

cat("Fish available for CWAK analysis:", nrow(cwak_data), "\n")

# Calculate accuracy for each CWAK group and analysis
cwak_accuracy <- cwak_data %>%
  group_by(analysis, cwak_group) %>%
  summarise(
    n_fish = n(),
    correct = sum(Watershed == .pred_class),
    accuracy = correct / n_fish,
    .groups = "drop"
  )

cat("\nCWAK vs non-CWAK Accuracy Results:\n")
print(cwak_accuracy)

# =============================================================================
# STEP 7: CALCULATE CWAK COMPONENT BREAKDOWN
# =============================================================================

cat("\nSTEP 7: Breaking down CWAK components...\n")

# Calculate accuracy for each component of CWAK (only fish with genetic data)
cwak_components <- cwak_data %>%
  filter(cwak_group == "CWAK", !is.na(genetic_group)) %>%
  mutate(
    cwak_component = case_when(
      Watershed == "Kuskokwim" ~ "All Kuskokwim",
      Watershed == "Nushagak" ~ "All Nushagak",
      Watershed == "Yukon" & genetic_group == "Lower" ~ "Lower Yukon",
      TRUE ~ NA_character_  # This shouldn't happen, but just in case
    )
  ) %>%
  filter(!is.na(cwak_component)) %>%  # Remove any rows that couldn't be classified
  group_by(analysis, cwak_component) %>%
  summarise(
    n_fish = n(),
    correct = sum(Watershed == .pred_class),
    accuracy = correct / n_fish,
    .groups = "drop"
  )

cat("\nCWAK Component Accuracy:\n")
print(cwak_components)

# =============================================================================
# STEP 8: CREATE VISUALIZATIONS
# =============================================================================

cat("\nSTEP 8: Creating visualizations...\n")

# FIGURE 1: CWAK vs non-CWAK comparison (both analyses)
plot1 <- ggplot(cwak_accuracy, aes(x = cwak_group, y = accuracy, fill = analysis)) +
  geom_col(position = "dodge", alpha = 0.8, width = 0.6) +
  geom_text(aes(label = paste0(sprintf("%.1f%%", accuracy * 100), "\n(n=", n_fish, ")")),
            position = position_dodge(width = 0.6), 
            vjust = -0.2, size = 4, fontface = "bold") +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1.1)) +
  scale_fill_manual(values = c("Total" = "#2E86AB", "Overlap" = "#A23B72")) +
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

# FIGURE 2: Total analysis only
total_only_data <- cwak_accuracy %>% filter(analysis == "Total")

plot2 <- ggplot(total_only_data, aes(x = cwak_group, y = accuracy)) +
  geom_col(fill = "#2E86AB", alpha = 0.8, width = 0.5) +
  geom_text(aes(label = paste0(sprintf("%.1f%%", accuracy * 100), "\n(n=", n_fish, ")")),
            vjust = -0.2, size = 5, fontface = "bold", color = "black") +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1.1)) +
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

# FIGURE 3: CWAK component breakdown
plot3 <- ggplot(cwak_components, aes(x = reorder(cwak_component, accuracy), y = accuracy, fill = analysis)) +
  geom_col(position = "dodge", alpha = 0.8, width = 0.7) +
  geom_text(aes(label = paste0(sprintf("%.1f%%", accuracy * 100), "\n(n=", n_fish, ")")),
            position = position_dodge(width = 0.7), 
            hjust = -0.1, size = 3.5, fontface = "bold") +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1.2)) +
  scale_fill_manual(values = c("Total" = "#2E86AB", "Overlap" = "#A23B72")) +
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

# =============================================================================
# STEP 9: SUMMARY STATISTICS (TOTAL ANALYSIS ONLY)
# =============================================================================

cat("\nSTEP 9: Summary statistics...\n")

cat("\n=== CWAK GROUP SUMMARY (TOTAL ANALYSIS) ===\n")

# CWAK results
cwak_results <- cwak_accuracy %>% 
  filter(cwak_group == "CWAK")

if(nrow(cwak_results) > 0) {
  cat("CWAK GROUP:\n")
  cat("  Total fish:", cwak_results$n_fish, "\n")
  cat("  Correctly classified:", cwak_results$correct, "\n") 
  cat("  Accuracy:", sprintf("%.1f%%", cwak_results$accuracy * 100), "\n")
}

# non-CWAK results
non_cwak_results <- cwak_accuracy %>% 
  filter(cwak_group == "non-CWAK")

if(nrow(non_cwak_results) > 0) {
  cat("non-CWAK GROUP:\n")
  cat("  Total fish:", non_cwak_results$n_fish, "\n")
  cat("  Correctly classified:", non_cwak_results$correct, "\n")
  cat("  Accuracy:", sprintf("%.1f%%", non_cwak_results$accuracy * 100), "\n")
}

# Component breakdown
if(nrow(cwak_components) > 0) {
  cat("CWAK COMPONENTS:\n")
  for(i in 1:nrow(cwak_components)) {
    comp <- cwak_components[i, ]
    cat("  ", comp$cwak_component, ": ", comp$n_fish, " fish, ", 
        sprintf("%.1f%%", comp$accuracy * 100), " accuracy\n")
  }
}

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("Script completed successfully!\n")