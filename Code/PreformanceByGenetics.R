# Classification Performance by Genetic Group Analysis
# Analyzes GAM Random Forest results by Yukon genetic groupings

library(tidyverse)
library(ggplot2)

# =============================================================================
# LOAD DATA
# =============================================================================

# Step 1: Load the GAM test data with genetic groupings (from previous script)
# This should be run after the genetic grouping script
if(!exists("gam_test_with_genetics")) {
  source("path/to/your/genetic_grouping_script.R")  # Update this path
}

# Step 2: Load GAM Random Forest prediction results
# Try both Total and Overlap analyses
rf_results_total <- tryCatch({
  read.csv("/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/Output/ModelResultsPreCal/Total/GAM_RF_TOTAL_predictions.csv")
}, error = function(e) NULL)

rf_results_overlap <- tryCatch({
  read.csv("/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/Output/ModelResultsPreCal/Filtered/GAM_RF_OVERLAP_predictions.csv")
}, error = function(e) NULL)

# Check which results are available
if(is.null(rf_results_total) && is.null(rf_results_overlap)) {
  stop("No GAM RF prediction results found! Please run ModelTesting.R first.")
}

# =============================================================================
# MERGE GENETIC DATA WITH PREDICTION RESULTS
# =============================================================================

# Function to merge genetics with RF results and analyze performance
analyze_genetic_performance <- function(rf_results, analysis_name) {
  
  cat("\n=== Analyzing", analysis_name, "===\n")
  
  # Since the RF results don't have Fish_id, we need to match by row order
  # The test sets should be in the same order as our gam_test_with_genetics
  
  # First, get the Fish_ids that are in the test set for this analysis
  if(analysis_name == "Total") {
    # For Total analysis, all test fish are included
    test_fish_genetics <- gam_test_with_genetics
  } else {
    # For Overlap analysis, only fish with Natal_Iso < 0.713 are included
    test_fish_genetics <- gam_test_with_genetics %>%
      filter(Natal_Iso < 0.713)  # Same threshold as in ModelTesting.R
  }
  
  # Check if the number of rows match
  if(nrow(rf_results) != nrow(test_fish_genetics)) {
    cat("Warning: Row count mismatch!\n")
    cat("RF results:", nrow(rf_results), "rows\n")
    cat("Test genetics:", nrow(test_fish_genetics), "rows\n")
    # Take the minimum to avoid errors
    n_rows <- min(nrow(rf_results), nrow(test_fish_genetics))
    rf_results <- rf_results[1:n_rows, ]
    test_fish_genetics <- test_fish_genetics[1:n_rows, ]
  }
  
  # Combine RF results with genetic information AND metadata
  rf_with_genetics <- cbind(rf_results, 
                            genetic_group = test_fish_genetics$genetic_group,
                            Fish_id = test_fish_genetics$Fish_id,
                            Lower = test_fish_genetics$Lower,
                            Middle = test_fish_genetics$Middle,
                            Upper = test_fish_genetics$Upper,
                            Natal_Iso = test_fish_genetics$Natal_Iso,
                            Year = test_fish_genetics$Year)
  
  return(rf_with_genetics)
}

# Analyze both analyses if available
results_list <- list()

if(!is.null(rf_results_total)) {
  results_list[["Total"]] <- analyze_genetic_performance(rf_results_total, "Total")
}

if(!is.null(rf_results_overlap)) {
  results_list[["Overlap"]] <- analyze_genetic_performance(rf_results_overlap, "Overlap")
}

# =============================================================================
# CALCULATE PERFORMANCE BY GENETIC GROUP
# =============================================================================

# Function to calculate genetic group performance
calculate_genetic_performance <- function(rf_genetics_data, analysis_name) {
  
  cat("\n=== Performance Analysis for", analysis_name, "===\n")
  
  # Overall statistics
  total_fish <- nrow(rf_genetics_data)
  yukon_fish <- sum(rf_genetics_data$Watershed == "Yukon", na.rm = TRUE)
  fish_with_genetics <- sum(!is.na(rf_genetics_data$genetic_group))
  fish_without_genetics <- sum(is.na(rf_genetics_data$genetic_group))
  
  cat("Total fish in analysis:", total_fish, "\n")
  cat("Yukon fish:", yukon_fish, "\n")
  cat("Fish with genetic assignments:", fish_with_genetics, "\n")
  cat("Fish without genetic assignments:", fish_without_genetics, "\n")
  
  # Calculate accuracy by watershed
  watershed_accuracy <- rf_genetics_data %>%
    group_by(Watershed) %>%
    summarise(
      n_fish = n(),
      correct = sum(Watershed == .pred_class),
      accuracy = correct / n_fish,
      .groups = "drop"
    ) %>%
    mutate(analysis = analysis_name)
  
  cat("\nOverall Watershed Accuracy:\n")
  print(watershed_accuracy)
  
  # Focus on Yukon fish only for genetic analysis
  yukon_fish_data <- rf_genetics_data %>%
    filter(Watershed == "Yukon")
  
  if(nrow(yukon_fish_data) == 0) {
    cat("No Yukon fish found in", analysis_name, "analysis\n")
    return(list(watershed_accuracy = watershed_accuracy))
  }
  
  # Calculate accuracy by genetic group (Yukon fish only)
  genetic_performance <- yukon_fish_data %>%
    group_by(genetic_group) %>%
    summarise(
      n_fish = n(),
      correct = sum(Watershed == .pred_class),
      accuracy = correct / n_fish,
      proportion_of_yukon = n_fish / nrow(yukon_fish_data),
      .groups = "drop"
    ) %>%
    mutate(analysis = analysis_name) %>%
    arrange(desc(accuracy))
  
  cat("\nGenetic Group Performance (Yukon fish only):\n")
  print(genetic_performance)
  
  # Create detailed breakdown
  genetic_detailed <- yukon_fish_data %>%
    group_by(genetic_group, .pred_class) %>%
    summarise(
      n_fish = n(),
      .groups = "drop"
    ) %>%
    mutate(analysis = analysis_name)
  
  cat("\nDetailed Genetic Group Classifications:\n")
  print(genetic_detailed)
  
  return(list(
    watershed_accuracy = watershed_accuracy,
    genetic_performance = genetic_performance,
    genetic_detailed = genetic_detailed,
    yukon_data = yukon_fish_data
  ))
}

# Calculate performance for each analysis
performance_results <- map(names(results_list), function(analysis_name) {
  calculate_genetic_performance(results_list[[analysis_name]], analysis_name)
})
names(performance_results) <- names(results_list)

# =============================================================================
# CREATE LOWER vs NON-LOWER COMPARISON
# =============================================================================

# Function to create Lower vs Non-Lower comparison data
create_lower_comparison_data <- function(performance_results) {
  
  # Combine all Yukon fish data from both analyses
  yukon_data_combined <- map_dfr(names(performance_results), function(analysis_name) {
    performance_results[[analysis_name]]$yukon_data %>%
      select(genetic_group, Watershed, .pred_class) %>%
      mutate(analysis = analysis_name)
  })
  
  if(nrow(yukon_data_combined) == 0) {
    cat("No Yukon fish data found\n")
    return(NULL)
  }
  
  # Create Lower vs Non-Lower groups
  comparison_data <- yukon_data_combined %>%
    filter(!is.na(genetic_group)) %>%  # Only fish with genetic assignments
    mutate(
      genetic_comparison = case_when(
        genetic_group == "Lower" ~ "Lower",
        genetic_group %in% c("Middle", "Upper") ~ "Non-Lower"
      )
    )
  
  # Calculate accuracy for each group by analysis
  group_performance <- comparison_data %>%
    group_by(analysis, genetic_comparison) %>%
    summarise(
      n_fish = n(),
      correct = sum(Watershed == .pred_class),
      accuracy = correct / n_fish,
      .groups = "drop"
    )
  
  return(group_performance)
}

# Create Lower vs Non-Lower comparison data
lower_comparison_data <- create_lower_comparison_data(performance_results)

# =============================================================================
# CREATE VISUALIZATIONS
# =============================================================================

# Figure 1: Lower vs Non-Lower comparison (both analyses)
if(!is.null(lower_comparison_data) && nrow(lower_comparison_data) > 0) {
  
  cat("\n=== Lower vs Non-Lower Comparison ===\n")
  print(lower_comparison_data)
  
  lower_vs_nonlower_plot <- ggplot(lower_comparison_data, 
                                   aes(x = genetic_comparison, y = accuracy, fill = analysis)) +
    geom_col(position = "dodge", alpha = 0.8, width = 0.6) +
    geom_text(aes(label = paste0(sprintf("%.1f%%", accuracy * 100), "\n(n=", n_fish, ")")),
              position = position_dodge(width = 0.6), 
              vjust = -0.2, size = 4, fontface = "bold") +
    scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1.1)) +
    scale_fill_manual(values = c("Total" = "#2E86AB", "Overlap" = "#A23B72")) +
    labs(
      title = "Classification Accuracy: Lower vs Non-Lower Genetic Groups",
      subtitle = "GAM Random Forest Performance in Yukon Fish",
      x = "Genetic Group Comparison",
      y = "Proportion Correctly Classified",
      fill = "Analysis Type"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
      plot.subtitle = element_text(hjust = 0.5, size = 12, color = "gray50"),
      axis.title = element_text(face = "bold", size = 12),
      axis.text = element_text(size = 11),
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank()
    )
  
  # Display the plot
  print(lower_vs_nonlower_plot)
  
} else {
  cat("No data available for Lower vs Non-Lower comparison\n")
}

# =============================================================================
# CREATE ADDITIONAL PERFORMANCE ANALYSES
# =============================================================================

# Function to create Natal_Iso binned performance data
create_natal_iso_performance <- function(performance_results) {
  
  # Combine all fish data from both analyses (using the rf_genetics_data which has all metadata)
  all_fish_combined <- map_dfr(names(performance_results), function(analysis_name) {
    # Get the full rf_genetics_data from results_list, not just yukon_data
    results_list[[analysis_name]] %>%
      select(genetic_group, Watershed, .pred_class, Natal_Iso, Year) %>%
      mutate(analysis = analysis_name)
  })
  
  if(nrow(all_fish_combined) == 0) {
    cat("No fish data found for Natal_Iso analysis\n")
    return(NULL)
  }
  
  # Create Natal_Iso bins from 0.7000 to 0.7200 by 0.0005
  bin_breaks <- seq(0.7000, 0.7200, by = 0.0005)
  
  # Add Natal_Iso bins
  binned_data <- all_fish_combined %>%
    filter(!is.na(Natal_Iso), Natal_Iso >= 0.7000, Natal_Iso <= 0.7200) %>%
    mutate(
      natal_iso_bin = cut(Natal_Iso, 
                          breaks = bin_breaks, 
                          include.lowest = TRUE,
                          labels = paste0(bin_breaks[-length(bin_breaks)], "-", bin_breaks[-1]))
    ) %>%
    filter(!is.na(natal_iso_bin))
  
  # Calculate accuracy by Natal_Iso bin and analysis
  natal_iso_performance <- binned_data %>%
    group_by(analysis, natal_iso_bin) %>%
    summarise(
      n_fish = n(),
      correct = sum(Watershed == .pred_class),
      accuracy = correct / n_fish,
      .groups = "drop"
    ) %>%
    filter(n_fish >= 3)  # Only include bins with at least 3 fish
  
  return(natal_iso_performance)
}

# Function to create Year performance data
create_year_performance <- function(performance_results) {
  
  # Combine all fish data from both analyses (using the rf_genetics_data which has all metadata)
  all_fish_combined <- map_dfr(names(performance_results), function(analysis_name) {
    # Get the full rf_genetics_data from results_list, not just yukon_data
    results_list[[analysis_name]] %>%
      select(genetic_group, Watershed, .pred_class, Natal_Iso, Year) %>%
      mutate(analysis = analysis_name)
  })
  
  if(nrow(all_fish_combined) == 0) {
    cat("No fish data found for Year analysis\n")
    return(NULL)
  }
  
  # Calculate accuracy by Year and analysis
  year_performance <- all_fish_combined %>%
    filter(!is.na(Year)) %>%
    group_by(analysis, Year) %>%
    summarise(
      n_fish = n(),
      correct = sum(Watershed == .pred_class),
      accuracy = correct / n_fish,
      .groups = "drop"
    ) %>%
    filter(n_fish >= 5)  # Only include years with at least 5 fish
  
  return(year_performance)
}

# Create performance data
natal_iso_performance <- create_natal_iso_performance(performance_results)
year_performance <- create_year_performance(performance_results)

# Create Lower vs Non-Lower comparison data
lower_comparison_data <- create_lower_comparison_data(performance_results)

# =============================================================================
# CREATE VISUALIZATIONS
# =============================================================================

# Figure 1: Lower vs Non-Lower comparison (both analyses)
if(!is.null(lower_comparison_data) && nrow(lower_comparison_data) > 0) {
  
  cat("\n=== Lower vs Non-Lower Comparison ===\n")
  print(lower_comparison_data)
  
  lower_vs_nonlower_plot <- ggplot(lower_comparison_data, 
                                   aes(x = genetic_comparison, y = accuracy, fill = analysis)) +
    geom_col(position = "dodge", alpha = 0.8, width = 0.6) +
    geom_text(aes(label = paste0(sprintf("%.1f%%", accuracy * 100), "\n(n=", n_fish, ")")),
              position = position_dodge(width = 0.6), 
              vjust = -0.2, size = 4, fontface = "bold") +
    scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1.1)) +
    scale_fill_manual(values = c("Total" = "#2E86AB", "Overlap" = "#A23B72")) +
    labs(
      title = "Classification Accuracy: Lower vs Non-Lower Genetic Groups",
      subtitle = "GAM Random Forest Performance in Yukon Fish",
      x = "Genetic Group Comparison",
      y = "Proportion Correctly Classified",
      fill = "Analysis Type"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
      plot.subtitle = element_text(hjust = 0.5, size = 12, color = "gray50"),
      axis.title = element_text(face = "bold", size = 12),
      axis.text = element_text(size = 11),
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank()
    )
  
  # Display the plot
  print(lower_vs_nonlower_plot)
  
} else {
  cat("No data available for Lower vs Non-Lower comparison\n")
}

# Figure 2: Total analysis only
if(!is.null(lower_comparison_data) && nrow(lower_comparison_data) > 0) {
  
  # Filter to Total analysis only
  total_only_data <- lower_comparison_data %>%
    filter(analysis == "Total")
  
  if(nrow(total_only_data) > 0) {
    
    cat("\n=== Total Analysis Only ===\n")
    print(total_only_data)
    
    total_only_plot <- ggplot(total_only_data, 
                              aes(x = genetic_comparison, y = accuracy)) +
      geom_col(fill = "#2E86AB", alpha = 0.8, width = 0.5) +
      geom_text(aes(label = paste0(sprintf("%.1f%%", accuracy * 100), "\n(n=", n_fish, ")")),
                vjust = -0.2, size = 5, fontface = "bold", color = "black") +
      scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1.1)) +
      labs(
        title = "Classification Accuracy: Lower vs Non-Lower Genetic Groups",
        subtitle = "GAM Random Forest Performance - Total Analysis",
        x = "Genetic Group Comparison",
        y = "Proportion Correctly Classified"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 12, color = "gray50"),
        axis.title = element_text(face = "bold", size = 12),
        axis.text = element_text(size = 11),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()
      )
    
    # Display the plot
    print(total_only_plot)
    
  } else {
    cat("No Total analysis data available\n")
  }
} else {
  cat("No data available for Total analysis plot\n")
}

# Figure 3: Performance by Natal_Iso bins
if(!is.null(natal_iso_performance) && nrow(natal_iso_performance) > 0) {
  
  cat("\n=== Natal_Iso Bin Performance ===\n")
  print(natal_iso_performance)
  
  # Extract bin midpoints for cleaner x-axis labels
  natal_iso_performance <- natal_iso_performance %>%
    mutate(
      # Extract start of bin range and add half the bin width (0.00025)
      bin_start = as.numeric(str_extract(natal_iso_bin, "^[0-9]\\.[0-9]+")),
      bin_midpoint = bin_start + 0.00025,
      bin_label = sprintf("%.4f", bin_midpoint)
    ) %>%
    filter(!is.na(bin_midpoint))  # Remove any rows with failed parsing
  
  natal_iso_plot <- ggplot(natal_iso_performance, 
                           aes(x = bin_midpoint, y = accuracy, color = analysis)) +
    geom_point(aes(size = n_fish), alpha = 0.7) +
    geom_line(alpha = 0.8, linewidth = 1) +
    geom_text(aes(label = n_fish), vjust = -1.2, size = 3) +
    scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1.1)) +
    scale_size_continuous(range = c(2, 8), guide = "none") +
    scale_color_manual(values = c("Total" = "#2E86AB", "Overlap" = "#A23B72")) +
    labs(
      title = "Classification Accuracy by Natal Isotope Ratio",
      subtitle = "GAM Random Forest Performance - Bins of 0.0005",
      x = "Natal Isotope Ratio (Sr87/Sr86)",
      y = "Proportion Correctly Classified",
      color = "Analysis Type"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      plot.subtitle = element_text(hjust = 0.5, size = 11, color = "gray50"),
      axis.title = element_text(face = "bold"),
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    ) +
    guides(color = guide_legend(override.aes = list(size = 3)))
  
  print(natal_iso_plot)
  
} else {
  cat("No data available for Natal_Iso bin analysis\n")
}

# Figure 4: Performance by Year
if(!is.null(year_performance) && nrow(year_performance) > 0) {
  
  cat("\n=== Year Performance ===\n")
  print(year_performance)
  
  year_plot <- ggplot(year_performance, 
                      aes(x = factor(Year), y = accuracy, fill = analysis)) +
    geom_col(position = "dodge", alpha = 0.8, width = 0.7) +
    geom_text(aes(label = paste0(sprintf("%.1f%%", accuracy * 100), "\n(n=", n_fish, ")")),
              position = position_dodge(width = 0.7), 
              vjust = -0.2, size = 3.5, fontface = "bold") +
    scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1.1)) +
    scale_fill_manual(values = c("Total" = "#2E86AB", "Overlap" = "#A23B72")) +
    labs(
      title = "Classification Accuracy by Collection Year",
      subtitle = "GAM Random Forest Performance",
      x = "Collection Year",
      y = "Proportion Correctly Classified",
      fill = "Analysis Type"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      plot.subtitle = element_text(hjust = 0.5, size = 11, color = "gray50"),
      axis.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom",
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank()
    )
  
  print(year_plot)
  
} else {
  cat("No data available for Year analysis\n")
}

# =============================================================================
# SUMMARY STATISTICS - FOCUS ON LOWER GENETIC GROUP
# =============================================================================

cat("\n=== LOWER GENETIC GROUP SUMMARY ===\n")

for(analysis_name in names(performance_results)) {
  cat("\n", analysis_name, "Analysis:\n")
  
  genetic_perf <- performance_results[[analysis_name]]$genetic_performance %>%
    filter(!is.na(genetic_group))
  
  # Focus on Lower genetic group specifically
  lower_group <- genetic_perf %>% filter(genetic_group == "Lower")
  
  if(nrow(lower_group) > 0) {
    cat("LOWER GENETIC GROUP RESULTS:\n")
    cat("  Total Lower fish:", lower_group$n_fish, "\n")
    cat("  Correctly classified:", lower_group$correct, "\n")
    cat("  Accuracy:", sprintf("%.1f%%", lower_group$accuracy * 100), "\n")
    cat("  Incorrectly classified:", lower_group$n_fish - lower_group$correct, "\n")
  } else {
    cat("No Lower genetic group fish found in", analysis_name, "analysis\n")
  }
  
  if(nrow(genetic_perf) > 0) {
    cat("\nComparison to other groups:\n")
    cat("Genetic group with highest accuracy:", 
        genetic_perf$genetic_group[which.max(genetic_perf$accuracy)], 
        "at", sprintf("%.1f%%", max(genetic_perf$accuracy) * 100), "\n")
    
    cat("Genetic group with lowest accuracy:", 
        genetic_perf$genetic_group[which.min(genetic_perf$accuracy)], 
        "at", sprintf("%.1f%%", min(genetic_perf$accuracy) * 100), "\n")
  }
}

# Optional: Save results
# write.csv(do.call(rbind, map(performance_results, "genetic_performance")), 
#           "genetic_group_classification_results.csv", row.names = FALSE)