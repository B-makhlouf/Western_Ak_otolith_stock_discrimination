# Classification Performance by CWAK vs non-CWAK Analysis
# Analyzes GAM Random Forest results by CWAK groupings
# CWAK = Lower Yukon + All Kuskokwim + All Nushagak
# non-CWAK = Middle Yukon + Upper Yukon

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
  
  # Check if RF results have Fish_id column
  if("Fish_id" %in% colnames(rf_results)) {
    # If Fish_id exists, merge directly
    rf_with_genetics <- rf_results %>%
      left_join(gam_test_with_genetics, by = "Fish_id")
  } else {
    # If no Fish_id, we need to match by row order
    # First, get the Fish_ids that are in the test set for this analysis
    if(analysis_name == "Total") {
      # For Total analysis, all test fish are included
      test_fish_genetics <- gam_test_with_genetics
    } else {
      # For Overlap analysis, only fish with Natal_Iso < 0.713 are included
      test_fish_genetics <- gam_test_with_genetics %>%
        filter(Natal_Iso < 0.713, !is.na(Natal_Iso))  # Same threshold as in ModelTesting.R
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
  }
  
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
# CALCULATE PERFORMANCE BY CWAK GROUPING
# =============================================================================

# Function to calculate CWAK group performance
calculate_cwak_performance <- function(rf_genetics_data, analysis_name) {
  
  cat("\n=== CWAK Performance Analysis for", analysis_name, "===\n")
  
  # Overall statistics
  total_fish <- nrow(rf_genetics_data)
  yukon_fish <- sum(rf_genetics_data$Watershed == "Yukon", na.rm = TRUE)
  kuskokwim_fish <- sum(rf_genetics_data$Watershed == "Kuskokwim", na.rm = TRUE)
  nushagak_fish <- sum(rf_genetics_data$Watershed == "Nushagak", na.rm = TRUE)
  fish_with_genetics <- sum(!is.na(rf_genetics_data$genetic_group))
  fish_without_genetics <- sum(is.na(rf_genetics_data$genetic_group))
  
  cat("Total fish in analysis:", total_fish, "\n")
  cat("Yukon fish:", yukon_fish, "\n")
  cat("Kuskokwim fish:", kuskokwim_fish, "\n")
  cat("Nushagak fish:", nushagak_fish, "\n")
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
  
  # Create CWAK grouping for ALL fish (not just Yukon)
  # CWAK = Lower Yukon + All Kuskokwim + All Nushagak
  # non-CWAK = Middle Yukon + Upper Yukon
  rf_genetics_data_cwak <- rf_genetics_data %>%
    mutate(
      cwak_group = case_when(
        # CWAK group
        Watershed == "Kuskokwim" ~ "CWAK",
        Watershed == "Nushagak" ~ "CWAK", 
        Watershed == "Yukon" & genetic_group == "Lower" ~ "CWAK",
        # non-CWAK group
        Watershed == "Yukon" & genetic_group %in% c("Middle", "Upper") ~ "non-CWAK",
        # Fish without genetic assignments or other cases
        TRUE ~ "Unassigned"
      )
    )
  
  # Filter to only fish that can be assigned to CWAK or non-CWAK
  cwak_assignable_fish <- rf_genetics_data_cwak %>%
    filter(cwak_group %in% c("CWAK", "non-CWAK"))
  
  cat("\nCWAK Group Assignments:\n")
  cwak_summary <- rf_genetics_data_cwak %>%
    count(cwak_group, name = "n_fish") %>%
    mutate(proportion = n_fish / sum(n_fish))
  print(cwak_summary)
  
  if(nrow(cwak_assignable_fish) == 0) {
    cat("No fish could be assigned to CWAK or non-CWAK groups in", analysis_name, "analysis\n")
    return(list(watershed_accuracy = watershed_accuracy))
  }
  
  # Calculate accuracy by CWAK group
  cwak_performance <- cwak_assignable_fish %>%
    group_by(cwak_group) %>%
    summarise(
      n_fish = n(),
      correct = sum(Watershed == .pred_class),
      accuracy = correct / n_fish,
      proportion_of_total = n_fish / nrow(cwak_assignable_fish),
      .groups = "drop"
    ) %>%
    mutate(analysis = analysis_name) %>%
    arrange(desc(accuracy))
  
  cat("\nCWAK Group Performance:\n")
  print(cwak_performance)
  
  # Create detailed breakdown by watershed and genetic group
  cwak_detailed <- cwak_assignable_fish %>%
    group_by(cwak_group, Watershed, genetic_group, .pred_class) %>%
    summarise(
      n_fish = n(),
      .groups = "drop"
    ) %>%
    mutate(analysis = analysis_name)
  
  cat("\nDetailed CWAK Group Classifications:\n")
  print(cwak_detailed)
  
  # Breakdown by individual components of CWAK
  cwak_components <- cwak_assignable_fish %>%
    filter(cwak_group == "CWAK") %>%
    mutate(
      cwak_component = case_when(
        Watershed == "Kuskokwim" ~ "All Kuskokwim",
        Watershed == "Nushagak" ~ "All Nushagak", 
        Watershed == "Yukon" & genetic_group == "Lower" ~ "Lower Yukon"
      )
    ) %>%
    group_by(cwak_component) %>%
    summarise(
      n_fish = n(),
      correct = sum(Watershed == .pred_class),
      accuracy = correct / n_fish,
      .groups = "drop"
    ) %>%
    mutate(analysis = analysis_name)
  
  cat("\nCWAK Component Performance:\n")
  print(cwak_components)
  
  return(list(
    watershed_accuracy = watershed_accuracy,
    cwak_performance = cwak_performance,
    cwak_detailed = cwak_detailed,
    cwak_components = cwak_components,
    cwak_data = cwak_assignable_fish
  ))
}

# Calculate performance for each analysis
performance_results <- map(names(results_list), function(analysis_name) {
  calculate_cwak_performance(results_list[[analysis_name]], analysis_name)
})
names(performance_results) <- names(results_list)

# =============================================================================
# CREATE CWAK vs NON-CWAK COMPARISON
# =============================================================================

# Function to create CWAK vs non-CWAK comparison data
create_cwak_comparison_data <- function(performance_results) {
  
  # Combine all CWAK assignable fish data from both analyses
  cwak_data_combined <- map_dfr(names(performance_results), function(analysis_name) {
    if(!is.null(performance_results[[analysis_name]]$cwak_data)) {
      performance_results[[analysis_name]]$cwak_data %>%
        select(cwak_group, Watershed, .pred_class) %>%
        mutate(analysis = analysis_name)
    }
  })
  
  if(is.null(cwak_data_combined) || nrow(cwak_data_combined) == 0) {
    cat("No CWAK assignable fish data found\n")
    return(NULL)
  }
  
  # Calculate accuracy for each group by analysis
  group_performance <- cwak_data_combined %>%
    group_by(analysis, cwak_group) %>%
    summarise(
      n_fish = n(),
      correct = sum(Watershed == .pred_class),
      accuracy = correct / n_fish,
      .groups = "drop"
    )
  
  return(group_performance)
}

# Create CWAK vs non-CWAK comparison data
cwak_comparison_data <- create_cwak_comparison_data(performance_results)

# =============================================================================
# CREATE CWAK COMPONENT COMPARISON
# =============================================================================

# Function to create CWAK component comparison data
create_cwak_component_data <- function(performance_results) {
  
  # Combine CWAK component data from both analyses
  component_data_combined <- map_dfr(names(performance_results), function(analysis_name) {
    if(!is.null(performance_results[[analysis_name]]$cwak_components)) {
      performance_results[[analysis_name]]$cwak_components
    }
  })
  
  return(component_data_combined)
}

# Create CWAK component comparison data
cwak_component_data <- create_cwak_component_data(performance_results)

# =============================================================================
# CREATE VISUALIZATIONS
# =============================================================================

# Figure 1: CWAK vs non-CWAK comparison (both analyses)
if(!is.null(cwak_comparison_data) && nrow(cwak_comparison_data) > 0) {
  
  cat("\n=== CWAK vs non-CWAK Comparison ===\n")
  print(cwak_comparison_data)
  
  cwak_vs_noncwak_plot <- ggplot(cwak_comparison_data, 
                                 aes(x = cwak_group, y = accuracy, fill = analysis)) +
    geom_col(position = "dodge", alpha = 0.8, width = 0.6) +
    geom_text(aes(label = paste0(sprintf("%.1f%%", accuracy * 100), "\n(n=", n_fish, ")")),
              position = position_dodge(width = 0.6), 
              vjust = -0.2, size = 4, fontface = "bold") +
    scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1.1)) +
    scale_fill_manual(values = c("Total" = "#2E86AB", "Overlap" = "#A23B72")) +
    labs(
      title = "Classification Accuracy: CWAK vs non-CWAK Groups",
      subtitle = "GAM Random Forest Performance\nCWAK = Lower Yukon + All Kuskokwim + All Nushagak\nnon-CWAK = Middle Yukon + Upper Yukon",
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
      legend.title = element_text(face = "bold"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank()
    )
  
  # Display the plot
  print(cwak_vs_noncwak_plot)
  
} else {
  cat("No data available for CWAK vs non-CWAK comparison\n")
}

# Figure 2: Total analysis only
if(!is.null(cwak_comparison_data) && nrow(cwak_comparison_data) > 0) {
  
  # Filter to Total analysis only
  total_only_data <- cwak_comparison_data %>%
    filter(analysis == "Total")
  
  if(nrow(total_only_data) > 0) {
    
    cat("\n=== Total Analysis Only ===\n")
    print(total_only_data)
    
    total_only_plot <- ggplot(total_only_data, 
                              aes(x = cwak_group, y = accuracy)) +
      geom_col(fill = "#2E86AB", alpha = 0.8, width = 0.5) +
      geom_text(aes(label = paste0(sprintf("%.1f%%", accuracy * 100), "\n(n=", n_fish, ")")),
                vjust = -0.2, size = 5, fontface = "bold", color = "black") +
      scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1.1)) +
      labs(
        title = "Classification Accuracy: CWAK vs non-CWAK Groups",
        subtitle = "GAM Random Forest Performance - Total Analysis\nCWAK = Lower Yukon + All Kuskokwim + All Nushagak\nnon-CWAK = Middle Yukon + Upper Yukon",
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
    
    # Display the plot
    print(total_only_plot)
    
  } else {
    cat("No Total analysis data available\n")
  }
} else {
  cat("No data available for Total analysis plot\n")
}

# Figure 3: CWAK Component Performance
if(!is.null(cwak_component_data) && nrow(cwak_component_data) > 0) {
  
  cat("\n=== CWAK Component Performance ===\n")
  print(cwak_component_data)
  
  cwak_component_plot <- ggplot(cwak_component_data, 
                                aes(x = reorder(cwak_component, accuracy), y = accuracy, fill = analysis)) +
    geom_col(position = "dodge", alpha = 0.8, width = 0.7) +
    geom_text(aes(label = paste0(sprintf("%.1f%%", accuracy * 100), "\n(n=", n_fish, ")")),
              position = position_dodge(width = 0.7), 
              hjust = -0.1, size = 3.5, fontface = "bold") +
    scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1.2)) +
    scale_fill_manual(values = c("Total" = "#2E86AB", "Overlap" = "#A23B72")) +
    coord_flip() +
    labs(
      title = "Classification Accuracy by CWAK Components",
      subtitle = "GAM Random Forest Performance - Individual CWAK Groups",
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
  
  print(cwak_component_plot)
  
} else {
  cat("No data available for CWAK component analysis\n")
}

# =============================================================================
# SUMMARY STATISTICS - FOCUS ON CWAK GROUP
# =============================================================================

cat("\n=== CWAK GROUP SUMMARY ===\n")

for(analysis_name in names(performance_results)) {
  cat("\n", analysis_name, "Analysis:\n")
  
  if(!is.null(performance_results[[analysis_name]]$cwak_performance)) {
    cwak_perf <- performance_results[[analysis_name]]$cwak_performance
    
    # Focus on CWAK group specifically
    cwak_group <- cwak_perf %>% filter(cwak_group == "CWAK")
    non_cwak_group <- cwak_perf %>% filter(cwak_group == "non-CWAK")
    
    if(nrow(cwak_group) > 0) {
      cat("CWAK GROUP RESULTS:\n")
      cat("  Total CWAK fish:", cwak_group$n_fish, "\n")
      cat("  Correctly classified:", cwak_group$correct, "\n")
      cat("  Accuracy:", sprintf("%.1f%%", cwak_group$accuracy * 100), "\n")
      cat("  Incorrectly classified:", cwak_group$n_fish - cwak_group$correct, "\n")
    }
    
    if(nrow(non_cwak_group) > 0) {
      cat("NON-CWAK GROUP RESULTS:\n")
      cat("  Total non-CWAK fish:", non_cwak_group$n_fish, "\n")
      cat("  Correctly classified:", non_cwak_group$correct, "\n")
      cat("  Accuracy:", sprintf("%.1f%%", non_cwak_group$accuracy * 100), "\n")
      cat("  Incorrectly classified:", non_cwak_group$n_fish - non_cwak_group$correct, "\n")
    }
    
    # CWAK component breakdown
    if(!is.null(performance_results[[analysis_name]]$cwak_components)) {
      cwak_comp <- performance_results[[analysis_name]]$cwak_components
      cat("\nCWAK COMPONENT BREAKDOWN:\n")
      for(i in 1:nrow(cwak_comp)) {
        comp <- cwak_comp[i, ]
        cat("  ", comp$cwak_component, ": ", comp$n_fish, " fish, ", 
            sprintf("%.1f%%", comp$accuracy * 100), " accuracy\n")
      }
    }
  } else {
    cat("No CWAK performance data found for", analysis_name, "analysis\n")
  }
}

# Optional: Save results
# write.csv(do.call(rbind, map(performance_results, "cwak_performance")), 
#           "cwak_classification_results.csv", row.names = FALSE)