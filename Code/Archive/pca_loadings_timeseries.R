# PCA Loadings Timeseries Visualization
# This script visualizes how PCA loadings map onto original timeseries data
# specifically for GAM and RAW data types, with reversed color scheme

library(tidyverse)
library(viridis)
library(cowplot)
library(tools)

# Set seed for reproducibility
set.seed(42)

# Clear all existing plots and graphics devices
graphics.off()
if (exists("last_plot")) rm(last_plot)
invisible(lapply(names(dev.list()), function(x) dev.off()))
cat("Cleared all existing plots and graphics devices\n")

# Function to find files in different possible locations
find_file <- function(file_patterns, base_dirs = NULL) {
  if (is.null(base_dirs)) {
    base_dirs <- c(
      ".", "data", "Data", 
      "./data", "./Data",
      "../data", "../Data",
      "./data/preprocessed_matrices", 
      "./Data/preprocessed_matrices",
      "../data/preprocessed_matrices", 
      "../Data/preprocessed_matrices",
      "Data/Processed/Preprocessed_ts_matrices",
      "Data/01_processed/Preprocessed_ts_matrices"
    )
  }
  
  for (pattern in file_patterns) {
    for (base in base_dirs) {
      if (dir.exists(base)) {
        files <- list.files(path = base, pattern = pattern, recursive = TRUE, full.names = TRUE)
        if (length(files) > 0) {
          return(files[1]) # Return the first match
        }
      }
    }
  }
  return(NULL) # Return NULL if no matching file is found
}

# Function to process a single data type
process_data_type <- function(data_type, data_path, output_dir, sample_indices = NULL) {
  cat(sprintf("\n==== Processing %s data ====\n", data_type))
  
  # Create output directory for this data type
  type_dir <- file.path(output_dir, data_type)
  dir.create(type_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Load the data
  cat(sprintf("Loading data from: %s\n", data_path))
  df <- tryCatch({
    read.csv(data_path)
  }, error = function(e) {
    cat(sprintf("ERROR: Could not read file: %s\n", data_path))
    cat(sprintf("Error message: %s\n", e$message))
    return(NULL)
  })
  
  if (is.null(df) || nrow(df) == 0) {
    cat(sprintf("ERROR: No data available for %s\n", data_type))
    return(NULL)
  }
  
  # Extract metadata columns and feature columns
  meta_columns <- grep("^X", names(df), value = TRUE, invert = TRUE)
  feature_columns <- grep("^X", names(df), value = TRUE)
  
  # Check for required columns
  if (!("Watershed" %in% meta_columns) || !("Fish_id" %in% meta_columns)) {
    cat("WARNING: Missing required metadata columns. Looking for alternates...\n")
    # Try to find alternate column names
    if ("watershed" %in% names(df)) {
      df$Watershed <- df$watershed
      meta_columns <- c(meta_columns, "Watershed")
    }
    if ("Fish_ID" %in% names(df)) {
      df$Fish_id <- df$Fish_ID
      meta_columns <- c(meta_columns, "Fish_id")
    }
    else if ("sample" %in% names(df)) {
      df$Fish_id <- df$sample
      meta_columns <- c(meta_columns, "Fish_id")
    }
  }
  
  # Make sure we have enough data
  if (length(feature_columns) < 10) {
    cat(sprintf("WARNING: Limited features in %s data (found %d). Continuing anyway.\n", 
                data_type, length(feature_columns)))
  }
  
  # Display dataset information
  cat(sprintf("Total samples: %d\n", nrow(df)))
  cat(sprintf("Features per sample: %d\n", length(feature_columns)))
  
  if ("Watershed" %in% names(df)) {
    watershed_table <- table(df$Watershed)
    cat("Watershed distribution:", 
        paste(names(watershed_table), watershed_table, sep = ": ", collapse = ", "), 
        "\n")
  }
  
  # Extract feature data for PCA
  X <- df[, feature_columns, drop = FALSE]
  
  # Perform PCA
  cat("Performing PCA...\n")
  pca_result <- prcomp(X, scale. = TRUE)
  
  # Get the loadings (eigenvectors)
  loadings <- pca_result$rotation
  
  # Create a function to plot the sample with loadings
  create_sample_plot <- function(sample_idx, loadings, pc_idx = 1) {
    # Clear any existing plots before creating new ones
    if (exists("last_plot", envir = .GlobalEnv)) rm(last_plot, envir = .GlobalEnv)
    invisible(lapply(names(dev.list()), function(x) dev.off()))
    
    # Get sample data
    sample <- as.numeric(df[sample_idx, feature_columns])
    
    # Get metadata
    metadata <- df[sample_idx, meta_columns, drop = FALSE]
    
    # Get loadings for the selected PC
    pc_loadings <- loadings[, pc_idx]
    
    # Get absolute loadings for coloring
    abs_loadings <- abs(pc_loadings)
    
    # Create a data frame for plotting
    plot_data <- data.frame(
      Index = 1:length(sample),
      Value = sample,
      AbsLoading = abs_loadings
    )
    
    # Get Fish ID and Watershed for title
    fish_id <- if ("Fish_id" %in% names(metadata)) metadata$Fish_id else paste("Sample", sample_idx)
    watershed <- if ("Watershed" %in% names(metadata)) metadata$Watershed else "Unknown"
    
    # Create the plot with REVERSED viridis color scheme
    # Only using points (no connecting lines)
    p <- ggplot(plot_data, aes(x = Index, y = Value)) +
      geom_point(aes(color = AbsLoading), size = 2, alpha = 0.8) +
      scale_color_viridis_c(
        option = "viridis",
        direction = -1,  # REVERSED color scheme
        name = sprintf("PC%d Loading\nMagnitude", pc_idx)
      ) +
      labs(
        title = sprintf("Otolith Timeseries: %s (%s)", 
                        fish_id, 
                        watershed),
        subtitle = sprintf("%s Data with PC%d Loading Magnitude", 
                           data_type, pc_idx),
        x = "Index",
        y = sprintf("%s Value", data_type)
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12),
        legend.position = "right",
        panel.grid.major = element_line(color = "gray80", linetype = "dashed"),
        panel.grid.minor = element_line(color = "gray90", linetype = "dotted")
      )
    
    return(list(
      plot = p,
      metadata = metadata
    ))
  }
  
  # Select samples
  if (is.null(sample_indices)) {
    sample_count <- min(30, nrow(df))
    sample_indices <- sample(1:nrow(df), sample_count)
  } else {
    # Filter indices that are out of bounds
    sample_indices <- sample_indices[sample_indices <= nrow(df)]
    sample_count <- length(sample_indices)
    cat(sprintf("Using %d pre-selected sample indices\n", sample_count))
  }
  
  # Create plots for each sample and each of the first 3 PCs
  for (pc_idx in 1:3) {  # For PC1, PC2, and PC3
    pc_dir <- file.path(type_dir, sprintf("PC%d", pc_idx))
    dir.create(pc_dir, recursive = TRUE, showWarnings = FALSE)
    
    cat(sprintf("Generating visualizations for PC%d...\n", pc_idx))
    
    # Create a summary list of all samples for this PC
    summary_data <- data.frame(
      Sample = integer(),
      Fish_ID = character(),
      Watershed = character(),
      File = character(),
      stringsAsFactors = FALSE
    )
    
    for (i in seq_along(sample_indices)) {
      sample_idx <- sample_indices[i]
      tryCatch({
        # Clear plots before each new one
        if (exists("last_plot", envir = .GlobalEnv)) rm(last_plot, envir = .GlobalEnv)
        graphics.off()
        
        result <- create_sample_plot(sample_idx, loadings, pc_idx = pc_idx)
        
        # Extract metadata for file naming
        metadata <- result$metadata
        
        # Get Fish ID
        fish_id <- if ("Fish_id" %in% names(metadata)) {
          as.character(metadata$Fish_id)
        } else {
          sprintf("Sample_%d", sample_idx)
        }
        
        # Clean up fish_id for filename (remove special characters)
        clean_fish_id <- gsub("[^a-zA-Z0-9]", "_", fish_id)
        
        # Get Watershed
        watershed <- if ("Watershed" %in% names(metadata)) {
          as.character(metadata$Watershed)
        } else {
          "Unknown"
        }
        
        # Create filename
        filename <- sprintf("sample_%02d_%s.png", i, clean_fish_id)
        
        # Add to summary
        summary_data <- rbind(summary_data, data.frame(
          Sample = i,
          Fish_ID = fish_id,
          Watershed = watershed,
          File = filename,
          stringsAsFactors = FALSE
        ))
        
        # Save the figure
        output_file <- file.path(pc_dir, filename)
        ggsave(output_file, result$plot, width = 10, height = 6, dpi = 300)
        
        if (i %% 5 == 0) {
          cat(sprintf("  Completed %d/%d samples\n", i, sample_count))
        }
      }, error = function(e) {
        cat(sprintf("ERROR processing sample %d: %s\n", sample_idx, e$message))
      })
    }
    
    # Save summary data
    write.csv(summary_data, file.path(pc_dir, "summary.csv"), row.names = FALSE)
  }
  
  # Create composite visualizations
  cat("Creating composite visualizations...\n")
  
  composite_dir <- file.path(type_dir, "composites")
  dir.create(composite_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Select a few samples for composite visualization
  composite_samples <- sample(sample_indices, min(5, length(sample_indices)))
  
  for (i in seq_along(composite_samples)) {
    # Clear plots before each composite
    if (exists("last_plot", envir = .GlobalEnv)) rm(last_plot, envir = .GlobalEnv)
    graphics.off()
    
    sample_idx <- composite_samples[i]
    tryCatch({
      # Get metadata for title
      if ("Fish_id" %in% names(df)) {
        fish_id <- df$Fish_id[sample_idx]
      } else {
        fish_id <- sprintf("Sample_%d", sample_idx)
      }
      
      if ("Watershed" %in% names(df)) {
        watershed <- df$Watershed[sample_idx]
      } else {
        watershed <- "Unknown"
      }
      
      # Clean fish_id for filename
      clean_fish_id <- gsub("[^a-zA-Z0-9]", "_", fish_id)
      
      # Create plots for PC1, PC2, PC3
      plots <- lapply(1:3, function(pc) tryCatch(
        create_sample_plot(sample_idx, loadings, pc_idx = pc)$plot,
        error = function(e) {
          cat(sprintf("ERROR creating PC%d plot for sample %d: %s\n", pc, sample_idx, e$message))
          NULL
        }
      ))
      
      # Filter out NULL plots
      plots <- plots[!sapply(plots, is.null)]
      
      if (length(plots) == 0) {
        cat(sprintf("WARNING: No valid plots for composite %d (sample %d)\n", i, sample_idx))
        next
      }
      
      # Combine plots vertically
      combined_plot <- plot_grid(
        plotlist = plots,
        ncol = 1,
        align = "v",
        labels = paste0("PC", seq_along(plots))
      )
      
      # Add title
      title <- ggdraw() + 
        draw_label(
          sprintf("Otolith Timeseries: %s (%s)\n%s Data with PC Loading Magnitudes", 
                  fish_id, watershed, data_type),
          fontface = "bold",
          size = 14
        )
      
      combined_plot_with_title <- plot_grid(
        title, combined_plot,
        ncol = 1,
        rel_heights = c(0.1, 0.9)
      )
      
      # Save the composite figure
      output_file <- file.path(composite_dir, sprintf("composite_%02d_%s.png", i, clean_fish_id))
      ggsave(output_file, combined_plot_with_title, width = 10, height = 15, dpi = 300)
      
    }, error = function(e) {
      cat(sprintf("ERROR creating composite for sample %d: %s\n", sample_idx, e$message))
    })
  }
  
  cat(sprintf("Completed processing %s data\n", data_type))
  return(sample_indices)
}

# Main execution logic
main <- function() {
  # Clear all existing plots at the start
  graphics.off()
  if (exists("last_plot", envir = .GlobalEnv)) rm(last_plot, envir = .GlobalEnv)
  invisible(lapply(names(dev.list()), function(x) dev.off()))
  cat("Cleared all existing plots and graphics devices\n")
  
  # Define data types to process
  data_types <- c("GAM", "RAW")
  
  # Look for both data types
  data_files <- list()
  for (data_type in data_types) {
    file_patterns <- c(
      paste0("preprocessed_", data_type, "\\.csv"),
      paste0("Processed_Core_Fw_", data_type, "\\.csv")
    )
    data_files[[data_type]] <- find_file(file_patterns)
  }
  
  # Print found files
  cat("Found data files:\n")
  for (data_type in names(data_files)) {
    if (!is.null(data_files[[data_type]])) {
      cat(sprintf("  %s: %s\n", data_type, data_files[[data_type]]))
    } else {
      cat(sprintf("  %s: Not found\n", data_type))
    }
  }
  
  # Check if we have at least one of the required data types
  if (all(sapply(data_files, is.null))) {
    stop("Neither GAM nor RAW data files found. Please check the data directory.")
  }
  
  # Create main output directory
  output_dir <- "figures/pca_loadings_timeseries"
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Save processing start time
  start_time <- Sys.time()
  cat(sprintf("Started processing at: %s\n", start_time))
  
  # Process first available data type and get sample indices
  sample_indices <- NULL
  for (data_type in data_types) {
    if (!is.null(data_files[[data_type]])) {
      cat(sprintf("\nProcessing first data type: %s\n", data_type))
      sample_indices <- process_data_type(data_type, data_files[[data_type]], output_dir)
      break
    }
  }
  
  # Process remaining data types with the same sample indices
  for (data_type in data_types) {
    if (!is.null(data_files[[data_type]]) && data_type != names(data_files)[1]) {
      cat(sprintf("\nProcessing data type: %s (using same sample indices)\n", data_type))
      process_data_type(data_type, data_files[[data_type]], output_dir, sample_indices)
    }
  }
  
  # Calculate and display processing time
  end_time <- Sys.time()
  processing_time <- difftime(end_time, start_time, units = "mins")
  cat(sprintf("\nProcessing completed at: %s\n", end_time))
  cat(sprintf("Total processing time: %.2f minutes\n", as.numeric(processing_time)))
  cat(sprintf("\nOutput saved to: %s\n", normalizePath(output_dir)))
  
  # Clear plots at the end
  graphics.off()
  if (exists("last_plot", envir = .GlobalEnv)) rm(last_plot, envir = .GlobalEnv)
  invisible(lapply(names(dev.list()), function(x) dev.off()))
}

# Run the main function
main()