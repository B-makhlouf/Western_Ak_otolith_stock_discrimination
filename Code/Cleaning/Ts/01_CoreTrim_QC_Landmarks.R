library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(plotly)
library(ggplot2)
library(cowplot)
library(tidyverse)
library(mgcv)
library(here)

# =============================================================================
# CONFIGURATION
# =============================================================================

# Define directories
INPUT_DIR <- here("Data/Processed/Trim_Locations")
GOOD_OUTPUT_DIR <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/data/LA_Data/AnalysisReady"
REVISE_OUTPUT_DIR <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/data/LA_Data/NeedsRevision"
RESULTS_FILE <- file.path(INPUT_DIR, "processing_results.csv")

# Create output directories
dir.create(GOOD_OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(REVISE_OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# DATA INITIALIZATION
# =============================================================================

# Get all input files
input_files <- list.files(INPUT_DIR, pattern = "_trimLocations.csv", full.names = TRUE)
all_fish_ids <- sapply(input_files, function(file) {
  tryCatch({
    data <- read.csv(file)
    return(data$Fish_id[1])
  }, error = function(e) return(NA))
})

# Remove invalid files
valid_files <- input_files[!is.na(all_fish_ids)]
valid_fish_ids <- all_fish_ids[!is.na(all_fish_ids)]

# Load existing results
if (file.exists(RESULTS_FILE) && file.info(RESULTS_FILE)$size > 0) {
  existing_results <- read.csv(RESULTS_FILE, stringsAsFactors = FALSE)
  processed_ids <- existing_results$Fish_ID
  remaining_files <- valid_files[!valid_fish_ids %in% processed_ids]
  remaining_ids <- valid_fish_ids[!valid_fish_ids %in% processed_ids]
} else {
  existing_results <- data.frame(
    Fish_ID = character(),
    QC_Grade = character(),
    Core_Start = numeric(),
    Processing_Date = character(),
    stringsAsFactors = FALSE
  )
  remaining_files <- valid_files
  remaining_ids <- valid_fish_ids
}

# =============================================================================
# UI
# =============================================================================

ui <- dashboardPage(
  dashboardHeader(title = "Otolith Processing App"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Processing", tabName = "processing", icon = icon("fish")),
      menuItem("Results", tabName = "results", icon = icon("table"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .btn-qc { margin: 2px; padding: 8px 12px; font-size: 14px; font-weight: bold; }
        .btn-good { background-color: #28a745; color: white; }
        .btn-revise { background-color: #ffc107; color: black; }
        .btn-delete { background-color: #dc3545; color: white; }
        .progress-box { padding: 10px; margin: 5px 0; }
        .sample-info { font-size: 16px; font-weight: bold; }
        .hotkey-info { 
          background-color: #f8f9fa; 
          border: 1px solid #dee2e6; 
          border-radius: 3px; 
          padding: 8px; 
          margin: 8px 0; 
          font-family: monospace;
        }
        .box { margin-bottom: 10px !important; }
        .box-header { padding: 8px 15px !important; }
        .box-body { padding: 10px !important; }
        .content-wrapper { padding: 10px !important; }
      ")),
      tags$script(HTML("
        $(document).on('keydown', function(e) {
          // Only process if not typing in an input field
          if (!$(e.target).is('input, textarea, select')) {
            switch(e.which) {
              case 71: // 'g' key for Good
                e.preventDefault();
                Shiny.setInputValue('hotkey_good', Math.random());
                break;
              case 82: // 'r' key for Revise  
                e.preventDefault();
                Shiny.setInputValue('hotkey_revise', Math.random());
                break;
              case 68: // 'd' key for Delete
                e.preventDefault();
                Shiny.setInputValue('hotkey_delete', Math.random());
                break;
              case 39: // Right arrow for Next
                e.preventDefault();
                Shiny.setInputValue('hotkey_next', Math.random());
                break;
              case 37: // Left arrow for Previous
                e.preventDefault();
                Shiny.setInputValue('hotkey_prev', Math.random());
                break;
              case 13: // Enter key for Next (after QC)
                e.preventDefault();
                Shiny.setInputValue('hotkey_enter', Math.random());
                break;
            }
          }
        });
      "))
    ),
    
    tabItems(
      # Processing Tab
      tabItem(tabName = "processing",
              fluidRow(
                # Left sidebar with controls
                column(width = 3,
                       # Sample Progress
                       box(width = 12, title = "Sample Progress", status = "primary",
                           div(class = "sample-info",
                               h4("Current Sample:"),
                               h3(textOutput("current_fish_id"), style = "margin-bottom: 15px;")
                           ),
                           div(class = "progress-box",
                               h5(textOutput("progress_text")),
                               progressBar(
                                 id = "progress_bar",
                                 value = 0,
                                 total = length(remaining_files),
                                 title = "",
                                 display_pct = TRUE
                               )
                           ),
                           div(style = "text-align: center; margin-top: 15px;",
                               actionButton("prev_btn", "← Previous", class = "btn btn-secondary", style = "width: 100%; margin-bottom: 10px;"),
                               actionButton("next_btn", "Next →", class = "btn btn-primary", style = "width: 100%;")
                           )
                       ),
                       
                       # QC Controls
                       box(width = 12, title = "Quality Control", status = "warning",
                           div(class = "hotkey-info",
                               h5("🎹 Shortcuts:", style = "margin-top: 0; margin-bottom: 8px;"),
                               p(style = "font-size: 12px; margin-bottom: 4px;",
                                 tags$strong("G"), " = Good | ",
                                 tags$strong("R"), " = Revise | ", 
                                 tags$strong("D"), " = Delete"),
                               p(style = "font-size: 12px; margin-bottom: 8px;",
                                 tags$strong("← →"), " = Prev/Next | ",
                                 tags$strong("Enter"), " = Next")
                           ),
                           div(style = "text-align: center;",
                               actionButton("qc_good", "GOOD (G)", class = "btn-qc btn-good", style = "width: 100%; margin-bottom: 5px;"),
                               actionButton("qc_revise", "REVISE (R)", class = "btn-qc btn-revise", style = "width: 100%; margin-bottom: 5px;"),
                               actionButton("qc_delete", "DELETE (D)", class = "btn-qc btn-delete", style = "width: 100%; margin-bottom: 10px;"),
                               h6(textOutput("qc_status"))
                           )
                       ),
                       
                       # Core Selection Status
                       conditionalPanel(
                         condition = "output.show_core_selection",
                         box(width = 12, title = "Core Selection", status = "success",
                             h6("Click on Sr87/86 plot to select core start:"),
                             h6(textOutput("core_selection_status"))
                         )
                       )
                ),
                
                # Right side with plots
                column(width = 9,
                       # Plots stacked vertically
                       box(width = 12, title = "Otolith Data Plots", status = "info",
                           h5("Sr88 Data"),
                           plotlyOutput("sr88_plot", height = "300px"),
                           br(),
                           h5("Sr87/86 Data (Click to select core start)"),
                           plotlyOutput("sr8786_plot", height = "400px")
                       ),
                       
                       # Final Result Preview (separate box, appears when core selected)
                       conditionalPanel(
                         condition = "output.show_final_plots",
                         box(width = 12, title = "Final Result with Landmarks", status = "success",
                             plotlyOutput("final_plot", height = "400px")
                         )
                       )
                )
              )
      ),
      
      # Results Tab
      tabItem(tabName = "results",
              fluidRow(
                column(width = 12,
                       box(width = 12, title = "Processing Results Summary", status = "primary",
                           fluidRow(
                             column(4,
                                    valueBoxOutput("total_processed", width = 12)
                             ),
                             column(4,
                                    valueBoxOutput("good_count", width = 12)
                             ),
                             column(4,
                                    valueBoxOutput("remaining_count", width = 12)
                             )
                           )
                       )
                )
              ),
              
              fluidRow(
                column(width = 12,
                       box(width = 12, title = "Detailed Results", status = "info",
                           DT::dataTableOutput("results_table"),
                           br(),
                           downloadButton("download_results", "Download Results CSV", class = "btn btn-success")
                       )
                )
              )
      )
    )
  )
)

# =============================================================================
# SERVER
# =============================================================================

server <- function(input, output, session) {
  
  # Reactive values
  values <- reactiveValues(
    current_index = 1,
    current_data = NULL,
    current_fish_id = NULL,
    qc_grade = NULL,
    core_start = NULL,
    results = existing_results,
    processed_current = FALSE
  )
  
  # Load current sample data
  observe({
    if (values$current_index <= length(remaining_files)) {
      file_path <- remaining_files[values$current_index]
      values$current_data <- read_csv(file_path, show_col_types = FALSE)
      values$current_fish_id <- values$current_data$Fish_id[1]
      values$qc_grade <- "GOOD"  # Default to GOOD
      values$core_start <- NULL
      values$processed_current <- FALSE
    }
  })
  
  # Progress text
  output$progress_text <- renderText({
    paste("Progress:", values$current_index - 1, "of", length(remaining_files), "completed")
  })
  
  # Update progress bar
  observe({
    updateProgressBar(
      session = session,
      id = "progress_bar",
      value = values$current_index - 1
    )
  })
  
  # Current fish ID
  output$current_fish_id <- renderText({
    if (!is.null(values$current_fish_id)) {
      values$current_fish_id
    } else {
      "No more samples"
    }
  })
  
  # QC Status
  output$qc_status <- renderText({
    if (!is.null(values$qc_grade)) {
      paste("QC Grade:", values$qc_grade)
    } else {
      "No QC grade assigned"
    }
  })
  
  # Show core selection panel
  output$show_core_selection <- reactive({
    !is.null(values$qc_grade) && values$qc_grade == "GOOD"
  })
  outputOptions(output, "show_core_selection", suspendWhenHidden = FALSE)
  
  # Core selection status
  output$core_selection_status <- renderText({
    if (!is.null(values$core_start)) {
      paste("✓ Core start selected at:", values$core_start, "μm - Ready to proceed!")
    } else if (!is.null(values$qc_grade) && values$qc_grade == "GOOD") {
      "👆 Click on the Sr87/86 plot above to select core start location"
    } else {
      "First assign QC grade as 'GOOD', then click on plot"
    }
  })
  
  # Show final plots
  output$show_final_plots <- reactive({
    !is.null(values$qc_grade) && values$qc_grade == "GOOD" && !is.null(values$core_start)
  })
  outputOptions(output, "show_final_plots", suspendWhenHidden = FALSE)
  
  # Sr88 Plot
  output$sr88_plot <- renderPlotly({
    if (!is.null(values$current_data)) {
      p <- ggplot(values$current_data, aes(x = Microns, y = Sr88)) +
        geom_point(color = "blue", size = 1) +
        geom_vline(xintercept = values$current_data$marine_start[1], 
                   color = "red", linetype = "dashed", alpha = 0.7) +
        labs(title = paste("Sr88 -", values$current_fish_id), 
             x = "Microns", y = "Sr88") +
        theme_minimal()
      
      ggplotly(p, tooltip = c("x", "y")) %>%
        config(displayModeBar = FALSE)
    }
  })
  
  # Sr87/86 Plot - This is where we capture clicks
  output$sr8786_plot <- renderPlotly({
    if (!is.null(values$current_data)) {
      # Create GAM model
      n <- nrow(values$current_data)
      k <- floor(15 * (n^(2/9)))
      model <- gam(Iso ~ s(Microns, bs="tp", k=k), gamma=0.9, data=values$current_data)
      
      predictions <- predict(model, se=TRUE)
      fit <- predictions$fit
      se <- predictions$se.fit
      
      plot_data <- values$current_data %>%
        mutate(
          gam_fit = fit,
          gam_lower = fit - 1.96 * se,
          gam_upper = fit + 1.96 * se
        )
      
      p <- ggplot(plot_data, aes(x = Microns, y = Iso)) +
        geom_ribbon(aes(ymin = gam_lower, ymax = gam_upper), 
                    alpha = 0.3, fill = "gold") +
        geom_point(alpha = 0.5, size = 1) +
        geom_line(aes(y = Iso_MA), color = "red", linewidth = 1) +
        geom_line(aes(y = gam_fit), color = "black", linewidth = 1.5) +
        geom_vline(xintercept = values$current_data$marine_start[1], 
                   color = "red", linetype = "dashed", alpha = 0.7) +
        geom_vline(xintercept = values$current_data$natal_microns_start[1], 
                   color = "blue", linetype = "dashed", alpha = 0.7) +
        geom_vline(xintercept = values$current_data$natal_microns_end[1], 
                   color = "blue", linetype = "dashed", alpha = 0.7) +
        geom_hline(yintercept = 0.7092, color = "blue", alpha = 0.7) +
        labs(title = paste("Sr87/86 -", values$current_fish_id, "(Click to select core start)"), 
             x = "Microns", y = "87Sr/86Sr") +
        theme_minimal()
      
      # Add core start line if selected
      if (!is.null(values$core_start)) {
        p <- p + geom_vline(xintercept = values$core_start, 
                            color = "purple", linewidth = 2, linetype = "solid")
      }
      
      ggplotly(p, tooltip = c("x", "y"), source = "sr8786_plot") %>%
        config(displayModeBar = FALSE)
    }
  })
  
  # Handle plotly clicks for core selection
  observeEvent(event_data("plotly_click", source = "sr8786_plot"), {
    click_data <- event_data("plotly_click", source = "sr8786_plot")
    
    if (!is.null(values$qc_grade) && values$qc_grade == "GOOD" && !is.null(click_data)) {
      values$core_start <- round(click_data$x, 1)
      
      showNotification(
        paste("✓ Core start selected at:", values$core_start, "μm"),
        type = "message",
        duration = 3
      )
    } else if (!is.null(click_data) && (is.null(values$qc_grade) || values$qc_grade != "GOOD")) {
      showNotification(
        "⚠️ Please set QC grade to 'GOOD' first before selecting core start",
        type = "warning",
        duration = 3
      )
    }
  }, ignoreInit = TRUE)
  
  # Final plot with landmarks
  output$final_plot <- renderPlotly({
    if (!is.null(values$current_data) && !is.null(values$core_start)) {
      # Add landmarks
      data_with_landmarks <- values$current_data
      data_with_landmarks$Core_Start <- values$core_start
      data_with_landmarks$Landmark <- NA
      
      # Find indices and assign landmarks
      core_start_index <- which.min(abs(data_with_landmarks$Microns - values$core_start))
      natal_start_index <- which.min(abs(data_with_landmarks$Microns - data_with_landmarks$natal_microns_start[1]))
      marine_index <- which.min(abs(data_with_landmarks$Microns - data_with_landmarks$marine_start[1]))
      early_marine_index <- which.min(abs(data_with_landmarks$Microns - (data_with_landmarks$marine_start[1] + 200)))
      
      data_with_landmarks$Landmark[core_start_index:natal_start_index] <- "Core"
      data_with_landmarks$Landmark[natal_start_index:marine_index] <- "Fw"
      data_with_landmarks$Landmark[marine_index:early_marine_index] <- "Early Marine"
      
      # Store for saving
      values$final_data <- data_with_landmarks
      
      p <- ggplot(data_with_landmarks, aes(x = Microns, y = Iso, color = Landmark)) +
        geom_point(size = 1) +
        geom_line(aes(y = Iso_MA), color = "gray20", linewidth = 1.2) +
        geom_vline(xintercept = values$core_start, 
                   color = "purple", linewidth = 2, linetype = "solid") +
        scale_color_manual(values = c("Core" = "dodgerblue", "Fw" = "firebrick", "Early Marine" = "darkorange"),
                           na.value = "gray") +
        labs(title = paste("Final Result - Core Start:", round(values$core_start, 1), "μm -", values$current_fish_id), 
             x = "Microns", y = "87Sr/86Sr") +
        theme_minimal()
      
      ggplotly(p, tooltip = c("x", "y", "colour")) %>%
        config(displayModeBar = FALSE)
    }
  })
  
  # Hotkey Handlers
  observeEvent(input$hotkey_good, {
    values$qc_grade <- "GOOD"
    showNotification("QC: GOOD (G key)", type = "message", duration = 1)
  })
  
  observeEvent(input$hotkey_revise, {
    values$qc_grade <- "REVISE"
    showNotification("QC: REVISE (R key)", type = "message", duration = 1)
    save_current_sample()
  })
  
  observeEvent(input$hotkey_delete, {
    values$qc_grade <- "DELETE"
    showNotification("QC: DELETE (D key)", type = "message", duration = 1)
    save_current_sample()
  })
  
  observeEvent(input$hotkey_next, {
    if (values$current_index < length(remaining_files)) {
      values$current_index <- values$current_index + 1
      showNotification("Next sample (→ key)", type = "message", duration = 1)
    }
  })
  
  observeEvent(input$hotkey_prev, {
    if (values$current_index > 1) {
      values$current_index <- values$current_index - 1
      showNotification("Previous sample (← key)", type = "message", duration = 1)
    }
  })
  
  observeEvent(input$hotkey_enter, {
    if (!is.null(values$qc_grade) && values$processed_current) {
      if (values$current_index < length(remaining_files)) {
        values$current_index <- values$current_index + 1
        showNotification("Moving to next sample (Enter key)", type = "message", duration = 1)
      }
    }
  })
  
  # QC Button Handlers (keep for backup)
  observeEvent(input$qc_good, {
    values$qc_grade <- "GOOD"
  })
  
  observeEvent(input$qc_revise, {
    values$qc_grade <- "REVISE"
    save_current_sample()
  })
  
  observeEvent(input$qc_delete, {
    values$qc_grade <- "DELETE"
    save_current_sample()
  })
  
  # Save current sample function
  save_current_sample <- function() {
    if (!is.null(values$qc_grade) && !values$processed_current) {
      
      # Save individual CSV file
      if (values$qc_grade == "GOOD" && !is.null(values$final_data)) {
        output_file <- file.path(GOOD_OUTPUT_DIR, paste0(values$current_fish_id, "_AnalysisReady.csv"))
        write_csv(values$final_data, output_file)
      } else if (values$qc_grade == "REVISE") {
        output_file <- file.path(REVISE_OUTPUT_DIR, paste0(values$current_fish_id, "_NeedsRevision.csv"))
        write_csv(values$current_data, output_file)
      }
      
      # Update results tracking
      new_result <- data.frame(
        Fish_ID = values$current_fish_id,
        QC_Grade = values$qc_grade,
        Core_Start = ifelse(is.null(values$core_start), NA, values$core_start),
        Processing_Date = as.character(Sys.Date()),
        stringsAsFactors = FALSE
      )
      
      values$results <- bind_rows(values$results, new_result)
      write.csv(values$results, RESULTS_FILE, row.names = FALSE)
      
      values$processed_current <- TRUE
      
      # Fixed: Changed from "success" to "message"
      showNotification(
        paste("Saved:", values$current_fish_id, "- QC:", values$qc_grade),
        type = "message",
        duration = 3
      )
    }
  }
  
  # Save when core start is selected for GOOD samples
  observe({
    if (!is.null(values$qc_grade) && values$qc_grade == "GOOD" && !is.null(values$core_start)) {
      save_current_sample()
    }
  })
  
  # Navigation buttons
  observeEvent(input$next_btn, {
    if (values$current_index < length(remaining_files)) {
      values$current_index <- values$current_index + 1
    }
  })
  
  observeEvent(input$prev_btn, {
    if (values$current_index > 1) {
      values$current_index <- values$current_index - 1
    }
  })
  
  # Results tab outputs
  output$total_processed <- renderValueBox({
    valueBox(
      value = nrow(values$results),
      subtitle = "Total Processed",
      icon = icon("check"),
      color = "blue"
    )
  })
  
  output$good_count <- renderValueBox({
    valueBox(
      value = sum(values$results$QC_Grade == "GOOD", na.rm = TRUE),
      subtitle = "Good Samples",
      icon = icon("thumbs-up"),
      color = "green"
    )
  })
  
  output$remaining_count <- renderValueBox({
    valueBox(
      value = length(remaining_files) - nrow(values$results),
      subtitle = "Remaining",
      icon = icon("clock"),
      color = "yellow"
    )
  })
  
  output$results_table <- DT::renderDataTable({
    DT::datatable(values$results, options = list(pageLength = 15, scrollX = TRUE))
  })
  
  output$download_results <- downloadHandler(
    filename = function() {
      paste("otolith_processing_results_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(values$results, file, row.names = FALSE)
    }
  )
}

# =============================================================================
# RUN APP
# =============================================================================

shinyApp(ui = ui, server = server)