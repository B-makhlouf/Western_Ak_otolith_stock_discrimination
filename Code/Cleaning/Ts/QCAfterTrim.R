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
ANALYSIS_READY_DIR <- "/Users/benjaminmakhlouf/Research_repos/04_Western_Ak_otolith_stock_discrimination/data/LA_Data/AnalysisReady"
CORRECTIONS_LOG_FILE <- file.path(ANALYSIS_READY_DIR, "landmark_corrections_log.csv")

# =============================================================================
# DATA INITIALIZATION
# =============================================================================

# Get all "Good" files
good_files <- list.files(ANALYSIS_READY_DIR, pattern = "_AnalysisReady.csv", full.names = TRUE)
all_fish_ids <- sapply(good_files, function(file) {
  tryCatch({
    data <- read.csv(file)
    return(data$Fish_id[1])
  }, error = function(e) {
    return(NA)
  })
})

# Remove invalid files
valid_files <- good_files[!is.na(all_fish_ids)]
valid_fish_ids <- all_fish_ids[!is.na(all_fish_ids)]

# Load existing corrections log
if (file.exists(CORRECTIONS_LOG_FILE) && file.info(CORRECTIONS_LOG_FILE)$size > 0) {
  existing_corrections <- read.csv(CORRECTIONS_LOG_FILE, stringsAsFactors = FALSE)
  reviewed_ids <- existing_corrections$Fish_ID
  remaining_files <- valid_files[!valid_fish_ids %in% reviewed_ids]
  remaining_ids <- valid_fish_ids[!valid_fish_ids %in% reviewed_ids]
} else {
  existing_corrections <- data.frame(
    Fish_ID = character(),
    Original_Natal_Start = numeric(),
    Original_Natal_End = numeric(),
    Original_Marine_Start = numeric(),
    Corrected_Natal_Start = numeric(),
    Corrected_Natal_End = numeric(),
    Corrected_Marine_Start = numeric(),
    Changes_Made = character(),
    Review_Date = character(),
    stringsAsFactors = FALSE
  )
  remaining_files <- valid_files
  remaining_ids <- valid_fish_ids
}

# =============================================================================
# UI
# =============================================================================

ui <- dashboardPage(
  dashboardHeader(title = "Landmark Refinement App"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Landmark Review", tabName = "review", icon = icon("crosshairs")),
      menuItem("Corrections Log", tabName = "log", icon = icon("table"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .btn-landmark { margin: 2px; padding: 8px 12px; font-size: 14px; font-weight: bold; }
        .btn-natal-start { background-color: #007bff; color: white; }
        .btn-natal-end { background-color: #6f42c1; color: white; }
        .btn-marine-start { background-color: #dc3545; color: white; }
        .btn-delete { background-color: #dc3545; color: white; }
        .btn-save { background-color: #28a745; color: white; }
        .btn-skip { background-color: #6c757d; color: white; }
        .progress-box { padding: 10px; margin: 5px 0; }
        .sample-info { font-size: 16px; font-weight: bold; }
        .landmark-info { 
          background-color: #f8f9fa; 
          border: 1px solid #dee2e6; 
          border-radius: 3px; 
          padding: 8px; 
          margin: 8px 0; 
          font-family: monospace;
          font-size: 12px;
        }
        .hotkey-info { 
          background-color: #e3f2fd; 
          border: 1px solid #bbdefb; 
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
          if (!$(e.target).is('input, textarea, select')) {
            switch(e.which) {
              case 49: // '1' key for Natal Start
                e.preventDefault();
                Shiny.setInputValue('hotkey_natal_start', Math.random());
                break;
              case 50: // '2' key for Natal End
                e.preventDefault();
                Shiny.setInputValue('hotkey_natal_end', Math.random());
                break;
              case 51: // '3' key for Marine Start
                e.preventDefault();
                Shiny.setInputValue('hotkey_marine_start', Math.random());
                break;
              case 68: // 'd' key for Delete
                e.preventDefault();
                Shiny.setInputValue('hotkey_delete', Math.random());
                break;
              case 83: // 's' key for Save
                e.preventDefault();
                Shiny.setInputValue('hotkey_save', Math.random());
                break;
              case 75: // 'k' key for Skip
                e.preventDefault();
                Shiny.setInputValue('hotkey_skip', Math.random());
                break;
              case 39: // Right arrow for Next
                e.preventDefault();
                Shiny.setInputValue('hotkey_next', Math.random());
                break;
              case 37: // Left arrow for Previous
                e.preventDefault();
                Shiny.setInputValue('hotkey_prev', Math.random());
                break;
            }
          }
        });
      "))
    ),
    
    tabItems(
      # Review Tab
      tabItem(tabName = "review",
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
                           # Quick Jump to Fish ID
                           div(style = "margin: 10px 0; padding: 8px; background-color: #e8f4f8; border-radius: 3px;",
                               h6("🎯 Quick Jump:", style = "margin: 0 0 5px 0; font-weight: bold;"),
                               selectInput("jump_to_fish", NULL, 
                                           choices = setNames(1:length(remaining_files), remaining_ids),
                                           selected = 1, width = "100%"),
                               actionButton("jump_btn", "Jump to Sample", 
                                            class = "btn btn-info btn-sm", 
                                            style = "width: 100%; font-size: 12px;")
                           ),
                           div(style = "text-align: center; margin-top: 15px;",
                               actionButton("prev_btn", "← Previous", class = "btn btn-secondary", style = "width: 100%; margin-bottom: 10px;"),
                               actionButton("next_btn", "Next →", class = "btn btn-primary", style = "width: 100%;")
                           )
                       ),
                       
                       # Landmark Information
                       box(width = 12, title = "Current Landmarks", status = "info",
                           div(class = "landmark-info",
                               h5("📍 Current Values:", style = "margin-top: 0; margin-bottom: 8px;"),
                               tags$div(id = "landmark_values_display"),
                               h5("🎯 Selected Values:", style = "margin-top: 10px; margin-bottom: 8px;"),
                               tags$div(id = "selected_values_display")
                           )
                       ),
                       
                       # Landmark Selection Controls
                       box(width = 12, title = "Landmark Selection", status = "warning",
                           div(class = "hotkey-info",
                               h5("🎹 Click Mode Shortcuts:", style = "margin-top: 0; margin-bottom: 8px;"),
                               p(style = "font-size: 12px; margin-bottom: 4px;",
                                 tags$strong("1"), " = Natal Start | ",
                                 tags$strong("2"), " = Natal End | ", 
                                 tags$strong("3"), " = Marine Start"),
                               p(style = "font-size: 12px; margin-bottom: 8px;",
                                 tags$strong("S"), " = Save/Next | ",
                                 tags$strong("K"), " = Skip | ",
                                 tags$strong("D"), " = Delete | ",
                                 tags$strong("← →"), " = Nav")
                           ),
                           div(style = "text-align: center;",
                               actionButton("select_natal_start", "1. Natal Start", 
                                            class = "btn-landmark btn-natal-start", 
                                            style = "width: 100%; margin-bottom: 5px;"),
                               actionButton("select_natal_end", "2. Natal End", 
                                            class = "btn-landmark btn-natal-end", 
                                            style = "width: 100%; margin-bottom: 5px;"),
                               actionButton("select_marine_start", "3. Marine Start", 
                                            class = "btn-landmark btn-marine-start", 
                                            style = "width: 100%; margin-bottom: 10px;"),
                               hr(),
                               actionButton("save_corrections", "💾 SAVE & NEXT (S)", 
                                            class = "btn-landmark btn-save", 
                                            style = "width: 100%; margin-bottom: 5px;"),
                               actionButton("skip_sample", "⏭️ SKIP & NEXT (K)", 
                                            class = "btn-landmark btn-skip", 
                                            style = "width: 100%; margin-bottom: 5px;"),
                               actionButton("delete_sample", "🗑️ DELETE FILE (D)", 
                                            class = "btn-landmark btn-delete", 
                                            style = "width: 100%;")
                           ),
                           h6(textOutput("selection_status"))
                       )
                ),
                
                # Right side with plots
                column(width = 9,
                       # Plots stacked vertically - showing only data after core start
                       box(width = 12, title = "Landmark Refinement (Data after Core Start)", status = "info",
                           h5("Sr88 Data"),
                           plotlyOutput("sr88_plot", height = "300px"),
                           br(),
                           h5("Sr87/86 Data (Click to adjust landmarks)"),
                           plotlyOutput("sr8786_plot", height = "400px")
                       ),
                       
                       # Summary of changes
                       conditionalPanel(
                         condition = "output.show_changes_summary",
                         box(width = 12, title = "Changes Summary", status = "success",
                             verbatimTextOutput("changes_summary")
                         )
                       )
                )
              )
      ),
      
      # Log Tab
      tabItem(tabName = "log",
              fluidRow(
                column(width = 12,
                       box(width = 12, title = "Corrections Summary", status = "primary",
                           fluidRow(
                             column(4,
                                    valueBoxOutput("total_reviewed", width = 12)
                             ),
                             column(4,
                                    valueBoxOutput("corrections_made", width = 12)
                             ),
                             column(4,
                                    valueBoxOutput("remaining_samples", width = 12)
                             )
                           )
                       )
                )
              ),
              
              fluidRow(
                column(width = 12,
                       box(width = 12, title = "Corrections Log", status = "info",
                           DT::dataTableOutput("corrections_table"),
                           br(),
                           downloadButton("download_log", "Download Corrections Log", class = "btn btn-success")
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
    original_natal_start = NULL,
    original_natal_end = NULL,
    original_marine_start = NULL,
    selected_natal_start = NULL,
    selected_natal_end = NULL,
    selected_marine_start = NULL,
    core_start = NULL,
    selection_mode = "none", # "natal_start", "natal_end", "marine_start"
    corrections_log = existing_corrections,
    changes_made = FALSE,
    jump_target = NULL
  )
  
  # Load current sample data
  observe({
    if (values$current_index <= length(remaining_files)) {
      file_path <- remaining_files[values$current_index]
      values$current_data <- read_csv(file_path, show_col_types = FALSE)
      values$current_fish_id <- values$current_data$Fish_id[1]
      
      # Update the jump selector to reflect current sample
      updateSelectInput(session, "jump_to_fish", selected = values$current_index)
      
      # Store original landmarks
      values$original_natal_start <- values$current_data$natal_microns_start[1]
      values$original_natal_end <- values$current_data$natal_microns_end[1]
      values$original_marine_start <- values$current_data$marine_start[1]
      values$core_start <- values$current_data$Core_Start[1]
      
      # Initialize selected landmarks to original values
      values$selected_natal_start <- values$original_natal_start
      values$selected_natal_end <- values$original_natal_end
      values$selected_marine_start <- values$original_marine_start
      
      values$selection_mode <- "none"
      values$changes_made <- FALSE
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
      value = values$current_index - 1,
      total = length(remaining_files)
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
  
  # Landmark values display
  output$landmark_values_display <- renderUI({
    if (!is.null(values$current_data)) {
      tagList(
        tags$p(sprintf("Natal Start: %.1f μm", values$original_natal_start), style = "margin: 2px 0; color: #007bff;"),
        tags$p(sprintf("Natal End: %.1f μm", values$original_natal_end), style = "margin: 2px 0; color: #6f42c1;"),
        tags$p(sprintf("Marine Start: %.1f μm", values$original_marine_start), style = "margin: 2px 0; color: #dc3545;"),
        tags$p(sprintf("Core Start: %.1f μm", values$core_start), style = "margin: 2px 0; color: #6c757d;")
      )
    }
  })
  
  # Selected values display
  output$selected_values_display <- renderUI({
    if (!is.null(values$selected_natal_start)) {
      tagList(
        tags$p(sprintf("Natal Start: %.1f μm", values$selected_natal_start), 
               style = paste0("margin: 2px 0; color: #007bff; font-weight: ", 
                              ifelse(abs(values$selected_natal_start - values$original_natal_start) > 0.1, "bold", "normal"))),
        tags$p(sprintf("Natal End: %.1f μm", values$selected_natal_end), 
               style = paste0("margin: 2px 0; color: #6f42c1; font-weight: ", 
                              ifelse(abs(values$selected_natal_end - values$original_natal_end) > 0.1, "bold", "normal"))),
        tags$p(sprintf("Marine Start: %.1f μm", values$selected_marine_start), 
               style = paste0("margin: 2px 0; color: #dc3545; font-weight: ", 
                              ifelse(abs(values$selected_marine_start - values$original_marine_start) > 0.1, "bold", "normal")))
      )
    }
  })
  
  # Selection status
  output$selection_status <- renderText({
    mode_text <- switch(values$selection_mode,
                        "natal_start" = "Click on plot to set Natal Start",
                        "natal_end" = "Click on plot to set Natal End", 
                        "marine_start" = "Click on plot to set Marine Start",
                        "Ready to save or navigate")
    
    if (values$changes_made) {
      paste("⚠️", mode_text, "| Changes pending!")
    } else {
      mode_text
    }
  })
  
  # Show changes summary
  output$show_changes_summary <- reactive({
    values$changes_made
  })
  outputOptions(output, "show_changes_summary", suspendWhenHidden = FALSE)
  
  # Changes summary
  output$changes_summary <- renderText({
    if (values$changes_made) {
      changes <- c()
      if (abs(values$selected_natal_start - values$original_natal_start) > 0.1) {
        changes <- c(changes, sprintf("Natal Start: %.1f → %.1f", values$original_natal_start, values$selected_natal_start))
      }
      if (abs(values$selected_natal_end - values$original_natal_end) > 0.1) {
        changes <- c(changes, sprintf("Natal End: %.1f → %.1f", values$original_natal_end, values$selected_natal_end))
      }
      if (abs(values$selected_marine_start - values$original_marine_start) > 0.1) {
        changes <- c(changes, sprintf("Marine Start: %.1f → %.1f", values$original_marine_start, values$selected_marine_start))
      }
      
      if (length(changes) > 0) {
        paste("Changes made:\n", paste(changes, collapse = "\n"))
      } else {
        "No changes detected"
      }
    }
  })
  
  # Sr88 Plot - showing only data after core start
  output$sr88_plot <- renderPlotly({
    if (!is.null(values$current_data) && !is.null(values$core_start)) {
      # Filter data to show only after core start
      filtered_data <- values$current_data %>% 
        filter(Microns >= values$core_start)
      
      p <- ggplot(filtered_data, aes(x = Microns, y = Sr88)) +
        geom_point(color = "blue", size = 1, alpha = 0.6) +
        geom_vline(xintercept = values$selected_natal_start, 
                   color = "#007bff", linetype = "solid", linewidth = 1.5, alpha = 0.8) +
        geom_vline(xintercept = values$selected_natal_end, 
                   color = "#6f42c1", linetype = "solid", linewidth = 1.5, alpha = 0.8) +
        geom_vline(xintercept = values$selected_marine_start, 
                   color = "#dc3545", linetype = "solid", linewidth = 1.5, alpha = 0.8) +
        labs(title = paste("Sr88 -", values$current_fish_id, "(After Core Start)"), 
             x = "Microns", y = "Sr88") +
        theme_minimal() +
        theme(plot.title = element_text(size = 12))
      
      ggplotly(p, tooltip = c("x", "y")) %>%
        config(displayModeBar = FALSE)
    }
  })
  
  # Sr87/86 Plot - showing only data after core start with click functionality
  output$sr8786_plot <- renderPlotly({
    if (!is.null(values$current_data) && !is.null(values$core_start)) {
      # Filter data to show only after core start
      filtered_data <- values$current_data %>% 
        filter(Microns >= values$core_start)
      
      # Create GAM model for the filtered data
      if (nrow(filtered_data) > 10) {
        n <- nrow(filtered_data)
        k <- floor(15 * (n^(2/9)))
        k <- min(k, 30)  # Cap k at 30
        model <- gam(Iso ~ s(Microns, bs="tp", k=k), gamma=0.9, data=filtered_data)
        
        predictions <- predict(model, se=TRUE)
        fit <- predictions$fit
        se <- predictions$se.fit
        
        plot_data <- filtered_data %>%
          mutate(
            gam_fit = fit,
            gam_lower = fit - 1.96 * se,
            gam_upper = fit + 1.96 * se
          )
      } else {
        plot_data <- filtered_data %>%
          mutate(
            gam_fit = Iso_MA,
            gam_lower = Iso_MA,
            gam_upper = Iso_MA
          )
      }
      
      p <- ggplot(plot_data, aes(x = Microns, y = Iso)) +
        geom_ribbon(aes(ymin = gam_lower, ymax = gam_upper), 
                    alpha = 0.3, fill = "gold") +
        geom_point(alpha = 0.4, size = 0.8, color = "gray30") +
        geom_line(aes(y = Iso_MA), color = "red", linewidth = 1, alpha = 0.8) +
        geom_line(aes(y = gam_fit), color = "black", linewidth = 1.2) +
        geom_vline(xintercept = values$selected_natal_start, 
                   color = "#007bff", linetype = "solid", linewidth = 2, alpha = 0.9) +
        geom_vline(xintercept = values$selected_natal_end, 
                   color = "#6f42c1", linetype = "solid", linewidth = 2, alpha = 0.9) +
        geom_vline(xintercept = values$selected_marine_start, 
                   color = "#dc3545", linetype = "solid", linewidth = 2, alpha = 0.9) +
        geom_hline(yintercept = 0.7092, color = "blue", alpha = 0.5) +
        labs(title = paste("Sr87/86 -", values$current_fish_id, "(Click to adjust landmarks)"), 
             x = "Microns", y = "87Sr/86Sr") +
        theme_minimal() +
        theme(plot.title = element_text(size = 12))
      
      ggplotly(p, tooltip = c("x", "y"), source = "sr8786_plot") %>%
        config(displayModeBar = FALSE)
    }
  })
  
  # Handle plotly clicks for landmark adjustment
  observeEvent(event_data("plotly_click", source = "sr8786_plot"), {
    click_data <- event_data("plotly_click", source = "sr8786_plot")
    
    if (!is.null(click_data) && values$selection_mode != "none") {
      clicked_x <- round(click_data$x, 1)
      
      if (values$selection_mode == "natal_start") {
        values$selected_natal_start <- clicked_x
        values$selection_mode <- "none"
        values$changes_made <- TRUE
        showNotification(paste("✓ Natal Start set to:", clicked_x, "μm"), type = "message", duration = 2)
        
      } else if (values$selection_mode == "natal_end") {
        values$selected_natal_end <- clicked_x
        values$selection_mode <- "none"
        values$changes_made <- TRUE
        showNotification(paste("✓ Natal End set to:", clicked_x, "μm"), type = "message", duration = 2)
        
      } else if (values$selection_mode == "marine_start") {
        values$selected_marine_start <- clicked_x
        values$selection_mode <- "none"
        values$changes_made <- TRUE
        showNotification(paste("✓ Marine Start set to:", clicked_x, "μm"), type = "message", duration = 2)
      }
    } else if (!is.null(click_data) && values$selection_mode == "none") {
      showNotification("⚠️ Please select a landmark type first (1, 2, or 3)", type = "warning", duration = 2)
    }
  }, ignoreInit = TRUE)
  
  # Landmark selection button handlers
  observeEvent(input$select_natal_start, {
    values$selection_mode <- "natal_start"
    showNotification("Click on plot to set Natal Start position", type = "message", duration = 3)
  })
  
  observeEvent(input$select_natal_end, {
    values$selection_mode <- "natal_end"
    showNotification("Click on plot to set Natal End position", type = "message", duration = 3)
  })
  
  observeEvent(input$select_marine_start, {
    values$selection_mode <- "marine_start"
    showNotification("Click on plot to set Marine Start position", type = "message", duration = 3)
  })
  
  # Hotkey handlers
  observeEvent(input$hotkey_natal_start, {
    values$selection_mode <- "natal_start"
    showNotification("Mode: Natal Start (1 key)", type = "message", duration = 1)
  })
  
  observeEvent(input$hotkey_natal_end, {
    values$selection_mode <- "natal_end"  
    showNotification("Mode: Natal End (2 key)", type = "message", duration = 1)
  })
  
  observeEvent(input$hotkey_marine_start, {
    values$selection_mode <- "marine_start"
    showNotification("Mode: Marine Start (3 key)", type = "message", duration = 1)
  })
  
  observeEvent(input$hotkey_delete, {
    # Show confirmation dialog for delete
    showModal(modalDialog(
      title = "⚠️ Confirm File Deletion",
      div(
        h4("Are you sure you want to DELETE this file?", style = "color: #dc3545;"),
        p(paste("Fish ID:", values$current_fish_id)),
        p("This will permanently remove the file from the AnalysisReady directory."),
        tags$strong("This action cannot be undone!", style = "color: #dc3545;")
      ),
      footer = tagList(
        actionButton("confirm_delete", "🗑️ Yes, Delete File", class = "btn btn-danger"),
        modalButton("Cancel")
      ),
      easyClose = FALSE
    ))
  })
  
  observeEvent(input$hotkey_save, {
    save_current_corrections()
  })
  
  observeEvent(input$hotkey_skip, {
    skip_current_sample()
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
  
  # Jump to specific fish handler
  observeEvent(input$jump_btn, {
    target_index <- as.numeric(input$jump_to_fish)
    if (!is.na(target_index) && target_index >= 1 && target_index <= length(remaining_files)) {
      if (values$changes_made) {
        # Show confirmation dialog if there are unsaved changes
        showModal(modalDialog(
          title = "Unsaved Changes",
          paste("You have unsaved changes. Do you want to save them before jumping to", remaining_ids[target_index], "?"),
          footer = tagList(
            actionButton("save_and_jump", "Save & Jump", class = "btn-success"),
            actionButton("discard_and_jump", "Discard & Jump", class = "btn-warning"),
            modalButton("Cancel")
          )
        ))
        # Store target for later use
        values$jump_target <- target_index
      } else {
        # No unsaved changes, jump directly
        values$current_index <- target_index
        showNotification(paste("Jumped to", remaining_ids[target_index]), type = "message", duration = 2)
      }
    }
  })
  
  # Save corrections function
  save_current_corrections <- function() {
    if (!is.null(values$current_fish_id)) {
      
      # Check if any changes were actually made
      natal_start_changed <- abs(values$selected_natal_start - values$original_natal_start) > 0.1
      natal_end_changed <- abs(values$selected_natal_end - values$original_natal_end) > 0.1
      marine_start_changed <- abs(values$selected_marine_start - values$original_marine_start) > 0.1
      
      any_changes <- natal_start_changed || natal_end_changed || marine_start_changed
      
      if (any_changes) {
        # Determine what changes were made
        changes_list <- c()
        if (natal_start_changed) changes_list <- c(changes_list, "natal_start")
        if (natal_end_changed) changes_list <- c(changes_list, "natal_end")
        if (marine_start_changed) changes_list <- c(changes_list, "marine_start")
        
        changes_text <- paste(changes_list, collapse = ", ")
        
        # Update the current data with new landmarks (keeping all original columns)
        updated_data <- values$current_data %>%
          mutate(
            natal_microns_start = values$selected_natal_start,
            natal_microns_end = values$selected_natal_end,
            marine_start = values$selected_marine_start
          )
        
        # Save directly back to the AnalysisReady directory (OVERWRITE original file)
        current_file_path <- remaining_files[values$current_index]
        write_csv(updated_data, current_file_path)
        
        # Update corrections log
        new_correction <- data.frame(
          Fish_ID = values$current_fish_id,
          Original_Natal_Start = values$original_natal_start,
          Original_Natal_End = values$original_natal_end,
          Original_Marine_Start = values$original_marine_start,
          Corrected_Natal_Start = values$selected_natal_start,
          Corrected_Natal_End = values$selected_natal_end,
          Corrected_Marine_Start = values$selected_marine_start,
          Changes_Made = changes_text,
          Review_Date = as.character(Sys.Date()),
          stringsAsFactors = FALSE
        )
        
        values$corrections_log <- bind_rows(values$corrections_log, new_correction)
        write.csv(values$corrections_log, CORRECTIONS_LOG_FILE, row.names = FALSE)
        
        showNotification(
          paste("✅ Updated", values$current_fish_id, "in AnalysisReady folder - Changes:", changes_text),
          type = "message",
          duration = 4
        )
      } else {
        # No changes made - just log as reviewed
        new_correction <- data.frame(
          Fish_ID = values$current_fish_id,
          Original_Natal_Start = values$original_natal_start,
          Original_Natal_End = values$original_natal_end,
          Original_Marine_Start = values$original_marine_start,
          Corrected_Natal_Start = values$original_natal_start,
          Corrected_Natal_End = values$original_natal_end,
          Corrected_Marine_Start = values$original_marine_start,
          Changes_Made = "reviewed_no_changes",
          Review_Date = as.character(Sys.Date()),
          stringsAsFactors = FALSE
        )
        
        values$corrections_log <- bind_rows(values$corrections_log, new_correction)
        write.csv(values$corrections_log, CORRECTIONS_LOG_FILE, row.names = FALSE)
        
        showNotification(
          paste("✓ Reviewed", values$current_fish_id, "- no changes needed"),
          type = "message",
          duration = 3
        )
      }
      
      values$changes_made <- FALSE
      
      # Move to next sample
      if (values$current_index < length(remaining_files)) {
        values$current_index <- values$current_index + 1
      }
    }
  }
  
  # Delete sample function
  delete_current_sample <- function() {
    if (!is.null(values$current_fish_id)) {
      current_file_path <- remaining_files[values$current_index]
      
      tryCatch({
        # Delete the file
        file.remove(current_file_path)
        
        # Log the deletion
        new_deletion <- data.frame(
          Fish_ID = values$current_fish_id,
          Original_Natal_Start = values$original_natal_start,
          Original_Natal_End = values$original_natal_end,
          Original_Marine_Start = values$original_marine_start,
          Corrected_Natal_Start = NA,
          Corrected_Natal_End = NA,
          Corrected_Marine_Start = NA,
          Changes_Made = "FILE_DELETED",
          Review_Date = as.character(Sys.Date()),
          stringsAsFactors = FALSE
        )
        
        values$corrections_log <- bind_rows(values$corrections_log, new_deletion)
        write.csv(values$corrections_log, CORRECTIONS_LOG_FILE, row.names = FALSE)
        
        values$changes_made <- FALSE
        
        showNotification(
          paste("🗑️ DELETED", values$current_fish_id, "from AnalysisReady directory"),
          type = "warning",
          duration = 5
        )
        
        # Move to next sample (or stay if at end)
        if (values$current_index < length(remaining_files)) {
          values$current_index <- values$current_index + 1
        } else if (values$current_index > 1) {
          values$current_index <- values$current_index - 1
        }
        
      }, error = function(e) {
        showNotification(
          paste("Error deleting file:", e$message),
          type = "error",
          duration = 10
        )
      })
    }
  }
  
  # Skip current sample function
  skip_current_sample <- function() {
    if (!is.null(values$current_fish_id)) {
      # Log as reviewed but no changes
      new_correction <- data.frame(
        Fish_ID = values$current_fish_id,
        Original_Natal_Start = values$original_natal_start,
        Original_Natal_End = values$original_natal_end,
        Original_Marine_Start = values$original_marine_start,
        Corrected_Natal_Start = values$original_natal_start,  # Keep original
        Corrected_Natal_End = values$original_natal_end,      # Keep original
        Corrected_Marine_Start = values$original_marine_start, # Keep original
        Changes_Made = "skipped",
        Review_Date = as.character(Sys.Date()),
        stringsAsFactors = FALSE
      )
      
      values$corrections_log <- bind_rows(values$corrections_log, new_correction)
      write.csv(values$corrections_log, CORRECTIONS_LOG_FILE, row.names = FALSE)
      
      values$changes_made <- FALSE
      
      showNotification(
        paste("⏭️ Skipped", values$current_fish_id, "- no changes made"),
        type = "message",
        duration = 3
      )
      
      # Move to next sample
      if (values$current_index < length(remaining_files)) {
        values$current_index <- values$current_index + 1
      }
    }
  }
  
  # Button handlers
  observeEvent(input$save_corrections, {
    save_current_corrections()
  })
  
  observeEvent(input$skip_sample, {
    skip_current_sample()
  })
  
  observeEvent(input$delete_sample, {
    # Show confirmation dialog for delete
    showModal(modalDialog(
      title = "⚠️ Confirm File Deletion",
      div(
        h4("Are you sure you want to DELETE this file?", style = "color: #dc3545;"),
        p(paste("Fish ID:", values$current_fish_id)),
        p("This will permanently remove the file from the AnalysisReady directory."),
        tags$strong("This action cannot be undone!", style = "color: #dc3545;")
      ),
      footer = tagList(
        actionButton("confirm_delete", "🗑️ Yes, Delete File", class = "btn btn-danger"),
        modalButton("Cancel")
      ),
      easyClose = FALSE
    ))
  })
  
  # Confirmation dialog handler
  observeEvent(input$confirm_delete, {
    delete_current_sample()
    removeModal()
  })
  
  # Navigation button handlers
  observeEvent(input$next_btn, {
    if (values$changes_made) {
      showModal(modalDialog(
        title = "Unsaved Changes",
        "You have unsaved changes. Do you want to save them before proceeding?",
        footer = tagList(
          actionButton("save_and_proceed", "Save & Continue", class = "btn-success"),
          actionButton("discard_and_proceed", "Discard & Continue", class = "btn-warning"),
          modalButton("Cancel")
        )
      ))
    } else if (values$current_index < length(remaining_files)) {
      values$current_index <- values$current_index + 1
    }
  })
  
  observeEvent(input$prev_btn, {
    if (values$changes_made) {
      showModal(modalDialog(
        title = "Unsaved Changes",
        "You have unsaved changes. Do you want to save them before proceeding?",
        footer = tagList(
          actionButton("save_and_go_back", "Save & Go Back", class = "btn-success"),
          actionButton("discard_and_go_back", "Discard & Go Back", class = "btn-warning"),
          modalButton("Cancel")
        )
      ))
    } else if (values$current_index > 1) {
      values$current_index <- values$current_index - 1
    }
  })
  
  # Modal dialog handlers
  observeEvent(input$save_and_proceed, {
    save_current_corrections()
    removeModal()
    if (values$current_index < length(remaining_files)) {
      values$current_index <- values$current_index + 1
    }
  })
  
  observeEvent(input$discard_and_proceed, {
    values$changes_made <- FALSE
    # Reset selected values to original
    values$selected_natal_start <- values$original_natal_start
    values$selected_natal_end <- values$original_natal_end
    values$selected_marine_start <- values$original_marine_start
    removeModal()
    if (values$current_index < length(remaining_files)) {
      values$current_index <- values$current_index + 1
    }
  })
  
  observeEvent(input$save_and_go_back, {
    save_current_corrections()
    removeModal()
    if (values$current_index > 1) {
      values$current_index <- values$current_index - 1
    }
  })
  
  observeEvent(input$discard_and_go_back, {
    values$changes_made <- FALSE
    # Reset selected values to original
    values$selected_natal_start <- values$original_natal_start
    values$selected_natal_end <- values$original_natal_end
    values$selected_marine_start <- values$original_marine_start
    removeModal()
    if (values$current_index > 1) {
      values$current_index <- values$current_index - 1
    }
  })
  
  # Jump dialog handlers
  observeEvent(input$save_and_jump, {
    save_current_corrections()
    removeModal()
    if (!is.null(values$jump_target)) {
      values$current_index <- values$jump_target
      showNotification(paste("Jumped to", remaining_ids[values$jump_target]), type = "message", duration = 2)
      values$jump_target <- NULL
    }
  })
  
  observeEvent(input$discard_and_jump, {
    values$changes_made <- FALSE
    # Reset selected values to original
    values$selected_natal_start <- values$original_natal_start
    values$selected_natal_end <- values$original_natal_end
    values$selected_marine_start <- values$original_marine_start
    removeModal()
    if (!is.null(values$jump_target)) {
      values$current_index <- values$jump_target
      showNotification(paste("Jumped to", remaining_ids[values$jump_target]), type = "message", duration = 2)
      values$jump_target <- NULL
    }
  })
  
  # Log tab outputs
  output$total_reviewed <- renderValueBox({
    valueBox(
      value = nrow(values$corrections_log),
      subtitle = "Total Reviewed",
      icon = icon("check"),
      color = "blue"
    )
  })
  
  output$corrections_made <- renderValueBox({
    valueBox(
      value = sum(values$corrections_log$Changes_Made != "skipped" & 
                    values$corrections_log$Changes_Made != "reviewed_no_changes" &
                    values$corrections_log$Changes_Made != "FILE_DELETED", na.rm = TRUE),
      subtitle = "Files Modified",
      icon = icon("edit"),
      color = "green"
    )
  })
  
  output$remaining_samples <- renderValueBox({
    deleted_count <- sum(values$corrections_log$Changes_Made == "FILE_DELETED", na.rm = TRUE)
    valueBox(
      value = length(remaining_files) - nrow(values$corrections_log),
      subtitle = paste("Remaining", ifelse(deleted_count > 0, paste("(", deleted_count, "deleted)"), "")),
      icon = icon("clock"),
      color = "yellow"
    )
  })
  
  output$corrections_table <- DT::renderDataTable({
    corrections_display <- values$corrections_log %>%
      mutate(
        Natal_Start_Change = round(Corrected_Natal_Start - Original_Natal_Start, 1),
        Natal_End_Change = round(Corrected_Natal_End - Original_Natal_End, 1),
        Marine_Start_Change = round(Corrected_Marine_Start - Original_Marine_Start, 1)
      ) %>%
      select(Fish_ID, Changes_Made, Natal_Start_Change, Natal_End_Change, Marine_Start_Change, Review_Date)
    
    DT::datatable(
      corrections_display, 
      options = list(
        pageLength = 15, 
        scrollX = TRUE,
        order = list(list(5, 'desc'))  # Sort by Review_Date descending
      ),
      colnames = c("Fish ID", "Changes Made", "Natal Start Δ", "Natal End Δ", "Marine Start Δ", "Review Date")
    ) %>%
      DT::formatStyle(
        columns = c("Natal_Start_Change", "Natal_End_Change", "Marine_Start_Change"),
        backgroundColor = DT::styleInterval(c(-0.1, 0.1), c("#ffcccc", "white", "#ccffcc")),
        fontWeight = DT::styleInterval(c(-0.1, 0.1), c("bold", "normal", "bold"))
      ) %>%
      DT::formatStyle(
        columns = "Changes_Made",
        backgroundColor = DT::styleEqual("FILE_DELETED", "#ffebee"),
        color = DT::styleEqual("FILE_DELETED", "#d32f2f"),
        fontWeight = DT::styleEqual("FILE_DELETED", "bold")
      )
  })
  
  output$download_log <- downloadHandler(
    filename = function() {
      paste("landmark_corrections_log_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(values$corrections_log, file, row.names = FALSE)
    }
  )
}

# =============================================================================
# RUN APP
# =============================================================================

shinyApp(ui = ui, server = server)