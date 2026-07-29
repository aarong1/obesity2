# ============================================================================
# INTERVENTION MODULE - Example Usage
# ============================================================================
# Demonstration of how to use the intervention timeline module

library(shiny)
source('modules/intervention_module/intervention_module.R')

# ============================================================================
# EXAMPLE 1: Basic Usage
# ============================================================================

example_basic_app <- function() {
  
  ui <- fluidPage(
    titlePanel("Intervention Module - Basic Example"),
    
    # Basic intervention module
    intervention_module_ui(
      id = "basic_intervention",
      chart_height = "300px"
    ),
    
    hr(),
    
    # Display results
    fluidRow(
      column(6,
        h4("Chart Status"),
        verbatimTextOutput("status")
      ),
      column(6,
        h4("Current Data"),
        tableOutput("data_table")
      )
    )
  )
  
  server <- function(input, output, session) {
    # Initialize module
    result <- intervention_module_server("basic_intervention")
    
    # Show status
    output$status <- renderText({
      paste(
        "Modified:", result$is_modified(),
        "\nInteractions:", result$interaction_count(),
        "\nData points:", length(result$chart_data())
      )
    })
    
    # Show data as table
    output$data_table <- renderTable({
      intervention_data_to_df(result$chart_data())
    })
  }
  
  shinyApp(ui = ui, server = server)
}

# ============================================================================
# EXAMPLE 2: Advanced Usage with Custom Data
# ============================================================================

example_advanced_app <- function() {
  
  # Custom intervention scenario data
  intervention_scenario <- list(
    c(2020, 1.00), c(2021, 1.05), c(2022, 1.10), c(2023, 1.15),
    c(2024, 1.25), c(2025, 1.30), c(2026, 1.28), c(2027, 1.25),
    c(2028, 1.20), c(2029, 1.15), c(2030, 1.10)
  )
  
  ui <- fluidPage(
    titlePanel("Intervention Module - Advanced Example"),
    
    fluidRow(
      column(8,
        # Advanced intervention module with all features
        intervention_module_ui(
          id = "advanced_intervention",
          chart_height = "350px",
          show_controls = TRUE,
          enable_canvas_drag = TRUE,
          initial_data = intervention_scenario
        )
      ),
      column(4,
        wellPanel(
          h4("Module Controls"),
          
          # External reset button
          actionButton("reset_chart", "Reset Chart", 
                      class = "btn-warning", width = "100%"),
          br(), br(),
          
          # Load predefined scenarios
          selectInput("scenario", "Load Scenario:",
            choices = list(
              "Custom" = "custom",
              "Gradual Increase" = "gradual", 
              "Sharp Intervention" = "sharp",
              "Declining Effect" = "decline"
            )
          ),
          
          actionButton("load_scenario", "Load Selected", 
                      class = "btn-primary", width = "100%")
        ),
        
        wellPanel(
          h4("Export Options"),
          downloadButton("download_data", "Download CSV", 
                        class = "btn-success", width = "100%"),
          br(), br(),
          downloadButton("download_plot", "Download Chart PNG",
                        class = "btn-info", width = "100%")
        )
      )
    ),
    
    hr(),
    
    # Results display
    tabsetPanel(
      tabPanel("Data Summary",
        fluidRow(
          column(6,
            h4("Intervention Effects"),
            tableOutput("intervention_table")
          ),
          column(6, 
            h4("Statistics"),
            verbatimTextOutput("statistics")
          )
        )
      ),
      tabPanel("Raw Data",
        verbatimTextOutput("raw_data")
      ),
      tabPanel("Debug Info",
        verbatimTextOutput("debug_info")
      )
    )
  )
  
  server <- function(input, output, session) {
    
    # Initialize intervention module  
    result <- intervention_module_server(
      id = "advanced_intervention",
      initial_data = intervention_scenario,
      enable_canvas_drag = TRUE
    )
    
    # Predefined scenarios
    scenarios <- list(
      gradual = list(
        c(2020, 1.0), c(2021, 1.1), c(2022, 1.2), c(2023, 1.3),
        c(2024, 1.4), c(2025, 1.5), c(2026, 1.6), c(2027, 1.7),
        c(2028, 1.8), c(2029, 1.9), c(2030, 2.0)
      ),
      sharp = list(
        c(2020, 1.0), c(2021, 1.0), c(2022, 1.0), c(2023, 2.0),
        c(2024, 2.0), c(2025, 2.0), c(2026, 2.0), c(2027, 2.0),
        c(2028, 2.0), c(2029, 2.0), c(2030, 2.0)
      ),
      decline = list(
        c(2020, 1.0), c(2021, 1.0), c(2022, 1.8), c(2023, 1.6),
        c(2024, 1.4), c(2025, 1.3), c(2026, 1.2), c(2027, 1.15),
        c(2028, 1.1), c(2029, 1.05), c(2030, 1.0)
      )
    )
    
    # Load scenario
    observeEvent(input$load_scenario, {
      if (input$scenario %in% names(scenarios)) {
        result$update_data(scenarios[[input$scenario]])
        showNotification("Scenario loaded successfully!", type = "message")
      }
    })
    
    # Reset chart
    observeEvent(input$reset_chart, {
      result$reset()
      showNotification("Chart reset to initial state", type = "message")
    })
    
    # Display intervention effects table
    output$intervention_table <- renderTable({
      df <- intervention_data_to_df(result$chart_data())
      df$effect_size <- df$value - 1.0
      df$percent_change <- (df$value - 1.0) * 100
      df[, c("year", "value", "effect_size", "percent_change")]
    }, digits = 3)
    
    # Display statistics
    output$statistics <- renderText({
      df <- intervention_data_to_df(result$chart_data())
      
      paste(
        "Data Points:", nrow(df),
        "\nYear Range:", min(df$year), "-", max(df$year),
        "\nValue Range:", round(min(df$value), 3), "-", round(max(df$value), 3),
        "\nMean Effect:", round(mean(df$value), 3),
        "\nMax Intervention:", round(max(df$value) - 1, 3),
        "\nTotal Interactions:", result$interaction_count(),
        "\nIs Modified:", result$is_modified()
      )
    })
    
    # Show raw data
    output$raw_data <- renderPrint({
      result$chart_data()
    })
    
    # Debug information
    output$debug_info <- renderText({
      last_int <- result$last_interaction()
      
      debug_text <- paste(
        "Module Status:",
        "\n- Chart Data Length:", length(result$chart_data()),
        "\n- Interaction Count:", result$interaction_count(),
        "\n- Is Modified:", result$is_modified(),
        "\n- Show Line Series:", result$inputs$show_line(),
        "\n- Change Subsequent:", result$inputs$change_subsequent(),
        "\n- Taper Subsequent:", result$inputs$taper_subsequent()
      )
      
      if (!is.null(last_int)) {
        debug_text <- paste(debug_text,
          "\n\nLast Interaction:",
          "\n- Data Index:", last_int$dataIndex,
          "\n- Timestamp:", Sys.time()
        )
      }
      
      debug_text
    })
    
    # Download handlers
    output$download_data <- downloadHandler(
      filename = function() {
        paste0("intervention_data_", Sys.Date(), ".csv")
      },
      content = function(file) {
        df <- intervention_data_to_df(result$chart_data())
        write.csv(df, file, row.names = FALSE)
      }
    )
    
    output$download_plot <- downloadHandler(
      filename = function() {
        paste0("intervention_chart_", Sys.Date(), ".png")
      },
      content = function(file) {
        # This would require additional setup to export the ECharts plot
        # For now, just save the data as a simple plot
        df <- intervention_data_to_df(result$chart_data())
        
        png(file, width = 800, height = 400)
        plot(df$year, df$value, type = "b", 
             main = "Intervention Timeline",
             xlab = "Year", ylab = "Intervention Effect",
             pch = 16, col = "blue")
        abline(h = 1, col = "gray", lty = 2)
        dev.off()
      }
    )
  }
  
  shinyApp(ui = ui, server = server)
}

# ============================================================================
# EXAMPLE 3: Multiple Modules in One App
# ============================================================================

example_multiple_modules <- function() {
  
  ui <- fluidPage(
    titlePanel("Multiple Intervention Modules"),
    
    fluidRow(
      column(6,
        h3("Scenario A: Conservative"),
        intervention_module_ui("scenario_a", chart_height = "250px")
      ),
      column(6,
        h3("Scenario B: Aggressive"), 
        intervention_module_ui("scenario_b", chart_height = "250px")
      )
    ),
    
    hr(),
    
    fluidRow(
      column(12,
        h3("Comparison"),
        plotOutput("comparison_plot")
      )
    )
  )
  
  server <- function(input, output, session) {
    
    # Initialize both modules with different data
    result_a <- intervention_module_server("scenario_a")
    result_b <- intervention_module_server("scenario_b") 
    
    # Comparison plot
    output$comparison_plot <- renderPlot({
      df_a <- intervention_data_to_df(result_a$chart_data())
      df_b <- intervention_data_to_df(result_b$chart_data())
      
      plot(df_a$year, df_a$value, type = "l", col = "blue", 
           ylim = range(c(df_a$value, df_b$value)),
           xlab = "Year", ylab = "Intervention Effect",
           main = "Scenario Comparison")
      lines(df_b$year, df_b$value, col = "red")
      legend("topright", legend = c("Scenario A", "Scenario B"), 
             col = c("blue", "red"), lty = 1)
      abline(h = 1, col = "gray", lty = 2)
    })
  }
  
  shinyApp(ui = ui, server = server)
}

# ============================================================================
# RUN EXAMPLES
# ============================================================================

# Uncomment to run examples:

# Basic example
# example_basic_app()

# Advanced example  
# example_advanced_app()

# Multiple modules example
# example_multiple_modules()

cat("Intervention Module Examples Ready!\n")
cat("Uncomment the desired example function call to run.\n")