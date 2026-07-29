# Last Run Time Display Module
# Shows "run: x mins ago" format for tracking action timestamps

#' Last Run Time Module UI
#' @param id Shiny module id
#' @param label Optional label to show before the time (default: "run:")
#' @return UI element displaying last run time
last_run_ui <- function(id, label = "run:") {
  ns <- shiny::NS(id)
  
  shiny::tags$span(
    class = "text-muted last-run-display",
    style = "font-size: 11px; color: #666; font-style: light; margin: 5px 5px;;",
    shiny::textOutput(ns("last_run_text"), inline = TRUE)
  )
}

#' Last Run Time Module Server
#' @param id Shiny module id
#' @param trigger Reactive expression that triggers when action occurs
#' @param label Optional label to show before the time (default: "run:")
#' @return List with update_time() function to manually trigger updates
last_run_server <- function(id, trigger = NULL, label = "run:") {
  shiny::moduleServer(id, function(input, output, session) {
    
    # Store the last run time
    last_run_time <- shiny::reactiveVal(NULL)
    
    # Auto-update timer (every 30 seconds to refresh "x mins ago")
    shiny::observe({
      shiny::invalidateLater(30000)  # 30 seconds
      # This will trigger the output to re-render and update the time display
      output$last_run_text <- shiny::renderText({
        format_time_ago(last_run_time())
      })
    })
    
    # Watch for trigger changes
    if (!is.null(trigger)) {
      shiny::observe({
        print('trigger')
        trigger()  # Execute trigger to establish dependency
        last_run_time(Sys.time())
      })
    }
    
    # Format time difference into human-readable format
    format_time_ago <- function(time) {
      if (is.null(time)) return(paste(label, "never"))
      
      diff <- as.numeric(difftime(Sys.time(), time, units = "secs"))
      
      if (diff < 60) {
        return(paste(label, "just now"))
      } else if (diff < 3600) {  # Less than 1 hour
        mins <- floor(diff / 60)
        return(paste(label, paste0(mins, " min", if(mins != 1) "s", " ago")))
      } else if (diff < 86400) {  # Less than 1 day
        hours <- floor(diff / 3600)
        return(paste(label, paste0(hours, " hour", if(hours != 1) "s", " ago")))
      } else {  # 1 day or more
        days <- floor(diff / 86400)
        return(paste(label, paste0(days, " day", if(days != 1) "s", " ago")))
      }
    }
    
    # Output the formatted time
    output$last_run_text <- shiny::renderText({
      format_time_ago(last_run_time())
    })
    
    # Return functions for manual control
    return(list(
      update_time = function() {
        last_run_time(Sys.time())
      },
      get_last_run = function() {
        last_run_time()
      }
    ))
  })
}

# Example usage function
if (interactive()) {
  demo_last_run <- function() {
    ui <- shiny::fluidPage(
      shiny::titlePanel("Last Run Time Demo"),
      
      shiny::fluidRow(
        shiny::column(6,
          shiny::h4("Auto-triggered (every button click)"),
          shiny::actionButton("trigger_btn", "Run Action"),
          last_run_ui("auto_run", "Auto run:")
        ),
        shiny::column(6,
          shiny::h4("Manual control"),
          shiny::actionButton("manual_btn", "Manual Update"),
          last_run_ui("manual_run", "Manual run:")
        )
      ),
      
      shiny::br(),
      shiny::p("The auto-triggered version updates whenever you click the button.",
              "The manual version only updates when you explicitly call update_time()."),
      shiny::p("Both displays auto-refresh every 30 seconds to update the 'x mins ago' text.")
    )
    
    server <- function(input, output, session) {
      # Auto-triggered version
      last_run_server("auto_run", 
                     trigger = shiny::reactive(input$trigger_btn),
                     label = "Auto run:")
      
      # Manual control version
      manual_controller <- last_run_server("manual_run", label = "Manual run:")
      
      shiny::observeEvent(input$manual_btn, {
        manual_controller$update_time()
      })
    }
    
    shiny::shinyApp(ui, server)
  }
  
  # Uncomment to run demo
  # demo_last_run()
}