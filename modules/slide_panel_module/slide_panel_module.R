#' Slide Panel Module - UI
#' 
#' Creates a sliding panel from the right side with customizable content
#' 
#' @param id Module namespace ID
#' @param panel_title Title for the panel header
#' @param panel_subtitle Subtitle/description text
#' @param editor_height Height of the editor area (default: "80%")
#' @param panel_width Width of the panel in pixels (default: 300)
#' @param include_quill Whether to include Quill editor dependencies (default: TRUE)
#' @param include_custom_js Whether to include custom JavaScript files (default: FALSE)
#' @param custom_js_files Vector of custom JavaScript file paths (optional)
#' 
#' @return UI elements for the slide panel
slide_panel_ui <- function(id, 
                          panel_title = "Collect Graphics from your Report",
                          panel_subtitle = "Plot Repository", 
                          editor_height = "80%",
                          panel_width = 300) {
  
  addResourcePath(prefix = 'spm',
                  directoryPath =  './modules/slide_panel_module/' )
  
  print(resourcePaths())
  
  
  ns <- shiny::NS(id)
  
  # Generate CSS with dynamic panel width
  panel_css <- sprintf("
    /* Panel styles */
    #%s {
      position: fixed;
      top: 25px;
      right: -%dpx; /* Start offscreen */
      width: %dpx;
      height: 100%%;
      background-color: #f8f9fa;
      box-shadow: -2px 0 5px rgba(0,0,0,0.3);
      transition: right 0.3s ease;
      padding: 20px;
      z-index: 1000;
    }

    /* Show the panel */
    #%s.open {
      right: 0;
    }

    #%s {
      position: fixed;
      top: 20px;
      right: 20px;
      z-index: 1100;
    }
  ", ns("slidePanel"), panel_width, panel_width, ns("slidePanel"), ns("toggleBtn"))
  
  # Build dependencies list
  dependencies <- list()
  
  # Add Quill dependencies if requested

    dependencies <- append(dependencies, list(
      shiny::tags$script(src = "https://cdn.jsdelivr.net/npm/quill@2.0.3/dist/quill.min.js"),
      shiny::tags$link(href = "https://cdn.jsdelivr.net/npm/quill@2.0.3/dist/quill.bubble.min.css", rel = "stylesheet")
    ))

  

  
  # Generate JavaScript for panel functionality
  panel_js <- sprintf("
    document.addEventListener('DOMContentLoaded', function(){
      document.getElementById('%s').onclick = function() {
        document.getElementById('%s').classList.toggle('open');
      };
      document.getElementById('%s').onclick = function() {
        document.getElementById('%s').classList.remove('open');
      };
    });
  ", ns("toggle_open"), ns("slidePanel"), ns("toggle_close"), ns("slidePanel"))
  
  shiny::tagList(
        tags$head(
      tags$script(src = "spm/app.js"),
      tags$script(src = "spm/clipboard.js"),
      tags$script(src = "spm/save_quill_contents.js"),
      
      HTML('<script src="
https://cdn.jsdelivr.net/npm/quill@2.0.3/dist/quill.min.js  
"></script>
<link href="
https://cdn.jsdelivr.net/npm/quill@2.0.3/dist/quill.bubble.min.css
" rel="stylesheet">')
    ),
    # CSS Styles
    shiny::tags$head(
      shiny::tags$style(shiny::HTML(panel_css))
    ),
    
    # Dependencies
    if (length(dependencies) > 0) shiny::tags$head(dependencies),
    
    # Toggle Open Button (fixed position)
    shiny::actionButton(
      inputId = ns("toggle_open"),
      label = NULL,
      icon = shiny::icon(style = 'transform: rotate(180deg)','right-from-bracket'), 
      class = "btn-outline-info", 
      style = 'position:fixed;bottom:15px;right:15px;'
    ),
    
    # Slide Panel
    shiny::tags$div(
      id = ns("slidePanel"),
      shiny::tags$nav(
        class = "navbar navbar-light bg-light mb-3 p-2 rounded",
        shiny::h4(panel_title, class = "navbar-brand mb-0")
      ),
      
      shiny::p(
        class = 'text-muted fs-5', 
        shiny::icon('indent', class = 'fs-6 ml-4',
          `data-bs-container` = "body",
          `data-bs-toggle` = "tooltip",
          `data-bs-placement` = "left",
          `data-bs-content` = "You can collate the visuals you see on the dashboard on this page",
          `data-bs-original-title` = "How do you use this?"
        ),
        panel_subtitle
      ),
      
      # Editor Area
      shiny::div(
        id = ns('editor'), 
        class = 'shadow-sm',
        style = sprintf('height:%s;', editor_height),
        shiny::br(),
        shiny::p(class = 'text-muted', 'Hello !'),
        shiny::p(class = 'text-muted', 'Drag and drop plots here (Edit me!) '),
        shiny::tags$br()
      ),
      
      # Close Button
      shiny::actionButton(
        inputId = ns("toggle_close"),
        label = NULL,
        icon = shiny::icon('right-from-bracket'), 
        class = "btn-info", 
        style = 'position:absolute;bottom:20px;right:15px;'
      ),
      
      # Action Buttons Group
      shiny::div(
        class = "btn-group", 
        role = "group", 
        `aria-label` = "Basic example",
        style = 'position:absolute;bottom:20px;left:15px;',
        
        shiny::actionButton(
          inputId = ns("save"),
          label = 'Save',
          icon = shiny::icon('floppy-disk'), 
          class = "btn-info"
        ),
        
        shiny::actionButton(
          inputId = ns("copy_paste"),
          label = 'Copy',
          icon = shiny::icon('copy'), 
          class = "btn-info"
        )
      )
    ),
    
    # JavaScript for panel functionality
    shiny::tags$script(shiny::HTML(panel_js))
  )
}

#' Slide Panel Module - Server
#' 
#' Server logic for the slide panel module
#' 
#' @param id Module namespace ID
#' @param save_handler Optional function to handle save button clicks
#' @param copy_handler Optional function to handle copy button clicks
#' 
#' @return List of reactive values and functions
slide_panel_server <- function(id, save_handler = NULL, copy_handler = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    
    # Track panel state
    panel_open <- shiny::reactiveVal(FALSE)
    
    # Handle panel toggle
    shiny::observeEvent(input$toggle_open, {
      panel_open(TRUE)
    })
    
    shiny::observeEvent(input$toggle_close, {
      panel_open(FALSE)
    })
    
    # Handle save button
    shiny::observeEvent(input$save, {
      if (!is.null(save_handler)) {
        save_handler()
      } else {
        shiny::showNotification("Save clicked", type = "message")
      }
    })
    
    # Handle copy button  
    shiny::observeEvent(input$copy_paste, {
      if (!is.null(copy_handler)) {
        copy_handler()
      } else {
        shiny::showNotification("Copy clicked", type = "message")
      }
    })
    
    # Return list of reactive values and functions
    list(
      panel_open = panel_open,
      is_open = shiny::reactive(panel_open())
    )
  })
}

#' Create Slide Panel Dependencies
#' 
#' Helper function to create HTML dependencies for custom JavaScript files
#' 
#' @param js_files Vector of JavaScript file paths relative to www directory
#' @param name Dependency name (default: "slide-panel-deps")
#' @param version Dependency version (default: "1.0.0")
#' 
#' @return HTML dependency object
slide_panel_deps <- function(js_files, name = "slide-panel-deps", version = "1.0.0") {
  htmltools::htmlDependency(
    name = name,
    version = version,
    src = c(file = "www"),
    script = js_files
  )
}

#' Simple Slide Panel - Minimal Version
#' 
#' A simplified version of the slide panel with minimal dependencies
#' 
#' @param id Module namespace ID  
#' @param panel_title Title for the panel
#' @param content_ui UI elements to include in the panel content area
#' @param panel_width Width of panel in pixels (default: 300)
#' 
#' @return UI elements for simple slide panel
simple_slide_panel_ui <- function(id, panel_title = "Panel", content_ui = NULL, panel_width = 300) {
  
  ns <- shiny::NS(id)
  
  panel_css <- sprintf("
    #%s {
      position: fixed;
      top: 0;
      right: -%dpx;
      width: %dpx;
      height: 100%%;
      background-color: #ffffff;
      border-left: 1px solid #dee2e6;
      box-shadow: -2px 0 10px rgba(0,0,0,0.1);
      transition: right 0.3s ease;
      padding: 20px;
      z-index: 1000;
      overflow-y: auto;
    }
    
    #%s.open {
      right: 0;
    }
    
    .panel-toggle-btn {
      position: fixed;
      bottom: 20px;
      right: 20px;
      z-index: 1100;
    }
  ", ns("panel"), panel_width, panel_width, ns("panel"))
  
  panel_js <- sprintf("
    $(document).ready(function() {
      $('#%s').click(function() {
        $('#%s').toggleClass('open');
      });
      
      $('#%s').click(function() {
        $('#%s').removeClass('open');
      });
    });
  ", ns("toggle"), ns("panel"), ns("close"), ns("panel"))
  
  shiny::tagList(
    shiny::tags$head(
      shiny::tags$style(shiny::HTML(panel_css)),
      shiny::tags$script(shiny::HTML(panel_js))
    ),
    
    shiny::actionButton(
      inputId = ns("toggle"),
      label = "Open Panel", 
      icon = shiny::icon("bars"),
      class = "btn btn-primary panel-toggle-btn"
    ),
    
    shiny::tags$div(
      id = ns("panel"),
      shiny::div(
        style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 20px;",
        shiny::h4(panel_title, style = "margin: 0;"),
        shiny::actionButton(
          inputId = ns("close"),
          label = NULL,
          icon = shiny::icon("times"),
          class = "btn btn-sm btn-outline-secondary",
          style = "margin-left: auto;"
        )
      ),
      
      shiny::hr(),
      
      if (!is.null(content_ui)) content_ui else shiny::p("Panel content goes here...")
    )
  )
}
