# ============================================================================
# COUNTUP.JS SHINY MODULE
# ============================================================================
      
      # LABEL DISPLAY - Dynamic text above counterodule id
#' @param startVal Initial number to display (default: 0)
#' @param endVal End value for animation (default: 500)
#' @param auto_start Legacy parameter for auto-start (default: FALSE)
#' @param auto_run_on_startup Run countup to endVal automatically on startup (default: FALSE)
#' @param enable_scrollspy Enable scrollspy to trigger animation when element comes into view (default: FALSE)
#' @param duration Animation duration in seconds (default: 2)
#' @param decimal_places Number of decimal places (default: 0)
#' @param separator Thousands separator (default: ",")
#' @param decimal Decimal separator (default: ".")
#' @param prefix Text before the number (default: "")
#' @param suffix Text after the number (default: "")
#' @param container_class CSS class for the container div
#' @param number_class CSS class for the number display
#' @return UI element with CountUp display and controls
#' 
countup_ui <- function(id, 
                        startVal = 0,
                       # endVal = 500,
                       # auto_start = F,
                       # auto_run_on_startup = FALSE,
                       # enable_scrollspy = FALSE,
                       # duration = 2,
                       # decimal_places = 0,
                       # separator = ",",
                       # decimal = ".",
                       # prefix = "",
                       # suffix = "",
                       container_class = "countup-container",
                       number_class = "countup-number") {
  ns <- shiny::NS(id)
  
  # --------------------------------------------------------------------------
  # DEPENDENCY MANAGEMENT - Loads external JavaScript library via CDN
  # --------------------------------------------------------------------------
  countup_dep <- htmltools::htmlDependency(
    name = "countup",
    version = "2.8.0",
    src = list(href = "https://cdn.jsdelivr.net/npm/countup.js@2.8.0/dist/"),
    script = "countUp.umd.js"
  )
  
  # --------------------------------------------------------------------------
  # CSS STYLING - Visual appearance and layout
  # --------------------------------------------------------------------------
  countup_css <- htmltools::tags$style(htmltools::HTML(paste0("
    .", container_class, " {
      text-align: center;
      margin: 10px 0;
    }
    
    .", number_class, " {
      font-size: 2.5em;
      font-weight: bold;
      color: #2c3e50;
      font-family: 'Arial', monospace;
      margin: 10px 0;
      display: inline-block;
      min-width: 100px;
    }
    
    .countup-controls {
      margin: 10px 0;
    }
    
    .countup-controls .btn {
      margin: 2px;
    }
    
    .countup-label {
      font-size: 0.9em;
      color: #666;
      margin-bottom: 5px;
    }
  ")))
  
  # --------------------------------------------------------------------------
  # JAVASCRIPT HANDLERS - Client-side animation control and Shiny messaging
  # --------------------------------------------------------------------------
  # Creates global functions and message handlers for:
  # - initCountUp()    - Initialize CountUp.js instance
  # - startCountUp()   - Start/update animation
  # - pauseCountUp()   - Pause/resume animation  
  # - resetCountUp()   - Reset to initial value
  # - Shiny message handlers for server communication
  
  countup_js <- htmltools::tags$script(htmltools::HTML(paste0('
 document.addEventListener("DOMContentLoaded", (event) => {
 
  window.countUpInstances = window.countUpInstances || {};
  
  function initCountUp(id, options) {
    var element = document.getElementById(id + "-number");
    if (!element) return;
    console.log("init countup 2 ");
    endVal = options.endVal || 50000;
        console.log(options);

    console.log(options);

    console.log(options);
    var countUpInst = new countUp.CountUp(id + "-number", endVal,options= {
      startVal: options.startVal,
      decimalPlaces: options.decimals,
      duration: options.duration,
      separator: options.separator || ",",
      decimal: options.decimal || ".",
      prefix: options.prefix || "",
      suffix: options.suffix || ""
    });
    
    console.log(countUpInst);
    window.countUpInstances[id] = countUpInst;
    window.countUpInst = countUpInst;
    return countUpInst;
  }
  
   function startCountUp(id) {
   console.log("start");
    var countUpInst = window.countUpInstances[id];

    if (countUpInst) {
      console.log("start2");
      console.log(countUpInst.endVal);
      console.log(countUpInst.options.endVal);
      countUpInst.start(300);
    }
  } 
  
  function updateCountUp(id, updateVal) {
    var countUpInst = window.countUpInstances[id];
    if (countUpInst) {
      countUpInst.update(updateVal);
    }
  } 
  
  function pauseCountUp(id) {
    var countUpInst = window.countUpInstances[id];
    if (countUpInst) {
      countUpInst.pauseResume();
    }
  }
  
  function resetCountUp(id,) {
    var countUpInst = window.countUpInstances[id];
    if (countUpInst) {
      countUpInst.reset();

    }
  }
  
  // Auto-run on startup if enabled
  function autoRunCountUp(id, endVal, options) {
    setTimeout(function() {
      var countUpInst = window.countUpInstances[id];
      if (countUp && endVal !== undefined) {
        countUpInst.start();
      }
    }, 100);
  }
  
  // Scrollspy functionality
  function setupScrollSpy(id) {
    var element = document.getElementById(id + "-number");
    if (!element) return;
    
    var observer = new IntersectionObserver(function(entries) {
      entries.forEach(function(entry) {
        if (entry.isIntersecting) {
          var countUpInst = window.countUpInstances[id];
          if (countUpInst) {
            countUp.start();
          }
        }
      });
    }, { threshold: 0.5 });
    
    observer.observe(element);
  }
  
  // Message handlers for Shiny
  Shiny.addCustomMessageHandler("countup-init", function(data) {
    console.log(data);
    
    initCountUp(data.id, data)//data.startVal, data.endVal, data.decimals, data.duration, data.options);
    
    // Auto-run on startup if enabled
    if (data.autoRunOnStartup) {
      autoRunCountUp(data.id, data.endVal, data.options);
    }
    
    // Setup scrollspy if enabled
    if (data.enableScrollspy) {
      setupScrollSpy(data.id);
    }
  });
  
  Shiny.addCustomMessageHandler("countup-start", function(data) {
    startCountUp(data.id);
  });
  
  Shiny.addCustomMessageHandler("countup-update", function(data) {
    updateCountUp(data.id, data.updateVal);
  });
  
  Shiny.addCustomMessageHandler("countup-pauseResume", function(data) {
    //pauseResumeCountUp(data.id);
    var countUp = window.countUpInstances[data.id];
    countUpInst

  });
  
  Shiny.addCustomMessageHandler("countup-pause", function(data) {
    pauseCountUp(data.id);
  });
  
  Shiny.addCustomMessageHandler("countup-reset", function(data) {
    resetCountUp(data.id);
  });
  });
  ')))
  
  htmltools::tagList(
    countup_dep,
    countup_css,
    countup_js,
    
    htmltools::div(
      class = container_class,
      
      # Label
      htmltools::div(
        class = "countup-label",
        shiny::textOutput(ns("label"), inline = TRUE)
      ),
      
      # NUMBER DISPLAY - Main animated counter element
      htmltools::div(
        id = ns("number"),
        class = number_class,
        startVal
      )#,
      
      
      # CONFIGURATION STORAGE - Hidden input storing initial settings
      # shiny::tags$input(id = ns("config"), type = "hidden", value = jsonlite::toJSON(list(
      #   startVal = startVal,
      #   endVal = endVal,
      #   auto_run_on_startup = auto_run_on_startup,
      #   enable_scrollspy = enable_scrollspy,
      #   duration = duration,
      #   decimal_places = decimal_places,
      #   separator = separator,
      #   decimal = decimal,
      #   prefix = prefix,
      #   suffix = suffix
      # )))
    )
   )
}

#' CountUp Module Server
#' @param id Shiny module id
#' @param label Reactive or static text for the label above the counter
#' @param auto_start Logical, whether to start animation automatically (default: FALSE)
#' @return List with control methods: start(), pause(), reset(), update()
#' 
countup_server <- function(id, label = "Counter", endVal = 5, auto_start = FALSE) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # The module server arguments are available here:
    # - label (accessible as just 'label')
    # - endVal (accessible as just 'endVal') 
    # - auto_start (accessible as just 'auto_start')
    
    print(paste("endVal argument:", endVal))
    print(paste("label argument:", label))
    print(paste("auto_start argument:", auto_start))
    
    # Reactive values
    current_value <- shiny::reactiveVal(endVal)  # Now using the endVal argument
    is_initialized <- shiny::reactiveVal(FALSE)
    
    
    # Label output
    output$label <- shiny::renderText({
      if (shiny::is.reactive(label)) label() else label
    })
    
   
    # Initialize CountUp
    initialize_countup <- function(startVal = 0,
                                   endVal = 3,
                                   duration = 2,
                                   decimal_places = 0,
                                   autoRunOnStartup = FALSE,
                                   enableScrollspy = FALSE,
                                   separator = ",",
                                   decimal = ".",
                                   prefix = "",
                                   suffix = "",
                                   autostart = F) {
      config = list(
        id = id,
        startVal = startVal,
        endVal = endVal,
        decimals = decimal_places,
        duration = duration,
        autoRunOnStartup = autoRunOnStartup,
        enableScrollspy = enableScrollspy,
        autoRunOnStartup = autoRunOnStartup,
        enableScrollspy = enableScrollspy,
          separator = separator,
          decimal =  decimal,
          prefix =  prefix,
          suffix =  suffix
      )
  
      session$sendCustomMessage("countup-init", config)
      is_initialized(TRUE)
    }
    observe({
      initialize_countup(endVal = endVal)  # endVal from function argument
    })
    
    # Auto-initialize when UI is ready
    shiny::observe({
      if (!is_initialized()) {
        initialize_countup()
        
        # Auto-start if requested
        if (auto_start) {
          shiny::invalidateLater(100)
          start_animation()
        }
      }
    })
    
    # Start animation function
    start_animation <- function() {

      session$sendCustomMessage("countup-start", list(
         id = id
      ))
      
    }
    
    # Pause/Resume function
    pause_resume <- function() {
      session$sendCustomMessage("countup-pause", list(id = id))
    }
    
    # Reset function
    reset_counter <- function() {

      session$sendCustomMessage("countup-reset", list(
        id = id
      ))
      
    }
    
    # Update function
    update_counter <- function(updateVal) {
      session$sendCustomMessage("countup-update", list(
        id = id,
        updateVal = updateVal
  
      ))
    }
    # Update function
    # pause_resume <- function() {
    #   session$sendCustomMessage("countup-pauseResume", list(
    #     id = id
    #   ))
    # }


    
    # Return control methods
    return(list(
      start = start_animation,
      pause = pause_resume,
      reset = reset_counter,
      update = update_counter,
      get_value = function() current_value(),
      is_initialized = function() is_initialized()
    ))
  })
}
# 
# # Demo function
# if (interactive()) {
#   demo_countup <- function() {
#     ui <- shiny::fluidPage(
#       shiny::titlePanel("CountUp.js Shiny Module Demo"),
#       
#       shiny::fluidRow(
#         shiny::column(6,
#           shiny::h4("Basic Counter"),
#           countup_ui("counter1", startVal = 0, suffix = " items")
#         ),
#         shiny::column(6,
#           shiny::h4("Currency Counter"),  
#           countup_ui("counter2", startVal = 0, prefix = "$", decimal_places = 2)
#         )
#       ),
#       
#       shiny::fluidRow(
#         shiny::column(12,
#           shiny::h4("External Control Example"),
#           shiny::actionButton("external_start", "Start", class = "btn btn-primary"),
#           shiny::actionButton("external_update", "Update", class = "btn btn-info"),
#           shiny::actionButton("external_reset", "Reset", class = "btn btn-secondary")
#         ),
#         shiny::numericInput("external_value", "Set Value:", value = 100, step = 1)
#       )
#     )
#     
#     server <- function(input, output, session) {
#       # Initialize counters
#       counter1 <- countup_server("counter1", "Items Sold",endVal = 5, auto_start = TRUE)
#       
#       counter1$start()
#       # External controls
#       
#       shiny::observeEvent(input$external_start, {
#  
#         
#         counter1$start()
#       })
#       
#       shiny::observeEvent(input$external_update, {
#         session$sendCustomMessage("countup-start", list(
#           endVal = 500
#         ))
#         #current_value(new_value)
#       
#       
#         #counter1$update(input$external_value)
#         
#       })
#       
#       shiny::observeEvent(input$external_reset, {
#         counter1$reset()
# 
#       })
#     }
# 
#     shiny::shinyApp(ui, server, options = list(hot.reload = TRUE))
#   }
# 
#   # Uncomment to run demo
#    #demo_countup()
# }
# 
