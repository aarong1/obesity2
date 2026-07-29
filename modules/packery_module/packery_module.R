# ============================================================================
# PACKERY.JS SHINY MODULE
# ============================================================================
# Masonry layout with draggable, resizable grid items
# Wraps Packery.js library with Shiny reactive interface
# 
# MAIN COMPONENTS:
# 1. packery_ui()     - Creates HTML structure, CSS, JS handlers, UI controls
# 2. packery_server() - Manages state, provides control methods, handles events

# ============================================================================
# UI FUNCTION - Creates the visual interface and client-side functionality
# ============================================================================
#' Packery Module UI
#' 
#' Builds complete UI including:
#' - HTML dependencies (Packery.js CDN)
#' - CSS styling for visual appearance
#' - JavaScript handlers for layout management
#' - HTML structure (container, items, controls)
#' 
#' @param id Shiny module id
#' @param items List of items to display, each item should have 'id', 'content', 'width', 'height'
#' @param item_selector CSS selector for grid items (default: ".grid-item")
#' @param column_width Width of columns in pixels (default: 200)
#' @param gutter Gutter between items in pixels (default: 10)
#' @param draggable Enable dragging of items (default: TRUE)
#' @param resize Enable resizing of items (default: FALSE)
#' @param fit_width Fit container width to available space (default: TRUE)
#' @param container_class CSS class for the main container
#' @param item_class CSS class for grid items
#' @return UI element with Packery layout
packery_ui <- function(id,
                       items = list(),
                       item_selector = ".grid-item",
                       column_width = 200,
                       gutter = 10,
                       draggable = TRUE,
                       resize = FALSE,
                       fit_width = TRUE,
                       container_class = "packery-container",
                       item_class = "grid-item") {
  ns <- shiny::NS(id)
  
  # --------------------------------------------------------------------------
  # DEPENDENCY MANAGEMENT - Loads external JavaScript libraries via CDN
  # --------------------------------------------------------------------------
  packery_dep <- htmltools::htmlDependency(
    name = "packery",
    version = "2.1.2",
    src = list(href = "https://unpkg.com/packery@2/dist/"),
    script = "packery.pkgd.min.js"
  )
  
  # Draggabilly dependency (for dragging functionality)
  draggabilly_dep <- htmltools::htmlDependency(
    name = "draggabilly",
    version = "3.0.0",
    src = list(href = "https://unpkg.com/draggabilly@3/dist/"),
    script = "draggabilly.pkgd.min.js"
  )
  
  # --------------------------------------------------------------------------
  # CSS STYLING - Visual appearance and layout
  # --------------------------------------------------------------------------
  packery_css <- htmltools::tags$style(htmltools::HTML(paste0("
    .", container_class, " {
      max-width: 100%;
      margin: 20px auto;
      background: #f9f9f9;
      border-radius: 8px;
      padding: 10px;
    }
    
    .", item_class, " {
      width: ", column_width, "px;
      background: #fff;
      border: 1px solid #ddd;
      border-radius: 6px;
      margin-bottom: ", gutter, "px;
      padding: 15px;
      box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      cursor: ", if(draggable) "move" else "default", ";
      transition: all 0.3s ease;
      position: relative;
    }
    
    .", item_class, ":hover {
      border-color: hsla(0, 0%, 100%, 0.5);
      cursor: move;
    }
    
    .", item_class, ".is-dragging,
    .", item_class, ".is-positioning-post-drag {
      background: #f0f0f0;
      z-index: 2;
    }
    
    .packery-drop-placeholder {
      outline: 3px dashed hsla(0, 0%, 0%, 0.5);
      outline-offset: -6px;
      -webkit-transition: -webkit-transform 0.2s;
      transition: transform 0.2s;
    }
    
    .", item_class, " .item-content {
      word-wrap: break-word;
    }
    
    .", item_class, " .item-header {
      font-weight: bold;
      margin-bottom: 10px;
      color: #333;
      border-bottom: 1px solid #eee;
      padding-bottom: 5px;
    }
    
    .", item_class, " .item-controls {
      position: absolute;
      top: 5px;
      right: 5px;
      opacity: 0;
      transition: opacity 0.3s ease;
    }
    
    .", item_class, ":hover .item-controls {
      opacity: 1;
    }
    
    .packery-controls {
      margin: 10px 0;
      text-align: center;
    }
    
    .packery-controls .btn {
      margin: 2px;
    }
    
    /* Size variations */
    .", item_class, ".w2 { width: ", column_width * 2 + gutter, "px; }
    .", item_class, ".w3 { width: ", column_width * 3 + gutter * 2, "px; }
    .", item_class, ".h2 { height: 200px; }
    .", item_class, ".h3 { height: 300px; }
  ")))
  
  # --------------------------------------------------------------------------
  # JAVASCRIPT HANDLERS - Client-side layout control and Shiny messaging
  # --------------------------------------------------------------------------
  # Creates global functions and message handlers for:
  # - initPackery()    - Initialize Packery.js instance
  # - addItem()        - Add new item to layout
  # - removeItem()     - Remove item from layout
  # - layoutItems()    - Trigger layout refresh
  # - Shiny message handlers for server communication
  packery_js <- htmltools::tags$script(htmltools::HTML(paste0('
  document.addEventListener("DOMContentLoaded", function() {
    window.packeryInstances = window.packeryInstances || {};
    
    function initPackery(id, options) {
      var container = document.getElementById(id + "-container");
      if (!container) return;
      
      console.log("Initializing Packery for:", id);
      console.log("Options:", options);
      
      var pckry = new Packery(container, {
        itemSelector: options.itemSelector || ".grid-item",
        columnWidth: options.columnWidth || 200,
        gutter: options.gutter || 10,
        fitWidth: options.fitWidth !== false,
        resize: true
      });
      
      // Enable dragging if requested (simplified approach)
      if (options.draggable !== false) {
        pckry.getItemElements().forEach(function(itemElem) {
          var draggie = new Draggabilly(itemElem);
          pckry.bindDraggabillyEvents(draggie);
        });
        
        // Update Shiny when items are dragged
        pckry.on("dragItemPositioned", function(event, draggedItem) {
          var itemId = draggedItem.element.getAttribute("data-item-id");
          Shiny.setInputValue(id + "_item_moved", {
            itemId: itemId,
            timestamp: Date.now()
          }, {priority: "event"});
        });
      }
      
      // Store instance
      window.packeryInstances[id] = pckry;
      
      // Initial layout
      pckry.layout();
      
      return pckry;
    }
    
    function addPackeryItem(id, itemData) {
      var pckry = window.packeryInstances[id];
      var container = document.getElementById(id + "-container");
      if (!pckry || !container) return;
      
      // Create new item element
      var itemElem = document.createElement("div");
      itemElem.className = "grid-item " + (itemData.className || "");
      itemElem.setAttribute("data-item-id", itemData.id);
      itemElem.innerHTML = itemData.content;
      
      // Add to container
      container.appendChild(itemElem);
      
      // Add to Packery
      pckry.appended(itemElem);
      
      // Enable dragging for new item (simplified)
      if (itemData.draggable !== false) {
        var draggie = new Draggabilly(itemElem);
        pckry.bindDraggabillyEvents(draggie);
      }
      
      // Layout
      pckry.layout();
    }
    
    function removePackeryItem(id, itemId) {
      var pckry = window.packeryInstances[id];
      if (!pckry) return;
      
      var itemElem = document.querySelector("[data-item-id=\'" + itemId + "\']");
      if (itemElem) {
        pckry.remove(itemElem);
        pckry.layout();
      }
    }
    
    function layoutPackery(id) {
      var pckry = window.packeryInstances[id];
      if (pckry) {
        pckry.layout();
      }
    }
    
    function destroyPackery(id) {
      var pckry = window.packeryInstances[id];
      if (pckry) {
        pckry.destroy();
        delete window.packeryInstances[id];
      }
    }
    
    // Message handlers for Shiny
    Shiny.addCustomMessageHandler("packery-init", function(data) {
      initPackery(data.id, data.options);
    });
    
    Shiny.addCustomMessageHandler("packery-add-item", function(data) {
      addPackeryItem(data.id, data.item);
    });
    
    Shiny.addCustomMessageHandler("packery-remove-item", function(data) {
      removePackeryItem(data.id, data.itemId);
    });
    
    Shiny.addCustomMessageHandler("packery-layout", function(data) {
      layoutPackery(data.id);
    });
    
    Shiny.addCustomMessageHandler("packery-destroy", function(data) {
      destroyPackery(data.id);
    });
  });
  ')))
  
  # --------------------------------------------------------------------------
  # HTML STRUCTURE - UI layout combining all components
  # --------------------------------------------------------------------------
  htmltools::tagList(
    packery_dep,
    draggabilly_dep,
    packery_css,
    packery_js,
    
    # Controls
    htmltools::div(
      class = "packery-controls",
      shiny::actionButton(ns("add_item"), "Add Item", class = "btn btn-primary btn-sm"),
      shiny::actionButton(ns("layout"), "Re-layout", class = "btn btn-secondary btn-sm"),
      shiny::actionButton(ns("clear_all"), "Clear All", class = "btn btn-danger btn-sm")
    ),
    
    # Main container
    htmltools::div(
      id = ns("container"),
      class = container_class,
      
      # Render initial items
      lapply(seq_along(items), function(i) {
        item <- items[[i]]
        htmltools::div(
          class = paste(item_class, item$class %||% ""),
          `data-item-id` = item$id %||% paste0("item-", i),
          
          # Item header
          if (!is.null(item$title)) {
            htmltools::div(class = "item-header", item$title)
          },
          
          # Item content
          htmltools::div(
            class = "item-content",
            if (is.character(item$content)) {
              htmltools::HTML(item$content)
            } else {
              item$content
            }
          ),
          
          # Item controls
          htmltools::div(
            class = "item-controls",
            htmltools::tags$button(
              class = "btn btn-xs btn-danger",
              onclick = paste0("Shiny.setInputValue('", ns("remove_item"), "', '", item$id %||% paste0("item-", i), "', {priority: 'event'})"),
              "×"
            )
          )
        )
      })
    ),
    
    # Hidden inputs
    shiny::tags$input(id = ns("config"), type = "hidden", value = jsonlite::toJSON(list(
      item_selector = item_selector,
      column_width = column_width,
      gutter = gutter,
      draggable = draggable,
      resize = resize,
      fit_width = fit_width
    )))
  )
}

# ============================================================================
# SERVER FUNCTION - Manages state and provides control methods
# ============================================================================
#' Packery Module Server
#' 
#' Provides server-side functionality including:
#' - State management (items, layout configuration)
#' - Layout control methods (add, remove, layout)
#' - Event handling for UI interactions
#' - Communication with JavaScript via custom messages
#' 
#' @param id Shiny module id
#' @param initial_items Reactive or static list of initial items
#' @return List with control methods: add_item(), remove_item(), layout(), clear()
packery_server <- function(id, initial_items = list()) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # --------------------------------------------------------------------------
    # STATE MANAGEMENT - Reactive values to track component state
    # --------------------------------------------------------------------------
    items <- shiny::reactiveVal(initial_items)
    is_initialized <- shiny::reactiveVal(FALSE)
    
    # --------------------------------------------------------------------------
    # CONFIGURATION MANAGEMENT - Get settings from UI
    # --------------------------------------------------------------------------
    get_config <- function() {
      if (!is.null(input$config)) {
        jsonlite::fromJSON(input$config)
      } else {
        list(
          item_selector = ".grid-item",
          column_width = 200,
          gutter = 10,
          draggable = TRUE,
          resize = FALSE,
          fit_width = TRUE
        )
      }
    }
    
    # --------------------------------------------------------------------------
    # INITIALIZATION - Set up Packery.js instance
    # --------------------------------------------------------------------------
    initialize_packery <- function() {
      config <- get_config()
      session$sendCustomMessage("packery-init", list(
        id = id,
        options = config
      ))
      is_initialized(TRUE)
    }
    
    # --------------------------------------------------------------------------
    # AUTO-INITIALIZATION - Set up when UI is ready
    # --------------------------------------------------------------------------
    shiny::observe({
      if (!is_initialized()) {
        shiny::invalidateLater(100)
        initialize_packery()
      }
    })
    
    # --------------------------------------------------------------------------
    # LAYOUT CONTROL FUNCTIONS - Core methods for layout manipulation
    # --------------------------------------------------------------------------
    
    # ADD ITEM - Add new item to layout
    add_item_func <- function(item) {
      current_items <- items()
      new_items <- append(current_items, list(item))
      items(new_items)
      
      session$sendCustomMessage("packery-add-item", list(
        id = id,
        item = item
      ))
    }
    
    # REMOVE ITEM - Remove item from layout
    remove_item_func <- function(item_id) {
      current_items <- items()
      new_items <- current_items[!sapply(current_items, function(x) x$id == item_id)]
      items(new_items)
      
      session$sendCustomMessage("packery-remove-item", list(
        id = id,
        itemId = item_id
      ))
    }
    
    # LAYOUT - Trigger layout refresh
    layout_func <- function() {
      session$sendCustomMessage("packery-layout", list(id = id))
    }
    
    # CLEAR - Remove all items
    clear_func <- function() {
      current_items <- items()
      for (item in current_items) {
        session$sendCustomMessage("packery-remove-item", list(
          id = id,
          itemId = item$id
        ))
      }
      items(list())
    }
    
    # --------------------------------------------------------------------------
    # EVENT HANDLERS - Respond to UI interactions
    # --------------------------------------------------------------------------
    
    # Add random item
    shiny::observeEvent(input$add_item, {
      new_item <- list(
        id = paste0("item-", as.numeric(Sys.time())),
        title = paste("Item", sample(1:100, 1)),
        content = paste("This is item content", sample(1:100, 1)),
        class = sample(c("", "w2", "h2"), 1)
      )
      add_item_func(new_item)
    })
    
    # Remove item
    shiny::observeEvent(input$remove_item, {
      if (!is.null(input$remove_item)) {
        remove_item_func(input$remove_item)
      }
    })
    
    # Re-layout
    shiny::observeEvent(input$layout, {
      layout_func()
    })
    
    # Clear all
    shiny::observeEvent(input$clear_all, {
      clear_func()
    })
    
    # Handle item moved events
    shiny::observeEvent(input[[paste0(id, "_item_moved")]], {
      # You can add custom logic here when items are moved
      message("Item moved: ", input[[paste0(id, "_item_moved")]]$itemId)
    })
    
    # --------------------------------------------------------------------------
    # RETURN VALUES - Methods for external control
    # --------------------------------------------------------------------------
    # Returns list of functions that can be called from outside the module:
    # add_item(item)   - Add new item to layout
    # remove_item(id)  - Remove item by ID
    # layout()         - Refresh layout
    # clear()          - Remove all items
    # get_items()      - Get current items
    return(list(
      add_item = add_item_func,
      remove_item = remove_item_func,
      layout = layout_func,
      clear = clear_func,
      get_items = function() items(),
      is_initialized = function() is_initialized()
    ))
  })
}

# ============================================================================
# DEMO FUNCTION - Example usage and testing
# ============================================================================
# Demonstrates:
# - Basic layout setup with different item sizes
# - Dynamic item addition and removal
# - External programmatic control
# - Drag and drop functionality
if (interactive()) {
  demo_packery <- function() {
    # Sample items
    sample_items <- list(
      list(id = "item1", title = "Welcome", content = "This is a basic item", class = ""),
      list(id = "item2", title = "Wide Item", content = "This item is twice as wide", class = "w2"),
      list(id = "item3", title = "Tall Item", content = "This item is taller than normal", class = "h2"),
      list(id = "item4", title = "Regular", content = "Another regular item", class = ""),
      list(id = "item5", title = "Big Item", content = "This item is both wide and tall", class = "w2 h2")
    )
    
    ui <- shiny::fluidPage(
      shiny::titlePanel("Packery.js Shiny Module Demo"),
      
      shiny::fluidRow(
        shiny::column(12,
          packery_ui("masonry", items = sample_items)
        )
      ),
      
      shiny::fluidRow(
        shiny::column(12,
          shiny::h4("External Controls"),
          shiny::actionButton("add_custom", "Add Custom Item", class = "btn btn-info"),
          shiny::actionButton("add_wide", "Add Wide Item", class = "btn btn-success"),
          shiny::actionButton("relayout", "Re-layout All", class = "btn btn-warning"),
          shiny::br(), shiny::br(),
          shiny::textInput("custom_title", "Custom Item Title:", "My Custom Item"),
          shiny::textAreaInput("custom_content", "Custom Item Content:", "This is custom content")
        )
      )
    )
    
    server <- function(input, output, session) {
      # Initialize packery
      packery_controller <- packery_server("masonry", sample_items)
      
      # External controls
      shiny::observeEvent(input$add_custom, {
        custom_item <- list(
          id = paste0("custom-", as.numeric(Sys.time())),
          title = input$custom_title,
          content = input$custom_content,
          class = ""
        )
        packery_controller$add_item(custom_item)
      })
      
      shiny::observeEvent(input$add_wide, {
        wide_item <- list(
          id = paste0("wide-", as.numeric(Sys.time())),
          title = "Wide Item",
          content = "This is a programmatically added wide item",
          class = "w2"
        )
        packery_controller$add_item(wide_item)
      })
      
      shiny::observeEvent(input$relayout, {
        packery_controller$layout()
      })
    }
    
    shiny::shinyApp(ui, server)
  }
  
  # Uncomment to run demo
  demo_packery()
}

