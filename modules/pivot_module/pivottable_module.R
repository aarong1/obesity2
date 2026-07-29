source('modules/pivot_module/pivottable.R')
require(reactable)

# ----------------------------------------------------------------------------
# Shiny module: UI + server to drive the programmable pivot
# ----------------------------------------------------------------------------

#' Pivot module UI
#' @param id Shiny module id
#' @param data_names Optional vector of column names to offer (defaults to names(data()) in server)
#' @return UI elements for selecting groups/values/functions and viewing a table
pivot_module_ui <- function(id, 
                            data_names = NULL,
                            fun_names = c("sum","mean","median","min","max","sd","count","n_distinct")#var
                            ) {
  ns <- shiny::NS(id)
  
  # CSS for drag and drop styling
  drag_drop_css <- shiny::tags$style(shiny::HTML("
  
    .column-pool {
      min-height: 100px;
      border: 0px solid #ccc;
      border-radius: 20px;  

      padding: 30px;
      margin: 30px;
    }
    
    .drop-zone {
      min-height: 90px;
      width: 20vw;
      border: 0px solid #007bff;
      border-radius: 20px;
      padding: 20px;
      margin: 20px;
    opacity:0.8;
    }
    
    .drop-zone.numeric{
    background: var( --bs-info-bg-subtle);
    }
    
    .drop-zone.categorical{
    background: var( --bs-cyan);
    background-opacity:0.8;
    }
    
    .drop-zone.function{
     /* border: 3px dashed var( --bs-danger-bg-subtle); */
    background:var( --bs-danger-bg-subtle);
    background-opacity:0.1;
    }
    
    .drop-zone.function-value{
    border: 3px solid var( --bs-info-bg-subtle););
    /*background:var( --bs-warning-bg-subtle);*/
    background-opacity:0.1;
    }
    
    /*
    .drop-zone.drag-over {
      background-color: #e6f3ff;
      border-color: #0056b3;
    }
    */
    .draggable-column {
      display: inline-block;
      padding: 3px 5px;
      margin: 3px;
      background-color: #007bff;
      color: white;
      border-radius: 4px;
      cursor: move;
      user-select: none;
    }
    
    .draggable-column:hover {
      background-color: #0056b3;
    }

    /* Numeric/categorical coloring for chips */
    .draggable-column.numeric {
      background: var( --bs-info-bg-subtle);
      color:  #333;
    }

    .draggable-column.categorical {
      background: var(--bs-cyan);
      color:#fff;
    }
    
       .draggable-column.function {
      background-color: var( --bs-danger-bg-subtle);
      color: #fff;
      color: #333;
    }
    
    
    .drop-zone-label {
      font-weight: bold;
      margin-bottom: 5px;
      color: #333;
    }
    
    .grid-container {
      display: grid;
      grid-template-columns: 1fr 1fr 0.6fr;
      grid-template-rows: 1fr 1fr;
      gap: 20px;
      padding: 20px;
      max-width: 1200px;
      margin: 0 auto;
    }
    
    .grid-element {
      min-height: 120px;
    }
    
    .grid-element.tall {
      grid-row: span 2;
    }
    
    /* Value-Function Pairs specific styling */
    #value-func-drop {
      width: 10vw;
      height:80%;
      display: flex;
      flex-wrap: wrap;
      align-content: flex-start;
    }
    
    #value-func-drop .draggable-column {
      width: calc(50% - 6px);
      margin: 3px;
      text-align: center;
      box-sizing: border-box;
    }
    
    #value-func-drop .draggable-column:nth-child(odd) {
      margin-right: 3px;
    }
    
    #value-func-drop .draggable-column:nth-child(even) {
      margin-left: 3px;
    }
  "))
  
  # JavaScript for drag and drop functionality using interact.js
  drag_drop_js <- shiny::tags$script(shiny::HTML('
  (function(){
    function getIdMap(){
      var node = document.querySelector(".pivot-idmap");
      if(!node) return null;
      return {
        groups: node.getAttribute("data-groups-id"),
        values: node.getAttribute("data-values-id"),
        wide:   node.getAttribute("data-wide-id"),
        funs:   node.getAttribute("data-funs-id"),
        valueFuncs: node.getAttribute("data-value-func-id")
      };
    }

    function updateInputs(idmap){
      var groups = Array.from(document.querySelectorAll("#groups-drop .draggable-column"))
                        .map(function(x){return x.getAttribute("data-col")||x.textContent;});
      var values = Array.from(document.querySelectorAll("#values-drop .draggable-column"))
                        .map(function(x){return x.getAttribute("data-col")||x.textContent;});
      var wideBy = "";
      var wb = document.querySelector("#wide-by-drop .draggable-column");
      if (wb) wideBy = wb.getAttribute("data-col")||wb.textContent;
      
      var funs = Array.from(document.querySelectorAll("#function-drop .draggable-column"))
                  .map(function(x){return x.getAttribute("data-col")||x.textContent;});
      
      // Extract value-function pairs
      var valueFuncPairs = Array.from(document.querySelectorAll("#value-func-drop .draggable-column"))
                  .map(function(x){return x.getAttribute("data-col")||x.textContent;});
                        
      if (idmap){
        Shiny.setInputValue(idmap.groups, groups, {priority:"event"});
        Shiny.setInputValue(idmap.values, values, {priority:"event"});
        Shiny.setInputValue(idmap.wide,   wideBy, {priority:"event"});
        Shiny.setInputValue(idmap.funs,   funs, {priority:"event"});
        Shiny.setInputValue(idmap.valueFuncs, valueFuncPairs, {priority:"event"});
      }
    }

    function appendChip(zone, col){
      // Allow duplicates in value-func-drop zone, avoid duplicates elsewhere
      if (zone.id !== "value-func-drop") {
        var exists = Array.from(zone.querySelectorAll(".draggable-column"))
                          .some(function(n){return (n.getAttribute("data-col")||n.textContent)===col;});
        if (exists) return;
      }
      
      var el = document.createElement("span");
      el.className = "draggable-column";
      el.textContent = col;
      el.setAttribute("data-col", col);
      // Add class for numeric/categorical
      var meta = window.pivotColMeta || {};
      if (meta[col] === "numeric") {
        el.classList.add("numeric");
      } else if (meta[col] === "categorical") {
        el.classList.add("categorical");
      }else {
        el.classList.add("function");
      }
      zone.appendChild(el);
      makeDraggable(el);
    }

    function makeDraggable(el){
      if (!el || el.getAttribute("data-draggable")) return;
      el.setAttribute("data-draggable","1");
      interact(el).draggable({
        inertia: true,
        listeners: {
          move: function (event) {
            var target = event.target;
            var x = (parseFloat(target.getAttribute("data-x")) || 0) + event.dx;
            var y = (parseFloat(target.getAttribute("data-y")) || 0) + event.dy;
            target.style.transform = "translate(" + x + "px, " + y + "px)";
            target.setAttribute("data-x", x);
            target.setAttribute("data-y", y);
          },
          end: function (event) {
            var t = event.target; t.style.transform = ""; t.removeAttribute("data-x"); t.removeAttribute("data-y");
          }
        }
      });
    }

    function setupDropzone(selector, opts){
      interact(selector).dropzone({
        accept: ".draggable-column.numeric",
        overlap: 0.2,
        ondragenter: function (ev) { ev.target.classList.add("drag-over"); },
        ondragleave: function (ev) { ev.target.classList.remove("drag-over"); },
        ondrop: function (ev) {
          var zone = ev.target;
          zone.classList.remove("drag-over");
          var col = ev.relatedTarget.getAttribute("data-col") || ev.relatedTarget.textContent;

          if (zone.id === "wide-by-drop" && zone.querySelector(".draggable-column")) return; // only one

          appendChip(zone, col);
          // If dropping back to pool from another zone, remove original
          if (zone.id === "column-pool"){
            var src = ev.relatedTarget.parentElement;
            if (src && src.classList.contains("drop-zone")){
              src.removeChild(ev.relatedTarget);
            }
          }
          updateInputs(getIdMap());
        }
      });
    }
    
     function setupDropzoneFunc(selector, opts){
      interact(selector).dropzone({
        accept: ".draggable-column.function",
        overlap: 0.2,
        ondragenter: function (ev) { ev.target.classList.add( "drag-over"); },
        ondragleave: function (ev) { ev.target.classList.remove( "drag-over"); },
        ondrop: function (ev) {
          var zone = ev.target;
          zone.classList.remove("drag-over");
          var col = ev.relatedTarget.getAttribute("data-col") || ev.relatedTarget.textContent;

          if (zone.id === "wide-by-drop" && zone.querySelector(".draggable-column")) return; // only one

          appendChip(zone, col);
          // If dropping back to pool from another zone, remove original
          if (zone.id === "column-pool"){
            var src = ev.relatedTarget.parentElement;
            if (src && src.classList.contains("drop-zone")){
              src.removeChild(ev.relatedTarget);
            }
          }
          updateInputs(getIdMap());
          
        }
      });
     }
    function setupDropzoneCat(selector, opts){
      interact(selector).dropzone({
        accept: ".draggable-column.categorical",
        overlap: 0.2,
        ondragenter: function (ev) { ev.target.classList.add("drag-over"); },
        ondragleave: function (ev) { ev.target.classList.remove("drag-over"); },
        ondrop: function (ev) {
          var zone = ev.target;
          zone.classList.remove("drag-over");
          var col = ev.relatedTarget.getAttribute("data-col") || ev.relatedTarget.textContent;

          if (zone.id === "wide-by-drop" && zone.querySelector(".draggable-column")) return; // only one

          appendChip(zone, col);
          // If dropping back to pool from another zone, remove original
          if (zone.id === "column-pool"){
            var src = ev.relatedTarget.parentElement;
            if (src && src.classList.contains("drop-zone")){
              src.removeChild(ev.relatedTarget);
            }
          }
          updateInputs(getIdMap());
        }
      });
    }
    
    function setupDropzoneValueFunc(selector, opts){
      interact(selector).dropzone({
        accept: ".draggable-column.numeric, .draggable-column.function",
        overlap: 0.2,
        ondragenter: function (ev) { ev.target.classList.add("drag-over"); },
        ondragleave: function (ev) { ev.target.classList.remove("drag-over"); },
        ondrop: function (ev) {
          var zone = ev.target;
          zone.classList.remove("drag-over");
          var col = ev.relatedTarget.getAttribute("data-col") || ev.relatedTarget.textContent;

          appendChip(zone, col);
          // If dropping back to pool from another zone, remove original
          if (zone.id === "column-pool" || zone.id === "function-pool"){
            var src = ev.relatedTarget.parentElement;
            if (src && src.classList.contains("drop-zone")){
              src.removeChild(ev.relatedTarget);
              
            }
          }
          updateInputs(getIdMap());
        }
      });
    }

    // Double-click removal inside any drop-zone
    document.addEventListener("dblclick", function(e){
      if (e.target && e.target.classList.contains("draggable-column") && e.target.parentElement.classList.contains("drop-zone")){
        e.target.parentElement.removeChild(e.target);
        updateInputs(getIdMap());
      }
    });
    
    // Initialize existing chips as draggable (pool or zones)
    function init(){
      document.querySelectorAll(".draggable-column").forEach(function(n){ makeDraggable(n); });
      
      setupDropzone("#function-pool");
      setupDropzoneFunc("#function-drop");
      setupDropzone("#column-pool");
      setupDropzoneCat("#groups-drop");
      setupDropzoneCat("#wide-by-drop");
      setupDropzone("#values-drop");
      setupDropzoneValueFunc("#value-func-drop");
    }
    
    // Listen for column pool updates from server
    Shiny.addCustomMessageHandler("updateColumns", function(data){
      window.pivotColMeta = data.types || {};
      var pool = document.getElementById("column-pool");
      if (!pool) return;
      while (pool.firstChild) pool.removeChild(pool.firstChild);
      console.log(data);
      Array.from(data.columns || []).forEach(function(col){ appendChip(pool, col); });
    });
    
    // Kick off once DOM is ready
    if (document.readyState === "loading") {
      document.addEventListener("DOMContentLoaded", init);
    } else { init(); }
  })();
'))
  
  shiny::tagList(
    drag_drop_css,
    shiny::tags$script(src = "https://cdn.jsdelivr.net/npm/interactjs/dist/interact.min.js"),
    drag_drop_js,


        # id mapping div for JS
        shiny::tags$div(
          class = "pivot-idmap",
          `data-groups-id` = ns("groups"),
          `data-values-id` = ns("values"),
          `data-wide-id`   = ns("wide_by"),
          `data-funs-id`   = ns("funs"),
          `data-value-func-id` = ns("value_func_pairs"),
        ),
        # Column pool
    div(class = 'd-flex gap-3 justify-content-center',
        shiny::tags$div(
          style = "margin-top:30px;font-size:12px;color:#333;",
          shiny::strong("Legend"),
          shiny::div(
            shiny::span(style="display:inline-block;width:30px;height:15px;background:var(--bs-cyan);border:0px solid #ccc;border-radius:7px;") ,
            " Categorical values"
          ),
          shiny::div(
            shiny::span(style="display:inline-block;width:30px;height:15px;background:var(--bs-info-bg-subtle);border:0px solid #ccc;border-radius:7px;") ,
            " Numeric values"
          ),
          shiny::div(
            shiny::span(style="display:inline-block;width:30px;height:15px;background:var(--bs-danger-bg-subtle);border:0px solid #ccc;border-radius:7px;"),
            " Function values"
          )
          
        ),
        shiny::div(
          #shiny::h4("Available Columns"),
          shiny::div(
            id = "column-pool",
            class = "column-pool bg-light-subtle",
            if (!is.null(data_names)) {
              lapply(data_names, function(col) {
                shiny::tags$span(class = "draggable-column", col)
              })
            } else {
              shiny::p("Loading columns...", style = "color: #666; font-style: italic;")
            }
          )
        ),
        shiny::div(
          #shiny::h4("Available Columns"),
          shiny::div(
            id = "function-pool",
            class = "column-pool bg-light-subtle",
            
            if (!is.null(fun_names)) {
              lapply(fun_names, function(col) {
                shiny::tags$span(class = "draggable-column function", col)
              })
            } else {
              shiny::p("Loading columns...", style = "color: #666; font-style: italic;")
            }
          )
        )

    ),
        # Drop zones in grid layout
        div(class = 'grid-container',
        # Top left - Dimensions
        shiny::div(
          class = "grid-element",
          shiny::div(class = "drop-zone-label", "Dimensions"),
          shiny::tags$small(class = "text-muted", "group by variables"),
          shiny::div(id = "groups-drop", class = "drop-zone categorical")
        ),
        # Top right - Measures  
        shiny::div(
          class = "grid-element",
          shiny::div(class = "drop-zone-label", "Measures"),
          shiny::tags$small(class = "text-muted", "numeric arguments"),
          shiny::div(id = "values-drop", class = "drop-zone numeric")
        ),
        # Right column (tall) - Value-Function Pairs
        shiny::div(
          class = "grid-element tall",
          shiny::div(class = "drop-zone-label", "Value-Function Pairs"),
          shiny::div(
          shiny::tags$small(class = "text-muted", "Drag column then function    "),
            shiny::span(style="display:inline-block;width:15px;height:10px;background:var(--bs-info-bg-subtle);border:0px solid #ccc;border-radius:5px;margin-left:12px;"),
            shiny::span(style="display:inline-block;width:15px;height:10px;background:var(--bs-danger-bg-subtle);border:0px solid #ccc;border-radius:5px;margin-left:2px;"),
            shiny::span(style="display:inline-block;width:15px;height:10px;background:var(--bs-info-bg-subtle);border:0px solid #ccc;border-radius:5px;margin-left:2px;"),
            shiny::span(style="display:inline-block;width:15px;height:10px;background:var(--bs-danger-bg-subtle);border:0px solid #ccc;border-radius:5px;margin-left:2px")
          ),
          shiny::div(id = "value-func-drop", class = "drop-zone function-value")
        ),
        # Bottom left - Pivot Wide
        shiny::div(
          class = "grid-element",
          shiny::div(class = "drop-zone-label", "Pivot Wide (max 1)"),
          shiny::tags$small(class = "text-muted font", "group variables"),
          shiny::div(id = "wide-by-drop", class = "drop-zone categorical")
        ),
        # Bottom right - Function
        shiny::div(
          class = "grid-element",
          shiny::div(class = "drop-zone-label", "Function"),
          shiny::tags$small(class = "text-muted font", "variable aggregation"),
          shiny::div(id = "function-drop", class = "drop-zone function")
        )
        ),
        # Aggregation functions (keep as selectize for now)
        # shiny::selectizeInput(ns("funs"), "Aggregations:",
        #                       choices = c("sum","mean","median","min","max","sd","var","count","n_distinct"),
        #                       multiple = TRUE, selected = c("mean","count")),
        # Hidden inputs to store the drag-drop selections
        shiny::tags$input(id = ns("groups"), type = "hidden"),
        shiny::tags$input(id = ns("values"), type = "hidden"),
        shiny::tags$input(id = ns("wide_by"), type = "hidden"),
        shiny::tags$input(id = ns("funs"), type = "hidden"),
        shiny::tags$input(id = ns("value_func_pairs"), type = "hidden"),
 
         shiny::br(),
 
        shiny::tags$span(class = 'text-body-secondary', "Tip: Drag columns to drop zones, double-click to remove. For column-specific functions, drag column then function to 'Value-Function Pairs' zone.", 
                style = "margin-left:5%;font-size: 12px; color: #666;"),
 
 
        # Download button
        br(),br(),
        
       
 
        # Pivot table output
        div( class = 'd-flex w-100 h-25 gap-3 justify-content-center',
              reactableOutput(ns('retbl'))
        ), 
    
    shiny::br(),
    tableOutput('tbl'),
    
    div(class = 'd-flex justify-content-end mb-3',
          shiny::downloadButton(ns("download_csv"), "Download CSV", 
                               class = "btn btn-outline primary btn-sm")
        )
  )
}

#' Pivot module server

#' @param id Shiny module id
#' @return A reactive expression with the current pivoted data
pivot_module_server <- function(id, data) {
  shiny::moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # Support both reactive and non-reactive data inputs
    data_rx <- if (shiny::is.reactive(data)) data else shiny::reactive(data)

    # Update choices dynamically from data
    shiny::observe({
      print(input$groups)

      shiny::req(data_rx())
      nms <- names(data_rx())
      shiny::updateSelectizeInput(session, "groups", choices = nms, server = TRUE)
      shiny::updateSelectizeInput(session, "values", choices = nms, server = TRUE)
      shiny::updateSelectInput(session, "wide_by", choices = c("", nms))
      shiny::updateSelectizeInput(session, "funs", choices = c("sum","mean","median","min","max","sd","count","n_distinct") #,"var" 
                                  )
      shiny::updateSelectizeInput(session, "value_func_pairs", 
                                  choices = c(nms,c("sum","mean","median","min","max","sd","count","n_distinct")),
                                  server = TRUE)
      
    })

    # Refresh column pool for JS when data changes, and send column types
    shiny::observe({
      # print( data_rx() )
      shiny::req(data_rx())
      cols <- names(data_rx())
      types <- vapply(data_rx(), function(x) if (is.numeric(x)) 'numeric' else 'categorical', character(1))
      session$sendCustomMessage('updateColumns', list(columns = as.list(cols), types = as.list(types)))
    })

    # observe({
    #   print("Functions:", input$funs)
    #   print("Value-function pairs:", input$value_func_pairs)
    # })

    # Helper function to parse value-function pairs into the expected format
    parse_value_func_pairs <- function(pairs) {
      if (is.null(pairs) | length(pairs) == 0) return(NULL)
      
      # Simple approach: expect alternating column-function pattern
      # e.g., c("mpg", "mean", "mpg", "sd", "hp", "sum")
      result <- list()
      i <- 1
      while (i < length(pairs)) {
        col_name <- pairs[i]
        fun_name <- pairs[i + 1]
        
        # Check if this looks like a function name
        valid_funs <- c("sum", "mean", "median", "min", "max", "sd", "var", "count", "n_distinct")
        if (fun_name %in% valid_funs) {
          if (is.null(result[[col_name]])) {
            result[[col_name]] <- fun_name
          } else {
            result[[col_name]] <- c(result[[col_name]], fun_name)
          }
          i <- i + 2
        } else {
          i <- i + 1  # Skip if pattern doesn't match
        }
      }
      
      if (length(result) > 0) result else NULL
    }

    pivoted <- shiny::reactiveVal(NULL)
    
    shiny::observe({
      print(input$groups)
      
      shiny::req(data_rx())
      req(input$groups)
      print(input$wide_by)
      print(input$values)
      # print(names(data_rx()))
      print(data_rx()[1:5,1:5])
      
      
      # wb <- if (nzchar(input$wide_by %||% NULL)) input$wide_by else NULL
      
      wb <- if (length(input$wide_by) ==0 ) input$wide_by else NULL
      
      
      # Check if we have value-function pairs, use them if available
      value_func_mapping <- parse_value_func_pairs(input$value_func_pairs)
      print(value_func_mapping)
      
      if (!is.null(value_func_mapping)) {
        # Use the new value_funs parameter
          print('if')
        x <- pivot_agg(
          data_rx(),
          groups = c(input$groups, wb) %||% character(),
          value_funs = value_func_mapping,
          wide_by = wb
        )
      } else {
        print('else')
        # Fall back to original behavior
        x <- pivot_agg(
          data_rx(),
          groups = c(input$groups, wb) %||% character(),
          values = input$values %||% character(),
          funs   = input$funs %||% 'mean',
          wide_by = wb
        )
      }
      # print(pivot_agg(
      #   data_rx(),
      #   groups = c(input$groups, wb) %||% character(),
      #   values = input$values %||% character(),
      #   funs   = input$funs %||% 'mean',
      #   wide_by = wb
      # ))
      
      # print(x)
      pivoted(x)
      # print(pivoted())
      # print(pivoted)
      
    })
    
    output$tbl <- renderTable ({

      # shiny::req(pivoted())
      print(pivoted())
      pivoted()
    })
    
    output$retbl <- renderReactable({
      print('in table')
      print(pivoted())
      # shiny::req(pivoted(), cancelOutput = TRUE)
      reactable::reactable(as.data.frame(pivoted()))
    })
    
    # Download handler
    output$download_csv <- downloadHandler(
      filename = function() {
        paste0("pivot_table_", Sys.Date(), ".csv")
      },
      content = function(file) {
        req(pivoted())
        write.csv(pivoted(), file, row.names = FALSE)
      }
    )
   
    # Return reactive
    return( 
      reactive({ pivoted() }) 
      )
  })
}

# ----------------------------------------------------------------------------
# Mini demos (only run when interactive())
# ----------------------------------------------------------------------------
if (interactive()) {
  
  # Example 1: tidyverse helper
  ex1 <- pivot_agg(mtcars, groups = c("cyl","gear"), values = c("mpg","hp"), funs = c("mean","sd","count"))
  print(utils::head(ex1))

  # Example 2: custom functions
  ex2 <- pivot_agg(mtcars, groups = "cyl", values = "qsec",
                   funs = list(p90 = ~quantile(.x, 0.9, na.rm = TRUE), iqr = IQR))
  print(ex2)

  # Example 3: data.table variant if available
  if (requireNamespace("data.table", quietly = TRUE)) {
    ex3 <- dt_pivot(mtcars, groups = c("cyl","gear"), values = c("mpg","hp"), funs = c("mean","sd","count"))
    print(utils::head(ex3))
  }

  # Example 4: launch a tiny Shiny app using the module
  launch_pivot_demo <- function() {
    ui <- bslib::page_fluid(
      #shiny::titlePanel("Programmable Pivot Demo"),
      pivot_module_ui("piv")
    )
    server <- function(input, output, session) {
      pivot_module_server("piv", data = shiny::reactive(as.data.frame(Titanic)))
      
    }
    shiny::shinyApp(ui, server)
  }
library(bslib)
library(shiny)
options(shiny.autoreload = TRUE)
  # Uncomment to run the demo
   launch_pivot_demo()
}

# End of file
