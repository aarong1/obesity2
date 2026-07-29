# ============================================================================
# INTERVENTION MODULE - Interactive ECharts Timeline Module
# ============================================================================
# Shiny module for creating draggable intervention timelines with ECharts

# Required libraries
if (!requireNamespace("shiny", quietly = TRUE)) {
  stop("Package 'shiny' is required for this module")
}

if (!requireNamespace("echarts4r", quietly = TRUE)) {
  stop("echarts4r 'shiny' is required for this module")
}



# ----------------------------------------------------------------------------
# MODULE UI FUNCTION
# ----------------------------------------------------------------------------

#' Intervention Module UI
#' 
#' Creates UI elements for an interactive intervention timeline chart
#' 
#' @param id Character string. Shiny module id for namespacing
#' @param chart_height Character string. CSS height for the chart container (default: "250px")
#' @param chart_width Character string. CSS width for the chart container (default: "100%")
#' @param show_controls Logical. Whether to show control checkboxes (default: TRUE)
#' @param enable_canvas_drag Logical. Whether to enable canvas dragging functionality (default: FALSE)
#' @param initial_data List. Initial data for the chart in format list(c(year, value), ...)
#' @return Shiny UI elements
#' @examples 
#' intervention_module_ui("intervention1", chart_height = "300px")
#' 

# source('./modules/intervention_module/javascript.R')
intervention_module_ui <- function(id, 
                                   # chart_height = "250px",
                                   # chart_width = "100%",
                                   #show_controls = TRUE,
                                   # enable_canvas_drag = FALSE,
                                   initial_data = NULL) {
  ns <- shiny::NS(id)
  
  # Include required external dependencies
  dependencies <- shiny::tagList(
    # ECharts from CDN
    
    
    # Module JavaScript files (these should be in www/js/ in your app)
    #shiny::tags$script(HTML(first_js)),#src = "js/intervention_chart.js"),
    
    # Canvas dragging if enabled
    #if (enable_canvas_drag) {
     # shiny::tags$script(HTML(second_js)),#src = "js/canvas_dragging.js"),
   #},
    
    # Module styles
    shiny::tags$style(shiny::HTML("
      .intervention-container {
        margin: 15px;
        padding: 20px;
        border-radius: 8px;
        /*box-shadow: 0 2px 4px rgba(0,0,0,0.1);*/
      }
      
      .intervention-controls {
        display: flex !important;
        justify-content: space-evenly;
        align-items: flex-end;
        margin: 15px 0;
      }
      
      .intervention-controls .checkbox {
        text-align: center;
        margin: 0 10px;
      }
      
      .intervention-chart {
        border: 1px solid #ddd;
        border-radius: 8px;
        background: white;
      }
    "))
  )
  
  # Control checkboxes (if enabled)
 controls <- #if (show_controls) {
    shiny::div(
      class = "intervention-controls",
      shiny::div(
        class = "text-center",
        shiny::checkboxInput(
          inputId = ns('change_subsequent_graph'),
          label = 'Snap all subsequent Years to intervention',
          value = TRUE,
          # checked = TRUE,
          width = '100%'
        ),
        shiny::checkboxInput(
          inputId = ns('taper_subsequent_graph'),
          label = 'Taper subsequent Years', 
          value = FALSE,
          width = '100%'
        ),
        shiny::checkboxInput(
          inputId = ns("toggle_line_series"),
          label = "Show line through points",
          value = FALSE,
          width = '100%'
        )
      )
    )#,
 # }
  
  # Canvas drag controls (if enabled)
  canvas_controls <- #if (enable_canvas_drag) {
    shiny::div(
      class = "canvas-drag-controls",
      style = "margin: 10px 0;",
      shiny::actionButton(
        inputId = ns("toggle_open"),
        label = "Enable Chart Dragging",
        class = "btn btn-sm btn-primary"
      ),
      shiny::actionButton(
        inputId = ns("toggle_close"), 
        label = "Disable Chart Dragging",
        class = "btn btn-sm btn-secondary"
      )
    )#,
 # }
  
  # Main UI structure
  shiny::tagList(
    dependencies,
    
    shiny::div(
      class = "bg-white intervention-container",
      
      # Chart container
      shiny::div(
        id = ns('chart_container'),
        #class = 'intervention-chart shadow-sm border-3 z-5',
        
           style = 'height:200px'#paste0('height:', chart_height, '; width:', chart_width, ';')
      ),
      
      shiny::br(),
      
      # Controls
      controls,
      #canvas_controls,
      
      # Hidden initialization script
      # shiny::tags$script(shiny::HTML(cat("
      #   $(document).ready(function() {
      #     // Initialize intervention chart when DOM is ready
      #     setTimeout(function() {
      #       if (typeof window.initInterventionChart === 'function') {
      #         const options = {
      #           initialData: ", jsonlite::toJSON(initial_data %||% list(), auto_unbox = TRUE), ",
      #           symbolSize: 15
      #         };
      #         window.initInterventionChart('", ns('chart_container'), "', '", id, "', options);
      #       } else {
      #         console.error('Intervention chart initialization function not found');
      #       }
      #       // Initialize canvas drag manager if enabled
      #       ", if (enable_canvas_drag) paste0("
      #       if (typeof window.initCanvasDragManager === 'function') {
      #         window.initCanvasDragManager('", id, "', {
      #           editorId: 'editor'
      #         });
      #       }
      #       ") else "", "
      #     }, 100);
      #   });
      # ")))
      
      shiny::tags$script(
        shiny::HTML("      var symbolSize = 15;
      var data = [  [2026, 1], [2027, 1], [2028, 1], [2029, 1], [2030, 1], [2031, 1], [2032, 1], [2033, 1], [2034, 1], [2035, 1], [2036, 1], [2037, 1], [2038, 1], [2039, 1], [2040, 1], [2041, 1], [2042, 1], [2043, 1], [2044, 1], [2045, 1]
      ];
      var myChart = echarts.init(document.getElementById('intervention1-chart_container'));
      console.log('s');
      console.log(myChart);
      console.log('s');
      
      function renderChart() {
      console.log('rendering chart');

        myChart.setOption({
          grid: {
            left: 30,
            right: 20,
            top: 30,
            bottom: 30,
            containLabel: true
          },
          tooltip: {
            triggerOn: 'none',
            position: function (point, params, dom, rect, size) {
              const x = point[0];
              const y = point[1];
              const boxWidth = size.contentSize[0];
              const boxHeight = size.contentSize[1];
              
              // Offset the tooltip to the right and center it vertically
              return [x - 15, y + boxHeight / 2];
            },
            formatter: function (params) {
              const year = params.data[0];
              const yValue = params.data[1];
              const note = yValue !== 1 ? '<br /><b>Intervened</b>' : '';
              return 'Year: ' + year + '<br /> ' + yValue.toFixed(2) + note;
            }
          },
          xAxis: {
            type: 'value',
            min: 2025,
            max: 2046,
            axisLine: { show: false },
            axisTick: { show: false },
            axisLabel: {
              show: true,
              formatter: function (value) {
                return String(value); // treat as years
              }
            },
            splitLine: {
              show: true,
              lineStyle: { type: 'dashed', color: '#ccc' }
            }
          },
          yAxis: {
            type: 'value',
            min: 0,
            max: 1.5,
            axisLine: { show: false },
            axisTick: { show: false },
            axisLabel: { show: false },
            splitLine: {
              show: true,
              lineStyle: { type: 'dashed', color: '#eee' }
            }
          },
          series: [
            { id: 'a', type: 'scatter', smooth: true,  itemStyle: {
              //color: '#4add8c'
              color: 'NA'
              },
              
              symbolSize: symbolSize, 
              data: data }
          ]
        });
        
        myChart.setOption({
          graphic: echarts.util.map(data, function(item, dataIndex) {
            return {
              type: 'circle',
              position: myChart.convertToPixel('grid', item),
              shape: { r: symbolSize / 2 },
              invisible: true,
              draggable: true,
              ondrag: echarts.util.curry(onPointDragging, dataIndex),
              onmousemove: echarts.util.curry(showTooltip, dataIndex),
              onmouseout: echarts.util.curry(hideTooltip, dataIndex),
              z: 100
            };
          })
        });
      }
      
      function showTooltip(dataIndex) {
        myChart.dispatchAction({ type: 'showTip', seriesIndex: 0, dataIndex: dataIndex });
      }
      
      function hideTooltip(dataIndex) {
        myChart.dispatchAction({ type: 'hideTip' });
      }
      
      function onPointDragging(dataIndex) {
        
        const draggedX = data[dataIndex][0];
        const newY = myChart.convertFromPixel('grid', this.position)[1];
        
        // Update y for current and all subsequent x values
        const save_taper_array = []
        
        if (data[dataIndex][0] == draggedX) {
          data[dataIndex][1] = newY;
        }
        
        console.log(Shiny.shinyapp.$inputValues['intervention1-taper_subsequent_graph']);
        
        if ( Shiny.shinyapp.$inputValues['intervention1-change_subsequent_graph'] &&
             Shiny.shinyapp.$inputValues['intervention1-taper_subsequent_graph']) {
          
          for (let i = dataIndex; i < data.length; i++) {
            
            //console.log((data[i][0]-draggedX)/(2026-draggedX));
            //console.log(data);
            if (data[i][0] > draggedX) {
              data[i][1] = (newY-1) * ((2045 - data[i][0])/(2045 - draggedX))+1;
            }
          }
        }
        
        if ( Shiny.shinyapp.$inputValues['intervention1-change_subsequent_graph'] && 
        !Shiny.shinyapp.$inputValues['intervention1-taper_subsequent_graph']) {
          for (let i = dataIndex; i < data.length; i++) {
            if (data[i][0] > draggedX) {
              data[i][1] = newY;
            }
          }
        }
        
        if ( ! Shiny.shinyapp.$inputValues['intervention1-toggleLineSeries'] ) {
          
          
          myChart.setOption({
            series: [{ id: 'a', data: data }]
          });
        } else {
            
          //set option WITH lines
          myChart.setOption({
            series: [
              { id: 'a', data: data },
              { id: 'line-a', data: data }
            ]
          });
        }
        
        myChart.setOption({
          graphic: echarts.util.map(data, function (item, dataIndex) {
            return {
              position: myChart.convertToPixel('grid', item)
            };
          })
        });
        
        Shiny.setInputValue('${moduleId}-draggable_data', {
            dataIndex: dataIndex,
            newData: data,
            nonce: Math.random()
          });
      
      
          Shiny.setInputValue('draggable_data', {
            dataIndex: dataIndex,
            newData: data,
            nonce: Math.random()
          });


        if (window.Shiny) {
          //Shiny.setInputValue('draggable_data', {
          Shiny.setInputValue('${moduleId}-draggable_data', {
            dataIndex: dataIndex,
            newData: data,
            nonce: Math.random()
          });
        }
      }
      
      console.log(myChart);
      renderChart();
      
      $(window).on('resize', function() {
        myChart.setOption({
          graphic: echarts.util.map(data, function(item, dataIndex) {
            return { position: myChart.convertToPixel('grid', item) };
          })
        });
      });
      
      //-----------
        
        if (window.Shiny) {
          Shiny.addCustomMessageHandler('toggleLineSeries', function(show) {
            if (show) {
              // Add line series if not already added
              myChart.setOption({
                series: [
                  {
                    id: 'line-a',
                    type: 'line',
                    smooth: true,
                    lineStyle: {
                      //color: '#4add8c',
                      color: 'rgb(226, 179, 108)',
                      width: 2
                    },
                    symbol: 'none',
                    data: data
                  }
                ]
              });
            } else {
              // Remove the line series
              myChart.setOption({
                series: [
                  {
                    id: 'line-a',
                    data: [] // effectively removes line visually
                  }
                ]
              });
            }
          });
        }"
      )
      )
      
      
    )
  )
}

# ----------------------------------------------------------------------------  
# MODULE SERVER FUNCTION
# ----------------------------------------------------------------------------

#' Intervention Module Server
#' 
#' Server logic for the intervention timeline module
#' 
#' @param id Character string. Shiny module id (must match UI)
#' @param initial_data Reactive or static data. Initial chart data 
#' @param enable_canvas_drag Logical. Whether canvas dragging is enabled
#' @return Named list of reactive values:
#'   - chart_data: Current chart data
#'   - last_interaction: Last drag interaction details
#'   - is_modified: Whether chart has been modified from initial state
#'   - interaction_count: Number of drag interactions
#' @examples
#' server <- function(input, output, session) {
#'   result <- intervention_module_server("intervention1")
#'   observe({
#'     cat("Chart modified:", result$is_modified())
#'   })
#' }
#' 
intervention_module_server <- function(id, 
                                       initial_data = NULL,
                                       enable_canvas_drag = FALSE) {
  
  shiny::moduleServer(id, function(input, output, session) {
    
    # Reactive values to store module state
    values <- shiny::reactiveValues(
      chart_data =     # Initialize default data if none provided
        if(!is.null(initial_data)){
               initial_data
          }else{
               list(
                 c(2025, 1), c(2026, 1), c(2027, 1), c(2028, 1), c(2029, 1),
                 c(2030, 1), c(2031, 1), c(2032, 1), c(2033, 1), c(2034, 1),
                 c(2035, 1), c(2036, 1), c(2037, 1), c(2038, 1), c(2039, 1),
                 c(2040, 1), c(2041, 1), c(2042, 1), c(2043, 1), c(2044, 1), 
                 c(2045, 1)
        )
          },
      last_interaction = NULL,
      interaction_count = 0,
      is_modified = FALSE
    )
    
    shiny::observeEvent(input$`intervention1-draggable_data`, {
      print(input$`intervention1-draggable_data`)
    })
      
      
      shiny::observeEvent(input$draggable_data, {
        print(input$draggable_data)
      })
        
 
    
    
    # Handle line series toggle
    shiny::observeEvent(input$toggle_line_series, {
      print(input$toggle_line_series)
      print(input$`intervention1-draggable_data`)
      
      session$sendCustomMessage("toggleLineSeries", list(
        moduleId = id,
        show = input$show_line_series
      ))
    })
    
    # Handle drag interactions from chart
    shiny::observeEvent(input$draggable_data, {
      print(input$draggable_data)
      req(input$draggable_data)
      
      # Update stored data
      values$chart_data <- input$draggable_data$newData
      values$last_interaction <- input$draggable_data
      values$interaction_count <- values$interaction_count + 1
      values$is_modified <- TRUE
      
      # Debug output (can be removed in production)
      cat("Intervention drag detected:\n")
      cat("  Data Index:", input$draggable_data$dataIndex, "\n")
      cat("  Interaction Count:", values$interaction_count, "\n")
      
      # Convert nested list to matrix for easier processing
      if (!is.null(input$draggable_data$newData)) {
        data_matrix <- do.call(rbind, input$draggable_data$newData)
        cat("  Data Matrix Dimensions:", dim(data_matrix), "\n")
      }
    })
      
    # Canvas drag functionality (if enabled)

    
    # Method to update chart data externally
    update_chart_data <- function(new_data) {
      values$chart_data <- new_data
      session$sendCustomMessage("interventionUpdateData", list(
        moduleId = id,
        data = new_data
      ))
    }
    
    # Method to reset chart to initial state
    reset_chart <- function() {
      values$chart_data <- initial_data
      values$is_modified <- FALSE
      values$interaction_count <- 0
      update_chart_data(initial_data)
    }
    
    # Return reactive interface
    return(list(
      # Reactive values
      chart_data = shiny::reactive({ values$chart_data }),
      last_interaction = shiny::reactive({ values$last_interaction }),
      is_modified = shiny::reactive({ values$is_modified }),
      interaction_count = shiny::reactive({ values$interaction_count }),
      
      # Methods
      update_data = update_chart_data,
      reset = reset_chart,
      
      # Access to raw input values (for advanced use)
      inputs = list(
        change_subsequent = shiny::reactive({ input$change_subsequent_graph }),
        taper_subsequent = shiny::reactive({ input$taper_subsequent_graph }),
        show_line = shiny::reactive({ input$show_line_series })
      )
    ))
  })
}

# ----------------------------------------------------------------------------
# UTILITY FUNCTIONS
# ----------------------------------------------------------------------------

#' Convert chart data to data frame
#' 
#' Helper function to convert chart data to a more convenient data frame format
#' 
#' @param chart_data List of c(year, value) pairs from the module
#' @return data.frame with columns 'year' and 'value'
#' 
intervention_data_to_df <- function(chart_data) {
  if (is.null(chart_data) || length(chart_data) == 0) {
    return(data.frame(year = numeric(0), value = numeric(0)))
  }
  
  # Handle both list of vectors and matrix formats
  if (is.matrix(chart_data)) {
    data.frame(
      year = chart_data[, 1],
      value = chart_data[, 2]
    )
  } else if (is.list(chart_data)) {
    data.frame(
      year = sapply(chart_data, `[`, 1),
      value = sapply(chart_data, `[`, 2)
    )
  } else {
    stop("Unsupported chart_data format")
  }
}

#' Convert data frame to chart data format
#' 
#' Helper function to convert data frame back to chart format
#' 
#' @param df data.frame with 'year' and 'value' columns
#' @return List format suitable for the intervention chart
#' 
df_to_intervention_data <- function(df) {
  if (!all(c("year", "value") %in% names(df))) {
    stop("Data frame must have 'year' and 'value' columns")
  }
  
  lapply(seq_len(nrow(df)), function(i) {
    c(df$year[i], df$value[i])
  })
}