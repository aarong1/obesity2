# Intervention Module

A Shiny module for creating interactive intervention timeline charts using ECharts. This module allows users to drag points on a timeline to simulate intervention effects and visualize their impact over time.

## Features

- **Interactive Timeline**: Drag points to modify intervention values
- **Subsequent Years Control**: Option to apply changes to all subsequent years
- **Taper Effect**: Option to gradually taper intervention effects over time  
- **Line Series Toggle**: Show/hide connecting lines between points
- **Canvas Dragging**: Export chart as draggable image to external editors
- **Responsive Design**: Works on desktop and mobile devices
- **Module Architecture**: Proper Shiny module with isolated namespace

## Files Structure

```
modules/intervention_module/
├── intervention_module.R      # Main module (UI & Server functions)
├── intervention_chart.js      # ECharts chart functionality
├── canvas_dragging.js         # Canvas drag-and-drop features  
├── intervention_styles.css    # Module-specific CSS styles
├── README.md                  # This documentation
└── example_usage.R           # Example implementation
```

## Installation & Setup

1. **Copy module files** to your Shiny app's `modules/intervention_module/` directory

2. **Add JavaScript files** to your app's `www/js/` directory:
   ```
   www/
   └── js/
       ├── intervention_chart.js
       └── canvas_dragging.js  # (optional, if using canvas drag)
   ```

3. **Source the module** in your main app file:
   ```r
   source('modules/intervention_module/intervention_module.R')
   ```

4. **Include CSS** (optional) in your UI:
   ```r
   includeCSS('modules/intervention_module/intervention_styles.css')
   ```

## Basic Usage

### Simple Implementation

```r
library(shiny)
source('modules/intervention_module/intervention_module.R')

# UI
ui <- fluidPage(
  titlePanel("Intervention Timeline Demo"),
  
  intervention_module_ui(
    id = "intervention1",
    chart_height = "300px",
    show_controls = TRUE
  )
)

# Server  
server <- function(input, output, session) {
  # Initialize intervention module
  intervention_result <- intervention_module_server("intervention1")
  
  # React to changes
  observe({
    if (intervention_result$is_modified()) {
      cat("Chart has been modified!\n")
      cat("Interactions:", intervention_result$interaction_count(), "\n")
    }
  })
}

shinyApp(ui = ui, server = server)
```

### Advanced Usage with Custom Data

```r
# Custom initial data (year, value pairs)
initial_data <- list(
  c(2020, 1.0), c(2021, 1.0), c(2022, 1.0), c(2023, 1.0),
  c(2024, 1.2), c(2025, 1.5), c(2026, 1.3), c(2027, 1.1)
)

# UI with custom configuration
ui <- fluidPage(
  intervention_module_ui(
    id = "advanced_intervention",
    chart_height = "400px", 
    chart_width = "90%",
    show_controls = TRUE,
    enable_canvas_drag = TRUE,  # Enable drag-to-editor functionality
    initial_data = initial_data
  ),
  
  # Display current data
  verbatimTextOutput("current_data")
)

# Server with data processing
server <- function(input, output, session) {
  result <- intervention_module_server(
    id = "advanced_intervention",
    initial_data = initial_data,
    enable_canvas_drag = TRUE
  )
  
  # Convert chart data to data frame for analysis
  output$current_data <- renderText({
    df <- intervention_data_to_df(result$chart_data())
    paste(capture.output(print(df)), collapse = "\n")
  })
  
  # React to specific changes
  observeEvent(result$last_interaction(), {
    interaction <- result$last_interaction()
    if (!is.null(interaction)) {
      showNotification(
        paste("Point", interaction$dataIndex, "modified"),
        type = "message"
      )
    }
  })
}
```

## API Reference

### UI Function

```r
intervention_module_ui(
  id,                    # Character: Module ID for namespacing
  chart_height = "250px", # Character: CSS height of chart
  chart_width = "100%",   # Character: CSS width of chart  
  show_controls = TRUE,   # Logical: Show control checkboxes
  enable_canvas_drag = FALSE, # Logical: Enable drag-to-editor
  initial_data = NULL     # List: Initial chart data points
)
```

### Server Function  

```r
intervention_module_server(
  id,                     # Character: Module ID (matches UI)
  initial_data = NULL,    # Reactive/Static: Initial data
  enable_canvas_drag = FALSE # Logical: Enable canvas dragging
)
```

### Return Values

The server function returns a list with these reactive values:

- **`chart_data()`**: Current chart data as list of c(year, value) pairs
- **`last_interaction()`**: Details of the most recent drag interaction
- **`is_modified()`**: Boolean indicating if chart differs from initial state
- **`interaction_count()`**: Total number of drag interactions
- **`inputs`**: List of reactive input values (change_subsequent, etc.)
- **`update_data(new_data)`**: Method to programmatically update chart
- **`reset()`**: Method to reset chart to initial state

### Utility Functions

```r
# Convert chart data to data frame
df <- intervention_data_to_df(chart_data)

# Convert data frame back to chart format  
chart_data <- df_to_intervention_data(df)
```

## Configuration Options

### Chart Controls

- **Snap Subsequent Years**: When dragging a point, apply the same value to all subsequent years
- **Taper Subsequent Years**: Gradually reduce intervention effect over time
- **Show Line Series**: Display connecting lines between data points

### Canvas Dragging

When `enable_canvas_drag = TRUE`, users can:
1. Click "Enable Chart Dragging" to make the chart draggable
2. Drag the chart to external applications (e.g., document editors)
3. The chart exports as a PNG image

## Data Format

Chart data should be a list of numeric vectors, each containing `c(year, value)`:

```r
data <- list(
  c(2020, 1.0),    # Year 2020, value 1.0
  c(2021, 1.2),    # Year 2021, value 1.2
  c(2022, 0.8)     # Year 2022, value 0.8
)
```

## Dependencies

- **Shiny**: Core framework
- **ECharts**: JavaScript charting library (loaded from CDN)
- **jQuery**: For DOM manipulation (included with Shiny)

## Browser Support

- Chrome 60+
- Firefox 55+  
- Safari 12+
- Edge 79+

## Troubleshooting

### Common Issues

1. **Chart not rendering**: Ensure ECharts CDN is accessible and JavaScript files are in correct location
2. **Drag not working**: Check browser console for JavaScript errors
3. **Canvas drag fails**: Verify `enable_canvas_drag = TRUE` in both UI and server calls
4. **Multiple instances conflict**: Ensure each module instance has a unique `id`

### Debug Mode

Enable debug output by adding this to your server function:

```r
observe({
  result <- intervention_module_server("your_id")
  cat("Debug - Chart Data:", length(result$chart_data()), "points\n")
})
```

## License

This module is part of the PHM/Obesity project and follows the same licensing terms.