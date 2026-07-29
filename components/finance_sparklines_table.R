library(echarts4r)
library(tidyverse)
library(htmltools)


# Sample data
fixed <- data.frame(
  pri = 1,
  category = c("Fixed inital cost"),
  values= c(10,1,1,1,1,1,1,1,1,1)
)

running <- data.frame(
  pri = 2,
  category = c("Running costs"),
  values= rep(5,10)
)

savings_initial <- data.frame(
  pri = 3,
  category = c("Inital savings (immediate) "),
  values= c(8,8:1)
)

savings_burden <- data.frame(
  pri = 4,
  category = c("Ongoing savings (burden) "),
  values= c(10,9,8,7,6,5,4,3,2,1,0)
)

data <- rbind(fixed,running,savings_initial,savings_burden)

# Create bar sparkline
x <- data %>%
  e_charts(category) %>%
  e_bar(values, name = "Value") %>%
  e_tooltip(trigger = "item") %>%
  e_grid(left = "5%", right = "5%", top = "10%", bottom = "5%") %>%  # Adjust margins
  e_x_axis(show = FALSE) %>%  # Hide x-axis
  e_y_axis(show = FALSE) %>% tagList %>% as.character() %>% HTML()
library(reactable)


data.frame(x=x) %>% 
reactable(., columns = list(
  x = colDef(name = "x", html = TRUE) # Render as HTML
))

library(DT)
data.frame(x=x) %>% 
datatable( # Only display the relevant columns
  escape = FALSE,            # Prevent escaping of HTML so charts render
  options = list(
    columnDefs = list(list(targets = 1, className = "dt-center")) # Centre-align the chart column
  )
)

library(reactablefmtr)
library(dataui)

data %>% 
reactable(
  .,
  columns = list(

    values = colDef(
      name = "Chinstrap Penguin Flipper Length (min and max values highlighted)",
      cell = react_sparkbar(
        .,
         min_value = 0,
 highlight_bars = highlight_bars(min="red",max="blue")
    )
  )
))

shiny::fluidPage(
data %>%
 group_by(category) %>%
 summarize(values = list(values)
           ) %>%
 reactable(.,
           defaultColDef = colDef(style = list()), # Remove default column styling
  bordered = FALSE,                       # Remove borders
  striped = FALSE,                        # Remove striped rows
  highlight = FALSE,                      # Remove row highlighting
  outlined = FALSE,                       # Remove outlined styling
  compact = FALSE,                        # No compact layout
  theme = reactableTheme(
    cellPadding = 10,                      # Remove padding
    headerStyle = list(),                 # Remove header styling
    rowStyle = list()                     # Remove row styling
  ),
 columns = list(values = colDef(cell = react_sparkbar(.,
  min_value = 0,
  max_value =20,
 highlight_bars = highlight_bars(min="#13b5cb",max="#0d2e33")
                                )
 )
                )
 )
) %>% browsable()
