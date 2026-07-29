library(shiny)
library(tidyverse)
library(bslib)
library(htmltools)

dot <- function(color = '#bbb') {
  ifelse(
    color == 1,
    yes = (color <- 'limegreen'),
    ifelse(
      color == 0,
      yes = (color <- 'red'),
      (color <- '#bbb')
    )
  )
  
  div(
    style = paste0(
      'height: 15px;',
      'width: 15px;',
      'background-color: ', color, ';',
      'border-radius: 50%;',
      'display: inline-block;'
    )
  )
}

# Example data load
x <- read.csv('./components/sandbox/risk_data/Risk Equations-Table 1.csv')

risk_table_df <- x[c(
  'Pathology',
  'used',
  'study',
  'paper',
  'statistical.model',
  'recommending.body',
  'risk.duration.years.',
  'calculator'
)] %>% 
  rowwise() %>% 
  mutate(
    Pathology = as.character(tagList(
      div(
        style = 'width:400px;',
        dot(used),
        h5(style = 'display:inline-block;margin-left:8px;', Pathology),
        p(class = 'd-inline px-4 text-muted', 'CVD')
      )
    ))
  ) %>% 
  mutate(
    study = as.character(
      tagList(
        div(
          style = 'width:350px;',
          tags$img(src = "www/framingham_heart_study.svg", width = "30px"),
          h5(style = 'display:inline-block;padding:10px;', study)
        ),
        tags$a(
          icon(style='padding:5px 5px 5px 35px;', class='fs-5 m-1', 'calculator'),
          'Risk Calculator',
          href = calculator
        ),
        tags$a(
          icon(class='fs-5 p-1 m-1', 'graduation-cap'),
          'Publication',
          href = paper
        )
      )
    )
  ) %>% 
  mutate(
    paper = as.character(
      tags$a(
        icon(class='fs-5 p-1 m-1','graduation-cap'),
        'Source',
        href = paper
      )
    )
  ) %>% 
  mutate(
    calculator = as.character(
      div(
        tags$a(
          icon(class='fs-5 p-1 m-1','calculator'),
          'Risk Calculator',
          href = calculator
        )
      )
    )
  ) %>% 
  mutate(
    reco = sample(
      x = c('www/NICE_logo.png','www/acc.png','www/aha.png'),
      size = 1,
      prob = c(1,1,1),
      replace = TRUE
    )
  ) %>% 
  mutate(
    reco = as.character(
      div(style = 'width:50px;',
          tags$img(src = reco, width = "55px")
      )
    )
  ) %>% 
  mutate(
    horizon = as.character(
      div(
        style = 'width:200px;',
        tags$span(
          style = 'float:right;width:90%;',
          class = "p-1 mx-3 d-inline-block badge text-bg-warning pill",
          h5(class='m-1 d-inline-block', risk.duration.years.),
          p(class='d-inline-block', 'year risk horizon')
        )
      )
    )
  ) %>% 
  mutate(
    statistical.model = as.character(
      div(
        contentEditable = 'true',
        statistical.model
      )
    )
  ) %>% 
  ungroup() %>% 
  select(Pathology, study, reco, horizon, statistical.model)

# --------- Pure HTML renderer (no DT) ------------------

render_risk_table <- function(df) {
  tags$div(
    id = "model_registry",
    class = "table-responsive",
    tags$table(
      class = "table table-borderless w-100",
      # Build all rows
      do.call(
        tagList,
        lapply(seq_len(nrow(df)), function(i) {
          row_vals <- df[i, ]
          tags$tr(
            class = "hv",
            lapply(row_vals, function(cell) {
              tags$td(
                class = "align-middle",
                HTML(cell)  # interpret cell as HTML, not plain text
              )
            })
          )
        })
      )
    )
  )
}

ui <- page_fluid(
  # For the icon() calls
  icon(style = 'display:none;', 'graduation-cap'),
  
  # Styles
  tags$head(
    tags$style(HTML("
      /* Card-like rows for model registry */
      #model_registry .hv {
        margin: 15px;
      }

      #model_registry tr {
        display: block;
        padding: 15px !important;
        transition: all 0.5s ease 0.2s;
      }

      #model_registry tr:hover {
        border-radius: 10px;
        box-shadow: 4px 4px 10px #bebebe, -4px -4px 10px #ffffff;
        padding: 15px !important;
        margin-left: 20px;
        transition: all 0.5s ease;
      }

      a {
        color: black;
        text-decoration: none;
      }

      a:hover {
        margin: 5px !important;
        transition: all 1.5s ease 0.2s;
      }

      td {
        padding: 10px !important;
        margin: 10px !important;
        border: solid lightgrey;
        border-width: 0px 0px 1px 0px;
      }
    "))
  ),
  
  # Our vanilla HTML table
  render_risk_table(risk_table_df)
)

server <- function(input, output, session) {
  print(getwd())
}

# shinyApp(ui, server)