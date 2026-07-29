library(shiny)

progress_pair_module_ui <- function(id,
                                    left_width = 40,
                                    right_width = 10,
                                    height = "20px") {
  ns <- NS(id)

  css_text <- sprintf(
    
      "#%s .progress {
        height: %s;
        width: 100%%;
      }
      
      #%s .progress-bar {
        transition: width 480ms ease;
      }",
    
    ns("wrap"),
    height,
    ns("wrap")
  )

  js_text <- sprintf(
      "(function () {

      console.log('progress pair module js loaded');
  function asPercent(value) {
    if (value === null || value === undefined || value === '') {
      return '0%%';
    }

    if (typeof value === 'number') {
      return value + '%%';
    }

    var text = String(value).trim();
    return text.endsWith('%%') ? text : text + '%%';
  }

  function setWidths(message) {
      console.log('setWidths called');

    if (!message || message.moduleId !== '%s') {
    console.log('no message content');
      return;
    }

    var leftBar = document.getElementById('%s');
    var rightBar = document.getElementById('%s');

    if (!leftBar || !rightBar) {
    console.log('no bar');
      return;
    }

    var leftWidth = asPercent(message.leftWidth);
    var rightWidth = asPercent(message.rightWidth);

    leftBar.style.width = leftWidth;
    rightBar.style.width = rightWidth;

    leftBar.setAttribute('aria-valuenow', String(message.leftWidth));
    rightBar.setAttribute('aria-valuenow', String(message.rightWidth));
  }
  
    Shiny.addCustomMessageHandler('progressPairSetWidths', setWidths);
  
})();",
    id,
    ns("left_bar"),
    ns("right_bar")
  )

  
    tagList(
      tags$head(
        tags$style(HTML(css_text)),
        tags$script(HTML(js_text))
      ),
      div(
        id = ns("wrap"),
        class = "w-100",
        div(
          class = "progress w-100",
          div(
            id = ns("left_bar"),
            class = "progress-bar bg-primary progress-bar-striped progress-bar-animated",
            role = "progressbar",
            style = paste0("width: ", left_width, "%"),
            `aria-valuenow` = as.character(left_width),
            `aria-valuemin` = "0",
            `aria-valuemax` = "100"
          ),
          div(
            id = ns("right_bar"),
            class = "progress-bar bg-primary-subtle",
            role = "progressbar",
            style = paste0("width: ", right_width, "%"),
            `aria-valuenow` = as.character(right_width),
            `aria-valuemin` = "0",
            `aria-valuemax` = "100"
          )
        )
      )
    )
  
}

progress_pair_module_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    widths <- reactiveValues(left = 40, right = 10)

    set_widths <- function(left_width, right_width) {
      widths$left <- left_width
      widths$right <- right_width

      session$sendCustomMessage(
        'progressPairSetWidths',
        list(
          moduleId = id,
          leftWidth = left_width,
          rightWidth = right_width
        )
      )
    }

    list(
      widths = reactive({
        list(left = widths$left, right = widths$right)
      }),
      set_widths = set_widths
    )
  })
}