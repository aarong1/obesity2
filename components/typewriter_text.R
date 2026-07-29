library(shiny)
#https://css-tricks.com/snippets/css/typewriter-effect/
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .typewriter h1 {
        overflow: hidden;
        border-right: .15em solid orange;
        white-space: nowrap;
        margin: 0 auto;
        letter-spacing: .15em;
        animation:
          typing 7.5s steps(40, end),
          blink-caret 1.25s step-end infinite;
        display: inline-block;
      }

      @keyframes typing {
        from { width: 0 }
        to { width: 100% }
      }

      @keyframes blink-caret {
        from, to { border-color: transparent }
        50% { border-color: orange; }
      }
    "))
  ),
  
  div(class = "typewriter",
      h1(textOutput("typeText"))
  )
)

server <- function(input, output, session) {
  output$typeText <- renderText({
    # Dynamically generated text
    Sys.Date() |> as.character() %>% paste("Today's date is",.)
  })
}

# shinyApp(ui, server)
