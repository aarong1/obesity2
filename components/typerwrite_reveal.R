library(shiny)
#https://css-tricks.com/snippets/css/typewriter-effect/

#https://mattboldt.com/demos/typed-js/

browsable(tagList(
  tags$head(
    tags$style(HTML("
    
     /* 
     body {
        background-color: #120439;
        height: 100%;
        font-family: 'Arial', sans-serif;
      }
      
      h1 {
        font-size: 5em;
        color: white;
        text-transform: uppercase;
      }
      */

      span.cursor {
        border-right: .05em solid;
        animation: caret 1s steps(1) infinite;
      }

      @keyframes caret {
        50% {
          border-color: transparent;
        }
      }
    ")),
    tags$script(HTML("
      document.addEventListener('DOMContentLoaded', function(event){
        var dataText = [\"Atrial Fibrillation Use Case\"];

        function typeWriter(text, i, fnCallback) {
          if (i < text.length) {
            document.querySelector(\"#typewriter\").innerHTML = text.substring(0, i+1) + '<span class=\"cursor\" aria-hidden=\"true\"></span>';
            setTimeout(function() {
              typeWriter(text, i + 1, fnCallback);
            }, 100);
          } else if (typeof fnCallback === 'function') {
            setTimeout(function() {
              // Remove the blinking cursor after 1.5 seconds
              let el = document.querySelector('#typewriter .cursor');
              if (el){ 
              el.style.animation = 'none';
              el.style.opacity = 0;
              }
              fnCallback();
            }, 1500);
          }
        }

        function StartTextAnimation(i) {
          if (typeof dataText[i] == 'undefined') {
            setTimeout(function() {
              StartTextAnimation(0);
            }, 20000);
          } else if (i < dataText.length) {
            typeWriter(dataText[i], 0, function(){
              StartTextAnimation(i + 1);
            });
          }
        }

        StartTextAnimation(0);
      });
    "))
  ),
  tags$div(
    align = "center",
    tags$h1(id = "typewriter", style = 'float:left;', "")
  ))
)



ui <- fluidPage(
  tags$head(
    tags$style(HTML("
    
     /* 
     body {
        background-color: #120439;
        height: 100%;
        font-family: 'Arial', sans-serif;
      }
      */
      
      h1 {
        font-size: 5em;
        color: white;
        text-transform: uppercase;
      }

      span.cursor {
        border-right: .05em solid;
        animation: caret 1s steps(1) infinite;
      }

      @keyframes caret {
        50% {
          border-color: transparent;
        }
      }
    ")),
    tags$script(HTML("
      document.addEventListener('DOMContentLoaded', function(event){
        var dataText = [\"Wij zijn Codefield!\"];

        function typeWriter(text, i, fnCallback) {
          if (i < text.length) {
            document.querySelector(\"#typewriter\").innerHTML = text.substring(0, i+1) + '<span class=\"cursor\" aria-hidden=\"true\"></span>';
            setTimeout(function() {
              typeWriter(text, i + 1, fnCallback);
            }, 100);
          } else if (typeof fnCallback === 'function') {
            setTimeout(function() {
              // Remove the blinking cursor after 1.5 seconds
              let el = document.querySelector('#typewriter .cursor');
              if (el){ 
              el.style.animation = 'none';
              el.style.opacity = 0;
              }
              fnCallback();
            }, 1500);
          }
        }

        function StartTextAnimation(i) {
          if (typeof dataText[i] == 'undefined') {
            setTimeout(function() {
              StartTextAnimation(0);
            }, 20000);
          } else if (i < dataText.length) {
            typeWriter(dataText[i], 0, function(){
              StartTextAnimation(i + 1);
            });
          }
        }

        StartTextAnimation(0);
      });
    "))
  ),
  tags$div(
    align = "center",
    tags$h1(id = "typewriter", "Hallo, Wij zijn Codefield!")
  )
)

server <- function(input, output, session) {}

# shinyApp(ui, server)
