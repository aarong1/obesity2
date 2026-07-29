# https://css-tricks.com/snippets/svg/shape-morphing-icons-button-click/


          
          




# https://css-tricks.com/snippets/css/apple-com-hamburger-bun-menu/

library(tidyverse)
library(shiny)
library(htmltools)

div(
  tags$head(
    tags$script(src = "https://code.jquery.com/jquery-3.6.0.min.js"),
    tags$script(HTML(
      "

// We're going to select some things and make them variables
var select = function(s) {
  return document.querySelector(s);
},
  icons = select('#icons'),
button = select('.button'),
buttonText = document.getElementById('button-text')

// Morph the Download icon into the Checkmark icon
var buttonTl = new TimelineMax({paused:true});
buttonTl.to('#download', 1, {
  morphSVG:{shape:'#checkmark'},
  ease:Elastic.easeInOut
})

// On button click, play the animation
button.addEventListener('click', function() {
  if (buttonTl.time() > 0) {
    buttonTl.reverse();
    
  } else {
    buttonTl.play(0);
  }
})

// On button click, swap out the button text
button.addEventListener('click', function() {  
  if (button.classList.contains('saved')) {
    button.classList.remove('saved');
    buttonText.innerHTML = 'Download';
  } else {
    button.classList.add('saved');
    buttonText.innerHTML = 'Saved!';
  }
}, false);



")),
    tags$style("
          /* The main SVG */
          .button-icons {
            width: 1.25em;
          }
        
        /* The individual icons */
          .icon {
            fill: #fff;
          }
        
        /* We hide the checkmark by default */
          #checkmark {
          visibility: hidden;
        }
")
    
  ),
  
  
  HTML('
<a class="button" href="#">
  
  <!-- The main SVG where both shapes will be drawn -->
  <svg id="icons" class="button-icons" version="1.1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 32 32">
    
    <!-- The download icon -->
    <path id="download" class="icon" d="M28 16h-5l-7 7-7-7h-5l-4 8v2h32v-2l-4-8zM0 28h32v2h-32v-2zM18 10v-8h-4v8h-7l9 9 9-9h-7z"></path>
      
      <!-- The checkmark icon -->
      <path id="checkmark" class="icon" d="M27 4l-15 15-7-7-5 5 12 12 20-20z"></path>
        </svg>
        
        <!-- The button text -->
        <!-- The ID will be used to swap the text with JavaScript -->
        <span id="button-text">Download</span>
          
          </a>')
) %>% browsable()
