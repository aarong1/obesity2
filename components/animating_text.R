library(shiny)
library(htmltools)

animate_text <- function() {
  div(class = "wrapper",
      
      # Word containers
      span(class = "word wisteria", 'Population Health Model'),
      span(class = "word belize", 'Atrial Fibrillation Use Case'),
      span(class = "word pomegranate", 'Economic Analysis'),
      
      # JavaScript for animation
      tags$head(
        tags$script(HTML("
        document.addEventListener('DOMContentLoaded', function () {
let words = document.getElementsByClassName('word');
let wordArray = [];
let currentWord = 0;

words[currentWord].style.opacity = 1;

const splitLetters = word => {
    let content = word.textContent;
    word.textContent = '';
    let letters = [];

    for (let i = 0; i < content.length; i++) {
        let letter = document.createElement('span');
        letter.className = 'letter';

        // preserve space explicitly
        if (content.charAt(i) === ' ') {
            letter.innerHTML = '&nbsp;';
        } else {
            letter.textContent = content.charAt(i);
        }

        word.appendChild(letter);
        letters.push(letter);
    }
    wordArray.push(letters);
};

for (let i = 0; i < words.length; i++) {
    splitLetters(words[i]);
}

const animateLetterOut = (cw, i) => {
    setTimeout(() => {
        cw[i].className = 'letter out';
    }, i * 80);
};

const animateLetterIn = (nw, i) => {
    setTimeout(() => {
        nw[i].className = 'letter in';
    }, 340 + (i * 80));
};

const changeWord = () => {
    let cw = wordArray[currentWord];
    let nextIndex = currentWord === words.length - 1 ? 0 : currentWord + 1;
    let nw = wordArray[nextIndex];

    for (let i = 0; i < cw.length; i++) {
        animateLetterOut(cw, i);
    }

    for (let i = 0; i < nw.length; i++) {
        nw[i].className = 'letter behind';
        nw[0].parentElement.style.opacity = 1;
        animateLetterIn(nw, i);
    }

    currentWord = nextIndex;
};

setInterval(changeWord, 4000);

});
      "))
      ),
      
      # Styling
      tags$style(HTML("
@import url('https://fonts.googleapis.com/css?family=Open+Sans:600');

body {
  font-family: 'Open Sans', sans-serif;
  font-weight: 600;
  font-size: 2rem;
  display: grid;
  place-items: center;
  min-height: 100vh;
  margin: 0;
  background-color: #00070d;
}

.word {
  position: absolute;
  opacity: 0;
  left: 50%;
  transform: translateX(-50%);
  text-transform: uppercase;
}

.wisteria { color: #8e44ad; }
.belize { color: #2980b9; }
.pomegranate { color: #ffffff; }

.letter {
  display: inline-block;
  transform-origin: 50% 50% 25px;
}

.letter.out {
  transform: rotateX(90deg);
  transition: transform 0.32s cubic-bezier(0.55, 0.055, 0.675, 0.19);
}

.letter.behind {
  transform: rotateX(-90deg);
}

.letter.in {
  transform: rotateX(0deg);
  transition: transform 0.38s cubic-bezier(0.175, 0.885, 0.32, 1.275);
}
    "))
  )
}


ui <- fluidPage(
  animate_text()
)
server <- function(input, output) {}
# shinyApp(ui, server)
