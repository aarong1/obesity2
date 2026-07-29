# https://css-tricks.com/snippets/css/apple-com-hamburger-bun-menu/
  
library(tidyverse)
library(shiny)
library(htmltools)

div(
tags$head(
tags$script(src = "https://code.jquery.com/jquery-3.6.0.min.js"),
tags$script(HTML(
"$('.hamburger').click (function(){
  $(this).toggleClass('open');
});
")),
tags$style(
("
.hamburger {
  cursor: pointer;
  position: absolute;
  width: 48px;
  height: 48px;
  transition: all 0.25s;
}

.hamburger__top-bun,
.hamburger__bottom-bun {
  content: '';
  display: block;
  position: absolute;
  left: 15px;
  width: 18px;
  height: 1px;
  background: #eee;
    transform: rotate(0);
  transition: all 0.25s;
}

.hamburger:hover [class*='-bun'] {
  background: #999;
}

.hamburger__top-bun {
  top: 23px;
  transform: translateY(-3px);
}

.hamburger__bottom-bun {
  bottom: 23px;
  transform: translateY(3px);
}

.open {
  transform: rotate(90deg);
}

.open .hamburger__top-bun {
  transform: 
    rotate(45deg) 
  translateY(0px);
}

.open .hamburger__bottom-bun {
  transform: 
    rotate(-45deg) 
  translateY(0px);
} ")
)
),


HTML('
<div class="hamburger">
  <span class="hamburger__top-bun"></span>
  <span class="hamburger__bottom-bun"></span>
  </div>')
) %>% browsable()
