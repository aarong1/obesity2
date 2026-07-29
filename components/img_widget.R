#https://codepen.io/Jaskaranbir/pen/zKxBmd

htmltools::browsable(
fluidPage(
tags$head(
tags$style('#main {
  width: 200px;
  height: 200px;
}

#img {
  width: 100px;
  height: 100px;
  background-image: url("https://encrypted-tbn1.gstatic.com/images?q=tbn:ANd9GcRM1iZ-UkRX_lgqv1YsBn4QRJtsuKRP45T19LCAKiNy7Jn-0MQLdQS0LzU");
  transition: opacity 1s;
}

#text {
  opacity: 0.3;
  transition: opacity 1s;
}

#img:hover {
  opacity: 0.3;
}

#img:hover + #text {
  opacity: 1;
}')),

div(id="main",
  div( id="img"),
  div( id="text", "Description Text")
)
)
)

