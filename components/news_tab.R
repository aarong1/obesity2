news_tab <- function(){
  div( class="container",
  div( style="background-color:red;border: solid black 15px;border-radius:50%;float:left:height:30px;width:30px;") ,
  p('Hello. How are you today?') ,
  span(style ='color:red;','11:00')
)
}


htmltools::browsable(news_tab())

