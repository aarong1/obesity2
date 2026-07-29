library(shiny)
library(bslib)
tiny_circular_value <- function(colour='lightgreen'){
  t <- tagList( 

  div(id='outer', style ='width:40px;height:40px;border:solid 7px #13b5cb;border-radius:50%;',
div(id ='inner', style = paste('border:solid 7px ',colour,';
  border-radius:50%;
  transform: scale(0.99);
  width:100%;height:100%;')
    )
)
)
  # print(t)
  return(t)
}



colours= c('lightgreen','yellow','#FDAA48', '#ff4741')


page_fluid(
  div(class='d-flex gap-3 m-3',
tiny_circular_value(colour='lightgreen'),
tiny_circular_value(colour='#ffed29'),
tiny_circular_value(colour='#FDAA48'),
tiny_circular_value(colour='#ff4741')


  
  
)
)

