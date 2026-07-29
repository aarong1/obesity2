cost_component <- function(total = 23443,
                           up = 43154,
                           down = 12451){

div(style ='font-weight:bold;',
  span(style='padding:0px 0px 40px; 0px;',
  p(style = 'display:inline;', 'Returns'), 
    p(style = 'float:right;color:rgb(137,46,34);margin-left:10px;margin-bottom:30px;', total)
  ),
 div(style='padding-left:10px;padding-top:5px;', span(style = 'display:inline;',
    'Cost',
    p(style = 'float:right;',down)
 )
    ),
     div(style='padding-left:10px;padding-top:3px;',
 'Savings',
 p(style = 'float:right;',up) #display:inline
 )
  ) 
}

browsable(cost_component())
