button_box_shadow <- function(text = 'text',
                              border = 'red',
                              color=' rgba(40,40,40,0.9)',
                              onclick='',
                              href='#'){
  
  div(tags$style(paste0(
   '.button-container{
   text-align:start;
   margin:10px;
   padding:10px;
   }
   
   .retro_button{
   color: ',color,';
   display:inline;
   padding:10px;
   border: solid ',border,' 5px;
   border-radius: 20px;
    -webkit-box-shadow: 7px 16px 0px 1px ',border,';
    -moz-box-shadow: 7px 16px 0px 1px ',border,';
    box-shadow:  2px 2px 0px 1px ',border,';
    
   }
   
   .retro_button:hover {
       color: ',border,';
    -webkit-box-shadow: 7px 16px 0px 1px ',border,';
    -moz-box-shadow: 7px 16px 0px 1px ',border,';
    box-shadow:  1px 1px 0px 1px ',border,';
    transition: color 0.4s;
    
   }
   
    .retro_button:active {
 
    -webkit-box-shadow: 7px 16px 0px 1px ',border,';
    -moz-box-shadow: 7px 16px 0px 1px ',border,';
      box-shadow:  1px 1px 1px 1px ',border,';
    
   }')),
  

  div( class='button-container', onclick=onclick,href=href,
      h4(class='retro_button',text)
      ))
  

 
}

browsable(fluidPage(button_box_shadow()))

button_block <- function(border = 'red',...){
  
  l <-  list(...)
 # print(class(l))
 # print(class(l[[1]]))
 div(

  
div(class='divcontainer',
style =paste0('padding:20px;
   display:flex;
   flex-direction:row;
   justify-content:space-around;
   flex-wrap:wrap;
   margin:15px;
   border: solid ',border,' 2px;
   border-radius: 50px;
   
    -webkit-box-shadow: 7px 16px 0px 1px ',border,';
    -moz-box-shadow: 7px 16px 0px 1px ',border,';
    box-shadow:  7px 16px 0px 1px ',border,';'),

#tagList(l),
as.tags(l)
  
)
 )
  }



browsable(button_block( color='#13b5cb', border = '#13b5cb'))
# button_box_shadow('Reference', border, color, onclick = "window.location='http://google.com';"),
# button_box_shadow('Download png', border, color),
# button_box_shadow('Go to Resources', border, color),
# button_box_shadow('Go to model', border, color)

browsable(button_block(border='#13b5cb',h1('f'),h1('f')))

browsable(button_block( border='#13b5cb',
button_box_shadow(color='#13b5cb',border='#13b5cb','Reference',   onclick = "window.location='http://google.com';"),
button_box_shadow(color='#13b5cb',border='#13b5cb','Download png'),
button_box_shadow(color='#13b5cb',border='#13b5cb','Go to Resources'),
button_box_shadow(color='#13b5cb',border='#13b5cb','Go to model')
))

