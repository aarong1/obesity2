two_tier_text_box <- function(title = 'Title',
                              text='Longer span of text',
                              border = 'red',
                              color=' rgba(40,40,40,0.9)',
                              onClick = ''){
 fluidPage(
  tags$style(paste0(
   ' #container{
   padding:50px;
   display:flex;
   flex-direction:column;
   gap:25px;

   }
   
   #title{
   color: ',color,';
   display:inline;
   padding:10px;
   margin-left:15px;
   border: solid ',border,' 5px;
   border-radius: 20px;
   
    -webkit-box-shadow: 7px 16px 0px 1px ',border,';
    -moz-box-shadow: 7px 16px 0px 1px ',border,';
    box-shadow:  2px 2px 0px 1px ',border,';
    
   }
   
   #button-container{
   text-align:end;
   }
   
   #button{
   color: ',border,';
   display:inline;
   padding:10px;
   margin-right:15px;
   border: solid ',border,' 5px;
   border-radius: 20px;
   
    -webkit-box-shadow: 7px 16px 0px 1px ',border,';
    -moz-box-shadow: 7px 16px 0px 1px ',border,';
    box-shadow:  2px 2px 0px 1px ',border,';
    
   }
   
   #button:hover {
       color: ',color,';
    -webkit-box-shadow: 7px 16px 0px 1px ',border,';
    -moz-box-shadow: 7px 16px 0px 1px ',border,';
    box-shadow:  1px 1px 0px 1px ',border,';
    transition: color 1s;
    
   }
   
    #button:active {
 
    -webkit-box-shadow: 7px 16px 0px 1px ',border,';
    -moz-box-shadow: 7px 16px 0px 1px ',border,';
   box-shadow:  1px 1px 1px 1px ',border,';
    
   }
   
   #text{  
      color: ',color,';
    padding:10px;
    margin-bottom:8px;
    border: solid ',border,' 5px;
    border-radius:20px;
    -webkit-box-shadow: 7px 16px 0px 1px ',border,';
    -moz-box-shadow: 7px 16px 0px 1px ',border,';
    box-shadow: 4px 8px 0px 1px ',border,';
   }')),
  
div(id='container',
  div( 
      h1(id='title',title)
      ),
  
  div(id='text',
      h4(text)
      ),
  
  div( id='button-container',
      h2(id='button','Enter',onClick=onClick)
      ),
  
)
 )
  }

browsable(two_tier_text_box(onClick="alert('hi');"))

          