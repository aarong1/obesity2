library(tidyverse)

library(shiny)
library(bslib)
'red' #ff4741


circular_value <- function(value=15e3,col = 'default'){

labels = c('below cost effective', 
  'lower threshold cost effective',
  'upper threshold cost effective',
  'cost effective'
  )

below_values = c(20000,25000,30000,1e8)

colours = c('lightgreen','yellaow','orange', '#ff4741')

col_ring <- colours[as.numeric(sub(x = value, pattern= ',', replacement = ''))<below_values][1]

div(
  HTML(paste0('<head><style>
  #inner{
  border:solid 20px ',col_ring,'; /* lightgreen */
  border-radius:50%;
  transform: scale(1);
  padding-top:20px;
  width:100%;
  height:100%;
    transition: transform 1s ease-in-out;

  }
  #inner:hover {
    transform: scale(1.4);
          transition: transform 0.5s ease-in-out;

    
  #content:hover {
    transform: none !important;
    
  </style>
       </head>')),
div(
  div(id='outer',style ='margin:10px;width:100%;height:100%;border:solid 20px cyan;border-radius:50%;aspect-ratio: 1 / 1;', ##13b5cb

div(id ='inner',
     div( id='content',
     style ='padding-top:20px;

border-radius:50%;

font-weight:bold;

    display:flex;

    align-items:center;

    align-content:center;
    
    justify-content: space-around;

    flex-direction:column;

    gap:-1rem;',

    
#     div(style='text-align:center;',
#         p('ICER'),
# p(style = 'font-size:10px;','Incremental Cost Effectiveness Ratio')) ,

    
    div(style='text-align:center;margin-top:-10px;',
        p(style = 'display:inline-block;font-size:15px;',
          div(id='myTargetElement', style = 'display:inline-block;width:95%;font-size:2rem',value), 
          # p(style = ';',''),
          p(style = 'display:inline-block;','£ / QALY'),
          br(),
          p(class='badge bg-info text-white','over 10 years')))),

# div(style='text-align:center;',
# )
))
)
)
}

browsable(page_fluid(
circular_value('40,212')
))

