library(DT)
library(shiny)
library(tidyverse)
library(sparkline)

# library(sass)
# 
# sass(
#   input = sass_file("www/scss/custom.scss"),
#   output = "www/css/custom.css"
# )

dot <- function(color='#bbb'){
  
  ifelse(color==1,
         yes = (color='limegreen'),
          ifelse(color==0, 
                 yes = (color='red'), 
                 (color='#bbb'))
         )
  
  div(style=paste0('height: 15px;
  width: 15px;
  background-color: ',color,';  /* #bbb*/
  border-radius: 50%;
  display: inline-block;'))}

htmltools::browsable(dot())

x <- read.csv('./components/sandbox/risk_data/Risk Equations-Table 1.csv')

risk_table_df <- x[c('Pathology','used','study','paper','statistical.model','recommending.body','risk.duration.years.','calculator')] %>% 
  rowwise() %>% 
  mutate(Pathology = 
    #"<h4>Pathology</h4><div style=\"height: 25px;&#10;  width: 25px;&#10;  background-color: #bbb;  /* #bbb*/&#10;  border-radius: 50%;&#10;  display: inline-block;\"></div>"
      as.character(tagList( 
        div(style='width:200px;',
       dot(used),
      h6(style = 'display:inline-block',Pathology) ,
          p(class='d-block px-4 text-muted','CVD'),
      # div(class='d-block',tags$span(style='float:right;',class="p-1 mx-3 d-inline-block badge text-bg-warning pill",
      #         h6(class='m-1 d-inline-block', risk.duration.years.),
      #                                p(class='d-inline-block','year risk horizon')))
      
      )
    )
   )
  ) %>%
  mutate(study = 
           as.character(
             tagList(
               div(style = 'width:250px;',
                   tags$img(src = "www/framingham_heart_study.svg", width = "30px"),
                 h6(style = 'display:inline-block;padding:5px;',study)),
               tags$a(target='_blank', icon(style='padding:5px 5px 5px 35px;',class='fs-5 m-1','calculator'), 'Risk Calculator', href=calculator),
               tags$a(target='_blank', icon(class='fs-5 p-1 m-1','graduation-cap'),'Paper',href=paper) 
               )
                 
             )
         ) %>% 
  mutate(
    paper = as.character(tags$a(target='_blank',icon(class='fs-5 p-1 m-1','graduation-cap'),'Source',href=paper)) )%>% 
  mutate( 
    calculator = as.character(div(tags$a(target='_blank', icon(class='fs-5 p-1 m-1','calculator'), '', href=calculator)))
                 
             ) %>% 
  #mutate(study =  paste(study, paper, calculator)   )%>% 
  mutate(reco = sample(x=c('www/NICE_logo.png','www/acc.png','www/aha.png'),size=1,prob = c(1,1,1),replace = T)) %>% 
  mutate(reco =  as.character(div(style='width:50px;', tags$img(src = reco, width = "55px")))) %>% 
  mutate(horizon  = as.character(
               div(class='px-1 mx-1',#'width:100px;',
                   tags$span(style='float:right;width:100%;',class="p-2 mx-1 d-inline-block badge text-bg-warning pill",
              h6(class='m-1 d-inline-block', risk.duration.years.),
                                     p(class='d-inline-block','year risk horizon'))
               )
              )) %>% 
  mutate(statistical.model=as.character(div(style='width:100px;overflow:hidden;',statistical.model)) ) %>% 
  mutate(comments = as.character(div(style = 'width:30px;', contentEditable="true", "data-text"="Enter text here"))) %>% 


  select(Pathology, study,reco,horizon,statistical.model,comments)#
#


  tags$style('
  #risk_equations_df a:hover{
  translate:scaleX(2);
  margin: 5px !important;
  transition: all 1.5s ease 0.2s;}
  
  #risk_equations_df tr{
  display:block;
    padding:10px !important;
    margin:10px !important;
  }
    
  #risk_equations_df a{
    color:black;
    text-decoration:none;}
    
  #risk_equations_df td{
   padding:10px !important;
    margin:10px !important;
    border:solid lightgrey;
     border-width:0px 0px 1px 0px;
             }'

             )
  
 # '  tr { display:block;
 #  padding:15px !important;
 #  margin:15px !important;
 #  transition: all 1s ease 0s}

 #  tr:hover {
 #   border-radius: 10px;
 #  box-shadow: 4px 4px 10px #bebebe, -4px -4px 10px #ffffff;
 #  padding: 5px !important;
 #  transition: all 0.5s ease 0.2s;}',
  
  ( risk_table_dt <- risk_table_df %>% 

datatable(data = .,
  rownames = FALSE, 
  escape =FALSE,
  height='80vh',
  options = list(
    dom = 't',  # Only show the table, no pagination or search box
    paging = FALSE,  # Disable pagination
    searching = FALSE,  # Disable search box
    ordering = FALSE,  # Disable column sorting
    info = FALSE,  # Disable info ("Showing X of Y entries")
    headerCallback = JS("function(thead, data, start, end, display){ $(thead).remove(); }")  # Remove headers

  ) ,  style = "semanticui")
)
htmltools::browsable(fluidPage(risk_table_dt))
