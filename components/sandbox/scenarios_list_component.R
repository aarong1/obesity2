library(DT)
library(shiny)
library(tidyverse)
library(sparkline)
x <- read.csv(textConnection("Name,Email,Age,Gender,Hypertension_Selected,Cholesterol_Selected,Diabetes_Selected,Affects_Hypertension,Affects_Cholesterol,Affects_Diabetes,Timestamp,Treatment_Comment,Model_Run_Name,Result
Margaret Arellano,richard.stewart@hscni.net,40-49,Other,True,True,True,True,True,False,2024-09-21,Medication adjustments for hypertension,Cardio_Model_Run_1,4
Charles Robinson,tyler.bright@hscni.net,60-69,Other,False,False,False,True,False,True,2024-09-22,Lifestyle interventions,Cardio_Model_Run_2,5
Michael Nguyen,deanna.norris@hscni.net,30-39,Female,False,True,False,False,True,False,2024-09-13,Lipid-lowering drugs,Cardio_Model_Run_3,2
Robert Boyer,michelle.bradley@hscni.net,60-69,Female,False,False,True,True,True,True,2024-09-10,Glucose management for diabetes,Cardio_Model_Run_4,3
Jade Curry,dana.hernandez@hscni.net,60-69,Other,True,True,False,True,True,False,2024-09-17,Medication adjustments for hypertension,Cardio_Model_Run_5,7
Gregory Wilkins,chelsea.reed@hscni.net,20-29,Male,True,True,False,True,False,False,2024-09-12,Medication adjustments for hypertension,Cardio_Model_Run_6,2
Carla Fernandez,emily.freeman@hscni.net,30-39,Male,True,True,False,True,True,True,2024-09-10,Medication adjustments for hypertension,Cardio_Model_Run_7,3
Daniel Chavez,alexis.day@hscni.net,40-49,Female,False,False,False,False,True,False,2024-09-27,Lifestyle interventions,Cardio_Model_Run_8,3
Mary Thomas,david.chen@hscni.net,60-69,Other,True,True,False,True,True,True,2024-09-11,Medication adjustments for hypertension,Cardio_Model_Run_9,4
Erin Smith,maria.bradley@hscni.net,80-89,Female,True,True,True,True,False,False,2024-09-16,Medication adjustments for hypertension,Cardio_Model_Run_10,5")) 

x

y <- x%>%
  rowwise() %>% 
  mutate(display=paste(h4(Name),p(Email))) %>% 
  rownames_to_column(var = 'model_run') %>% 
  select(!contains('selected')) %>% 
  mutate(display=paste(display,
                       tags$span(class="badge rounded-pill text-bg-primary p-2 m-2",'hypertension'),
                       tags$span(class='badge rounded-pill text-bg-warning d-inline p-2','cholesterol'),
                       tags$span(class='badge rounded-pill text-bg-secondary d-inline p-2',Age))) %>% 
  mutate(display = paste("<div class='p-2 hv d-inline'>", display, "</div>"))  %>%
  #select(display) #%>% 
  mutate(model_run = paste(h4('Model'), h2(class='d-inline',model_run))) %>% 
  mutate(spk = sparkline::spk_chr(1:10,lineColor= 'green'))
  #mutate(display = as.character(display))
  
  #pivot_longer(names_to = 'params', cols = contains('Affects')) %>% 
  # group_by(display) %>% 
  # ungroup() %>% 
  # summarise(
  #   paste(
  #     display, 
  #     paste0(collapse = ' ',
  #       tags$div(
  #         class='btn btn-primary', params))))

ui <- fluidPage(
  tags$style('
  .hv{margin:15px;}
  
  tr { display:block;
  padding:15px !important;
  margin:15px !important;  
  transition: all 0.5s ease 0s;
  }
  
  tr:hover {
   border-radius: 10px;
   transform:scale(1.05);
     padding:10px !important;

  box-shadow: 4px 4px 10px #bebebe, -4px -4px 10px #ffffff;

  transition: all 0.5s ease 0s;}'),
  
  datatable(data=y[c('display','model_run','spk')],
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

  ),
  style = "semanticui") %>% spk_add_deps()

)

# shinyApp(ui,function(input, output, session){})

