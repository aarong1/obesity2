carousel <- function(id = NULL){

if (!is.character(id)) {
    stop("Error: 'id' argument cannot be NULL and must be a character.")
}
  
  tags$div(id=id, class="carousel slide", style = 'padding:10px;height:100%;', 

    # Indicators for slides
    tags$div(class="carousel-indicators",
      tags$button(type = "button", `data-bs-target`=paste0("#",id), `data-bs-slide-to`="0", class="active", `aria-current`="true", `aria-label`="Slide 1"),
      tags$button(type = "button", `data-bs-target`=paste0("#",id), `data-bs-slide-to`="1", `aria-label`="Slide 2"),
      tags$button(type = "button", `data-bs-target`=paste0("#",id), `data-bs-slide-to`="2", `aria-label`="Slide 3"),
      tags$button(type = "button", `data-bs-target`=paste0("#",id), `data-bs-slide-to`="3", `aria-label`="Slide 3"),
      tags$button(type = "button", `data-bs-target`=paste0("#",id), `data-bs-slide-to`="4", `aria-label`="Slide 3"),
      tags$button(type = "button", `data-bs-target`=paste0("#",id), `data-bs-slide-to`="5", `aria-label`="Slide 3"),
      tags$button(type = "button", `data-bs-target`=paste0("#",id), `data-bs-slide-to`="6", `aria-label`="Slide 3"),
      tags$button(type = "button", `data-bs-target`=paste0("#",id), `data-bs-slide-to`="7", `aria-label`="Slide 3"),
      tags$button(type = "button", `data-bs-target`=paste0("#",id), `data-bs-slide-to`="8", `aria-label`="Slide 3"),


    ),
    
    # Carousel items (add custom content here)
    tags$div(class="carousel-inner",class='h-100 p-30',
      
      # First slide (with text content)
      tags$div(class="carousel-item active", 
        tags$div(class="d-block w-100 text-white p-3",style="height: 420px;background-color:royalblue;border:solid white 1px;border-radius:10px;",
          tags$h5("Demographics"),
          tags$p("Demographics are largely fixed. Nisra Estimates provides estimations for areas now, and into the future"),
          #tags$h5('Source'),
          tags$a(href='https://www.nisra.gov.uk/publications/2022-mid-year-population-estimates-northern-ireland',
                 "NISRA mid-year estimates"),
          br(),
          tags$a(href='https://www.nisra.gov.uk/publications/2022-mid-year-population-estimates-small-geographical-areas',
                 "NISRA mid-year estimates for small geographical ares"),
          br(),
          tags$a(href = "https://www.nisra.gov.uk/publications/2018-based-population-projections-areas-within-northern-ireland",
                 "Projections")
          )
      ),
      
      tags$div(class="carousel-item", 
        tags$div(class="d-block w-100 text-white p-3",style="height: 420px;background-color:royalblue;border:solid white 1px;border-radius:10px;",
          tags$h5("Demographics"),
          tags$h5('Inequality'),

          tags$p("Both deprivation and gender are the most considered measures of inequality.
                 Regional inequality is not considered, only in the sense that a regional is considered as characterised exclusively as a function of its demographic make up.
                 It is in this context that spatial delineation of results are prepared."),
          tags$h5('Source'),
          tags$a(href='https://connect.strategyunitwm.nhs.uk/content/b05b649b-511d-4921-9069-2a7a62d694dd/',
                 "NHS Strategy Unit"),

          )
      ),
      
      # Second slide (with a table)
      tags$div(class="carousel-item",
        tags$div(class="d-block w-100 text-white p-3", style="height:420px;background-color:lightgreen;border:solid white 1px;border-radius:10px;",
          tags$h5("Behavioural factors"),
          tags$h5("BMI"),
         tags$p("This is binary categorised as at-elevated risk and below elevated-risk either side of a measurement of 25 Kgm-2. Underweight conditions are not considered.
                As diet and alcohol are not considered explicitly, possible scenarios targeting BMI include these
                interventions, mimicking gender differences and deprivation inequality in prevalence. 
                Additionally, weight loss pharmeceuticals are also topical."),
         tags$a(href = 'https://www.health-ni.gov.uk/news/health-survey-ni-first-results-201920',
                "DoH Health Survey"),
       
      )
      ),
      
       # Second slide (with a table)
      tags$div(class="carousel-item",
        tags$div(class="d-block w-100 text-white p-3", style="height:420px;background-color:lightgreen;border:solid white 1px;border-radius:10px;",
          tags$h5("Behavioural factors"),
         tags$a(href = 'https://www.health-ni.gov.uk/news/health-survey-ni-first-results-201920',
                "DoH Health Survey"),
         tags$h5("Smoking"),
         tags$p("Smokers are similarly bagged into smoking and non smoking.
                An additional state 'used to smoke' represents the dimished, but not vanishing risk, to cardiovascular complication.
                Possible interventions are smoking cessation programmes with particular focus on elevated prevalence among deprived areas, 
                regression and programme adherence, and extending to consider vaping")

        )
      ),
      
       # Third slide (with a plot)
      tags$div(class="carousel-item",
        tags$div(class="d-block w-100 text-white p-3", style="height: 420px; background-color:orange;border:solid white 1px;border-radius:10px;",
          tags$h5("Physiological"),
         
         tags$p('Physiological risk factors are strongly demographic and risk factor dependent.
                Data is less regularly captured and while some have associated mortality they are not always
                the reason for primary care contact and as such are not recorded accurately. As opposed to the survey data for behavioural risk factors then
                additional considerations are that ')
        )
      ),
      
      # Third slide (with a plot)
      tags$div(class="carousel-item",
        tags$div(class="d-block w-100 text-white p-3", style="height: 420px; background-color:orange;border:solid white 1px;border-radius:10px;",
          tags$h5("Physiological"),
         tags$h5("Diabetes,type 2"),
         tags$p('Diabetes type 2')
        )
      ),
      
       # Third slide (with a plot)
      tags$div(class="carousel-item",
        tags$div(class="d-block w-100 text-white p-3", style="height: 420px; background-color:orange;border:solid white 1px;border-radius:10px;",
          tags$h5("Physiological"),
         tags$h5("Hypertension"),
         tags$p("")
        )
      ),
      
       # Third slide (with a plot)
      tags$div(class="carousel-item",
        tags$div(class="d-block w-100 text-white p-3", style="height: 420px; background-color:orange;border:solid white 1px;border-radius:10px;",
          tags$h5("Physiological"),
         tags$h5("Cholesterol"),
         tags$p('Cholesterol is a risk factor for various serious CVD. Here it largely refers to the ')
        )
      )
    ),
    
    # Carousel controls
    tags$button(class="carousel-control-prev", type="button", `data-bs-target`=paste0("#",id), `data-bs-slide`="prev",
      #tags$span(class="carousel-control-prev-icon", `aria-hidden`="true"),
      tags$span(class="visually-hidden", "Previous")
    ),
    tags$button(class="carousel-control-next", type="button", `data-bs-target`=paste0("#",id), `data-bs-slide`="next",
      #tags$span(class="carousel-control-next-icon", `aria-hidden`="true"),
      tags$span(class="visually-hidden", "Next")
    )
  )
  }
