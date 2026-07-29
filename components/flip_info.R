library(shiny)

# tibble(no = 1:7,
#        title1
#        title2
#        blurb

risk_factor_carousel <- function(id = "carouselExample") {
  tags$div(
    id = id,
    class = "carousel slide",
    `data-bs-ride` = "carousel",
    style = "padding: 10px; height: 100%; transform: rotateX(0deg) rotateY(0deg) scale3d(1, 1, 1);",
    
    # Indicators --------------------------------------------------------------
    tags$div(
      class = "carousel-indicators",
      tags$button(
        type = "button", `data-bs-target` = paste0("#", id),
        `data-bs-slide-to` = "0", class = "active",
        `aria-current` = "true", `aria-label` = "Slide 1"
      ),
      tags$button(
        type = "button", `data-bs-target` = paste0("#", id),
        `data-bs-slide-to` = "1", `aria-label` = "Slide 2"
      ),
      tags$button(
        type = "button", `data-bs-target` = paste0("#", id),
        `data-bs-slide-to` = "2", `aria-label` = "Slide 3"
      ),
      tags$button(
        type = "button", `data-bs-target` = paste0("#", id),
        `data-bs-slide-to` = "3", `aria-label` = "Slide 4"
      ),
      tags$button(
        type = "button", `data-bs-target` = paste0("#", id),
        `data-bs-slide-to` = "4", `aria-label` = "Slide 5"
      ),
      tags$button(
        type = "button", `data-bs-target` = paste0("#", id),
        `data-bs-slide-to` = "5", `aria-label` = "Slide 6"
      ),
      tags$button(
        type = "button", `data-bs-target` = paste0("#", id),
        `data-bs-slide-to` = "6", `aria-label` = "Slide 7"
      ),
      tags$button(
        type = "button", `data-bs-target` = paste0("#", id),
        `data-bs-slide-to` = "7", `aria-label` = "Slide 8"
      ),
      tags$button(
        type = "button", `data-bs-target` = paste0("#", id),
        `data-bs-slide-to` = "8", `aria-label` = "Slide 9"
      ),
      tags$button(
        type = "button", `data-bs-target` = paste0("#", id),
        `data-bs-slide-to` = "9", `aria-label` = "Slide 10"
      ),
      tags$button(
        type = "button", `data-bs-target` = paste0("#", id),
        `data-bs-slide-to` = "10", `aria-label` = "Slide 11"
      ),
      tags$button(
        type = "button", `data-bs-target` = paste0("#", id),
        `data-bs-slide-to` = "11", `aria-label` = "Slide 12"
      )
    ),
    
    # Slides ------------------------------------------------------------------
    tags$div(
      class = "carousel-inner h-100 p-30",
      
      # 1) Demographics -------------------------------------------------------
      tags$div(
        class = "carousel-item active",
        tags$div(
          class = "d-block w-100 text-white p-3",
          style = "height: 420px;background-color:royalblue;border:solid white 1px;border-radius:10px;",
          tags$h5("Demographics"),
          tags$p("Demographics are largely fixed. Nisra Estimates provides estimations for areas now, and into the future"),
          tags$a(
            href = "https://www.nisra.gov.uk/publications/2022-mid-year-population-estimates-northern-ireland",
            "NISRA mid-year estimates"
          ),
          tags$br(),
          tags$a(
            href = "https://www.nisra.gov.uk/publications/2022-mid-year-population-estimates-small-geographical-areas",
            "NISRA mid-year estimates for small geographical ares"
          ),
          tags$br(),
          tags$a(
            href = "https://www.nisra.gov.uk/publications/2018-based-population-projections-areas-within-northern-ireland",
            "Projections"
          )
        )
      ),
      
      # 2) Demographics / Inequality -----------------------------------------
      tags$div(
        class = "carousel-item",
        tags$div(
          class = "d-block w-100 text-white p-3",
          style = "height: 420px;background-color:royalblue;border:solid white 1px;border-radius:10px;",
          tags$h5("Demographics"),
          tags$h5("Inequality"),
          tags$p(
            "Both deprivation and gender are the most considered measures of inequality.
             Regional inequality is not considered, only in the sense that a regional is considered as characterised exclusively as a function of its demographic make up.
             It is in this context that spatial delineation of results are prepared."
          ),
          tags$h5("Source"),
          tags$a(
            href = "https://connect.strategyunitwm.nhs.uk/content/b05b649b-511d-4921-9069-2a7a62d694dd/",
            "NHS Strategy Unit"
          )
        )
      ),
      
      # 3) Behavioural – BMI --------------------------------------------------
      tags$div(
        class = "carousel-item",
        tags$div(
          class = "d-block w-100 text-white p-3",
          style = "height:420px;background-color:mediumseagreen;border:solid white 1px;border-radius:10px;",
          tags$h5("Behavioural factors"),
          tags$h5("BMI"),
          tags$p(
            "This is binary categorised as at-elevated risk and below elevated-risk either side of a measurement of 25 Kgm-2. Underweight conditions are not considered.
             As diet and alcohol are not considered explicitly, possible scenarios targeting BMI include these interventions, mimicking gender differences and deprivation inequality in prevalence.
             Additionally, weight loss pharmeceuticals are also topical."
          ),
          tags$a(
            href = "https://www.health-ni.gov.uk/news/health-survey-ni-first-results-201920",
            "DoH Health Survey"
          )
        )
      ),
      
      # 4) Behavioural – Smoking ---------------------------------------------
      tags$div(
        class = "carousel-item",
        tags$div(
          class = "d-block w-100 text-white p-3",
          style = "height:420px;background-color:lightgreen;border:solid white 1px;border-radius:10px;",
          tags$h5("Behavioural factors"),
          tags$a(
            href = "https://www.health-ni.gov.uk/news/health-survey-ni-first-results-201920",
            "DoH Health Survey"
          ),
          tags$h5("Smoking"),
          tags$p(
            "Smokers are similarly bagged into smoking and non smoking.
             An additional state 'used to smoke' represents the dimished, but not vanishing risk, to cardiovascular complication.
             Possible interventions are smoking cessation programmes with particular focus on elevated prevalence among deprived areas,
             regression and programme adherence, and extending to consider vaping."
          )
        )
      ),
      
      # 5) Physiological – general -------------------------------------------
      tags$div(
        class = "carousel-item",
        tags$div(
          class = "d-block w-100 text-white p-3",
          style = "height: 420px; background-color:orangered;border:solid white 1px;border-radius:10px;",
          tags$h5("Physiological"),
          tags$p(
            "Physiological risk factors are strongly demographic and risk factor dependent.
             Data is less regularly captured and while some have associated mortality they are not always
             the reason for primary care contact and as such are not recorded accurately. As opposed to the survey data for behavioural risk factors then
             additional considerations are that."
          )
        )
      ),
      
      # 6) Physiological – Diabetes ------------------------------------------
      tags$div(
        class = "carousel-item",
        tags$div(
          class = "d-block w-100 text-white p-3",
          style = "height: 420px; background-color:orange;border:solid white 1px;border-radius:10px;",
          tags$h5("Physiological"),
          tags$h5("Diabetes,type 2"),
          tags$p("Diabetes type 2")
        )
      ),
      
      # 7) Physiological – Hypertension --------------------------------------
      tags$div(
        class = "carousel-item",
        tags$div(
          class = "d-block w-100 text-white p-3",
          style = "height: 420px; background-color:orange;border:solid white 1px;border-radius:10px;",
          tags$h5("Physiological"),
          tags$h5("Hypertension"),
          tags$p("")
        )
      ),
      tags$div(
        class = "carousel-item",
        tags$div(
          class = "d-block w-100 text-white p-3",
          style = "height: 420px; background-color:rgb(var('--bs-rgb-info'));border:solid white 1px;border-radius:10px;",
          tags$h5("Physiological / Demographic"),
          tags$h5("Pregnancy"),
          tags$p(
            "Pregnancy has serious implications for the cardiovascular health of the mother and potentially the child. These are considered outside a 
            generalised framework and on a more case by case basis. e.g. Gestational Diabetes."
          )
        )
      ),
      # 8) Physiological – Cholesterol ---------------------------------------
      tags$div(
        class = "carousel-item",
        tags$div(
          class = "d-block w-100 text-white p-3",
          style = "height: 420px; background-color:orange;border:solid white 1px;border-radius:10px;",
          tags$h5("Physiological"),
          tags$h5("Peripheral Arterial Disease"),
          tags$p(
            "PAD is a disease in its own right affecting the peripherla vascular system. It also a risk factor for various serious CVD. "
          )
        )
      ),
      tags$div(
        class = "carousel-item",
        tags$div(
          class = "d-block w-100 text-white p-3",
          style = "height: 420px; background-color:orange;border:solid white 1px;border-radius:10px;",
          tags$h5("Physiological"),
          tags$h5("Chronic Kidney Disease"),
          tags$p(
            "Kidney Disease incidence and its progression is considered separately as a renal affliction progressing to possible dialysis and transplant, possibly death.
            It is included in risks for its considerable influence on the cardiovascular system. "
          )
        )
      ),
      tags$div(
        class = "carousel-item",
        tags$div(
          class = "d-block w-100 text-white p-3",
          style = "height: 420px; background-color:orange;border:solid white 1px;border-radius:10px;",
          tags$h5("Physiological"),
          tags$h5("Atrial fibrillation"),
          tags$p(
            "Atrial Fibrillation is a risk factor for serious CVD particularly cardiological disease."
          )
        )
      ),
      tags$div(
        class = "carousel-item",
        tags$div(
          class = "d-block w-100 text-white p-3",
          style = "height: 420px; background-color:orange;border:solid white 1px;border-radius:10px;",
          tags$h5("Physiological"),
          tags$h5("Venous Thromboembelism"),
          tags$p(
            "VTE is a morbidity in its own right, but also strongly risked for various serious CVD diseases. 
            
            Here it covers both Deep Vein Thrombosis and Pulmonary Embelism collectively"
          )
        )
      )
    ),
    
    # Controls ---------------------------------------------------------------
    tags$button(
      class = "carousel-control-prev",
      type  = "button",
      `data-bs-target` = paste0("#", id),
      `data-bs-slide`  = "prev",
      span(class = "carousel-control-prev-icon", `aria-hidden` = "true"),
      span(class = "visually-hidden", "Previous")
    ),
    tags$button(
      class = "carousel-control-next",
      type  = "button",
      `data-bs-target` = paste0("#", id),
      `data-bs-slide`  = "next",
      span(class = "carousel-control-next-icon", `aria-hidden` = "true"),
      span(class = "visually-hidden", "Next")
    )
  )
}

ui <- page_fluid(
  # If not already using BS5 via bslib, you may want to ensure Bootstrap 5 is loaded.
  risk_factor_carousel()
)

server <- \(input, output, session) {}

# shinyApp(ui, server)