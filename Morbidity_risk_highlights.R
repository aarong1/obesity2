library(shiny)
library(bslib)

# Original prototype preserved for reference/reuse.
original_stroke_prototype_html <- '
<h2 class = d-inline> Stroke </h2>

<i class="fa-solid fa-burst mx-5 mb-2" style="color: rgb(200, 0, 0);"></i>

<div class="d-flex flex-wrap gap-2 align-items-center small">

<br>
  <span class="fw-bold text-dark"
        data-bs-toggle="tooltip"
        title="Body Mass Index">
    BMI
  </span>

  <span class="fw-bold text-muted"
        data-bs-toggle="tooltip"
        title="Smoking Status">
    SMK
  </span>

  <span class="fw-bold text-dark"
        data-bs-toggle="tooltip"
        title="Alcohol Consumption">
    ALC
  </span>

  <span class="fw-bold text-dark"
        data-bs-toggle="tooltip"
        title="Physical Activity">
    PA
  </span>

  <span class="fw-bold text-light"
        data-bs-toggle="tooltip"
        title="Raised Cholesterol">
    CHL
  </span>

  <span class="fw-bold text-dark"
        data-bs-toggle="tooltip"
        title="Hypertension">
    HYP
  </span>

  <span class="fw-bold text-dark"
        data-bs-toggle="tooltip"
        title="Type 2 Diabetes">
    T2D
  </span>

  <span class="fw-bold text-light"
        data-bs-toggle="tooltip"
        title="Atrial Fibrillation">
    AF
  </span>

  <span class="fw-bold text-dark"
        data-bs-toggle="tooltip"
        title="Chronic Kidney Disease">
    CKD
  </span>

  </div>

  <div class="d-flex flex-wrap gap-2 align-items-center small">

    <span class="fw-bold text-light"
        data-bs-toggle="tooltip"
        title="Pollution 2.5 microns">
    PM2.5
  </span>

  <span class="text-dark"
        data-bs-toggle="tooltip"
        title="Obstructive Sleep Apnea">
    OSA
  </span>

     <span class="fw-bold text-light"
        data-bs-toggle="tooltip"
        title="Venuous Thromboembelism">
    VTE
  </span>

  <span class="text-dark"
        data-bs-toggle="tooltip"
        title="Peripheral Artery Disease">
    PAD
  </span>

  <span class="fw-bold text-dark"
        data-bs-toggle="tooltip"
        title="Depression">
    DEP
  </span>

</div>

<script>
document.querySelectorAll("[data-bs-toggle=\"tooltip\"]").forEach(el => {
  new bootstrap.Tooltip(el)
})
</script>
'

resolve_matrix_path <- function() {
  candidates <- c(
    "disease_engines/disease_risk_factor_matrix.csv",
    "../disease_engines/disease_risk_factor_matrix.csv"
  )
  existing <- candidates[file.exists(candidates)]
  if (length(existing) == 0) {
    stop("Could not find disease_risk_factor_matrix.csv")
  }
  existing[[1]]
}

risk_matrix <- read.csv(resolve_matrix_path(), stringsAsFactors = FALSE)

risk_cols <- c(
  "bmi", "smoking", "alcohol", "physical_activity", "hypertension",
  "diabetes", "cholesterol", "pm25", "sleep", "ethnicity"
)

risk_labels <- c(
  bmi = "BMI",
  smoking = "SMK",
  alcohol = "ALC",
  physical_activity = "PA",
  hypertension = "HYP",
  diabetes = "T2D",
  cholesterol = "CHL",
  pm25 = "PM2.5",
  sleep = "OSA",
  ethnicity = "ETH",
  age_sex_only = 'AGE'
)

fatality_diseases <- c(
  "chd",
  "stroke",
  "heart_failure",
  "diabetes",
  "chronic_kidney_disease",
  "dementia",
  "asthma",
  "copd",
  "lung_cancer",
  "colorectal_cancer",
  "prostate_cancer",
  "female_breast_cancer",
  "oral_cancer",
  "pancreatic_cancer",
  "uterine_cancer",
  "ovarian_cancer",
  "kidney_cancer"
)

explicitly_modelled_death <- fatality_diseases


# Map model disease names to matrix disease names where they differ.
disease_lookup <- c(
  diabetes = "diabetes_type_2",
  chronic_kidney_disease = "kidney_disease"
)

map_to_matrix_disease <- function(x) {
  if (x %in% names(disease_lookup)) disease_lookup[[x]] else x
}

prettify_disease <- function(x) {
  tools::toTitleCase(gsub("_", " ", x, fixed = TRUE))
}

y <- risk_matrix %>% #filter(row_number()<4) %>% 
  
  pivot_longer(-c(1,2,3,4,'notes')) %>% 
  filter(name!= 'ehtnicity') %>% 
  mutate(name = recode(name, !!!risk_labels)) %>% 
  # filter(value != 0) %>% 
  group_by(disease, disease_pretty_name) %>%
  summarise(morbidity_class=
              first(morbidity_class),
            risks = paste(ifelse(value==1,   
                                 paste0('<span class="fw-semibold text-dark" 
                  data-bs-toggle="tooltip"
                  title="Body Mass Index">',
                                        name,
                                        '</span>'),
                                 #text-light  
                                 paste0('<span class="fw-semibold text-default"
                  data-bs-toggle="tooltip"
                  title="Body Mass Index">',
                                        name,
                                        '</span>')),
                          
                          collapse = "  ") ) %>% 
  mutate(risks = paste('<span> <h5 class = "fw-bold">', disease_pretty_name, '</h5></span>',
                       '<span> <h5 class = "lead">', morbidity_class,  '</h5></span>',
                       ifelse(disease %in% fatality_diseases,
                              '<i class="fa-solid fa-burst position-absolute top-0 end-0 m-3" style="color: rgb(200, 0, 0);"></i></br>',
                              '</br>'),
                       risks)
  ) %>% 
  mutate(r = c(disease = risks))

n <- y$disease
y[['r']]

y = y[['r']]

names(y) = prettify_disease(n) #[1:3]

# sapply(FUN = function(x){
#       list(
#         # print(x)
#         x[[1]]
#         )}); y


# pull(risks) %>%
# purrr::pluck(1) %>%
# HTML() %>% 
# page_fluid() %>%
# browsable()


ui <- page_fluid(
  theme = bs_theme(version = 5,font_scale = 0.7),
  tags$head(
    
  ),
  tags$div(
    class = "container py-3",
    h4("Risk Highlights"),
    selectizeInput(
      inputId = "disease",
      label = "Disease",
      choices = character(0),
      selected = NULL,
      options = list(
        valueField = "email",
        labelField = "name",
        render = I("{
    item: function(item, escape) {
      console.log(item);
      var name = item.email ? '<span class=\"name\">' + item.email + '</span>' : '';
      return '<div class =  m-2 p-2>' + '<span class=\"email m-2\">' + item.name + '</span></div>';
    },
    option: function(item, escape) {
      var label = item.name || item.email;
      var caption = item.name ? item.email : null;
      return '<div class =  \"m-2 p-2 rounded-3 \">' +
       (caption ? '<div class=\"label position-relative\">' + item.email + '</div>' : '') 
        
      '</div>';
    }
  }") 
      )),
    icon('globe',class='visually-hidden'),
    tags$div(class = "mt-2 mb-2", textOutput("selected_disease")),
    uiOutput("risk_ui")
  )
)

server <- function(input, output, session) {
  output$selected_disease <- renderText({
    req(input$disease)
    
    print(paste("Selected:", input$disease))
    
    paste("Selected:", input$disease)
  })
  
  observeEvent(TRUE, {
    choice_labels <- vapply(fatality_diseases, function(d) {
      has_icon <- d %in% explicitly_modelled_death
      icon_html <- if (has_icon) {
        "<i class='fa-solid fa-burst ms-2' style='color: rgb(200, 0, 0);'></i>"
      } else {
        ""
      }
      paste0("<span>", d, icon_html, "</span>")
    }, character(1))
    
    #   updateSelectizeInput(
    #     session = session,
    #     inputId = "disease",
    #     choices = setNames(fatality_diseases, choice_labels),
    #     selected = "stroke",
    #     server = FALSE
    #   )
    # }, once = TRUE)
    
    updateSelectizeInput(session, 
                         inputId='disease', 
                         selected = 'stroke',
                         choices = y
                         # choices = pop %>%
                         #   count(age20) %>%
                         #   t() %>%
                         #   as.data.frame() %>%
                         #   setNames(format(
                         #     big.mark=",",
                         #     as.numeric(.[2,])*10 #model_specification$population$scale_down_factor
                         #   )) %>%
                         #   sapply(FUN = function(x){
                         #     list(
                         #       x[[1]]
                         #       )})
    )
  })
  
  output$risk_ui <- renderUI({
    req(input$disease)
    
    matrix_disease <- map_to_matrix_disease(input$disease)
    row <- risk_matrix[risk_matrix$disease == matrix_disease, , drop = FALSE]
    
    if (nrow(row) == 0) {
      return(div(class = "alert alert-warning", "No matrix row found for selected disease."))
    }
    
    button_nodes <- lapply(risk_cols, function(rc) {
      is_on <- identical(as.integer(row[[rc]][1]), 1L)
      btn_class <- if (is_on) "btn btn-primary disabled active" else "btn btn-outline-secondary disabled"
      tags$label(
        class = btn_class,
        `aria-disabled` = "true",
        risk_labels[[rc]]
      )
    })
    
    div(
      tags$div(
        class = "mb-2",
        tags$span(class = "fw-semibold", prettify_disease(input$disease))
      ),
      tags$div(
        class = "btn-group flex-wrap gap-1",
        role = "group",
        `aria-label` = "Risk factors",
        button_nodes
      ),
      if (identical(as.integer(row$age_sex_only[1]), 1L)) {
        tags$div(class = "mt-2", tags$span(class = "badge bg-secondary", "Age/Sex only model"))
      }
    )
  })
}

shinyApp(ui, server)

