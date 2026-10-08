library(htmltools)
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
    "../../disease_engines/disease_risk_factor_matrix.csv"
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
  "kidney_disease",
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
  "renal_cancer"
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
  mutate(disease = case_when(disease == 'chronic_kidney_disease' ~ 'kidney_disease',
                             T ~ disease)) %>% 
  pivot_longer(-c(1,2,3,4,'notes')) %>% 
  filter(name!= 'ethnicity') %>% 
  mutate(name = recode(name, !!!risk_labels)) %>% 
  # filter(value != 0) %>% 
  
  mutate(rn = row_number()) %>%
  group_by(disease,disease_pretty_name) %>%
  mutate(rn = first(rn)) %>%
  group_by(rn,disease,disease_pretty_name) %>%
  summarise(morbidity_class =
                # ifelse(disease %in% fatality_diseases,
                       # paste('<span class = "text-danger">',  first(morbidity_class), '</span>'),
                       first(morbidity_class),
                       # ),
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
                       ifelse(disease %in% fatality_diseases,
                       paste('<span> <p class = "bg-danger text-white rounded-2 fw-bolder px-1 d-inline-block">', morbidity_class,  '</p></span></br>'),
                              # "<i class='fa-solid fa-burst ms-2' style='color: rgb(200, 0, 0);'></i>",
                              # '<i class="fa-solid fa-burst position-relative top-0 end-0 m-3" style="color: rgb(200, 0, 0);"></i></br>',
                              # '</br>'
                       paste('<span> <p class = "bg-cyan text-white rounded-2 fw-bolder px-1 d-inline-block">', morbidity_class,  '</p></span></br>')
                       
                              ),
                       risks)
  ) %>% 
  mutate(risks = paste('<div risk-value = ', disease ,'class = "p-5 risks_container">', risks, '</div>')) %>% 
  mutate(r = c(disease = risks))

disease_selector_html = paste(y[['risks']], collapse = ' ') 


disease_selector_module <- function(){
  div(
  tags$head(
    tags$style(HTML("
    .risks_containers {
      display: inline-block;
      padding: 10px 14px;
      margin-bottom: 12px;
      border: 1px solid #d9d9d9;
      border-radius: 6px;
      background: #f8f9fa;
      transition: transform 0.25s ease-in-out;
      will-change: transform;
    }

    .risks_container:hover {
      transform: scale(1.05);
      cursor: pointer;
      transition: transform 0.25s ease-in-out;
    }
  ")),
    
    tags$script(HTML("
    $(document).on('click', '.risks_container', function() {
      const value = $(this).attr('risk-value');
      console.log('risk_container clicked:', value);

      $('.risks_container').removeClass('bg-light');
      $(this).addClass('bg-light');

      Shiny.setInputValue('risk_container_clicked', value, { priority: 'event' });
    });
  "))
  ),
  
    HTML(disease_selector_html)
  )
}

# y %>% 
# HTML() %>%
page_fluid(#.,icon('home'),
  #          tags$head(
  #          tags$style(HTML("
  #   .risks_containers {
  #     display: inline-block;
  #     padding: 10px 14px;
  #     margin-bottom: 12px;
  #     border: 1px solid #d9d9d9;
  #     border-radius: 6px;
  #     background: #f8f9fa;
  #     transition: transform 0.25s ease-in-out;
  #     will-change: transform;
  #   }
  # 
  #   .risks_container:hover {
  #     transform: scale(1.05);
  #     cursor: pointer;
  #     transition: transform 0.25s ease-in-out;
  #   }
  # ")),
  #          
  #          tags$script(HTML("
  #   $(document).on('click', '.risks_container', function() {
  #     const value = $(this).attr('risk-value');
  #     console.log('risk_container clicked:', value);
  # 
  #     $('.risks_container').removeClass('bg-light');
  #     $(this).addClass('bg-light');
  # 
  #     Shiny.setInputValue('risk_container_clicked', value, { priority: 'event' });
  #   });
  # "))
# )
  
  disease_selector_module()
  
) %>%
browsable()



