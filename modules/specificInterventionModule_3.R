  # app.R ---------------------------------------------------------------
  library(shiny)
  library(bslib)
  
  # --------------------------------------------------------------------
  # 1) Small helpers
  # --------------------------------------------------------------------

  
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  
xx <- pop %>% 
  mutate(SETTLEMENT2015_name = replace_na(SETTLEMENT2015_name,'Rural')) %>% 
  mutate(SETTLEMENT2015_Band = replace_na(SETTLEMENT2015_Band,'Rural')) %>% 
  
    group_by(SETTLEMENT2015_Band) %>% 
    summarise(s = list(unique(SETTLEMENT2015_name)),
              n = n() * model_specification$population$scale_down_factor ) %>% 
  rowwise() %>% 
  mutate(ss=length(s)) %>% 
  mutate(label = HTML(paste(replace_na(SETTLEMENT2015_Band,'Rural'),' (',ss,') ', 
                      # '<div>',
                            format(big.mark=",",n ))#,
                       #'</div>'
                      )
         ) #%>% View()

xx[[2]][[1]] <- list(xx[[2]][[1]] ,'')
xx[[2]][[2]] <- list(xx[[2]][[2]] ,'')
xx[[2]][[7]] <- xx[[2]][[7]]


  # --------------------------------------------------------------------
  # 2) Ordinal toggle input (Bootstrap checkbox button group)
  # --------------------------------------------------------------------
  
  ordinalToggleInput <- function(inputId, label, choices, class = NULL) {
    ids <- paste0(inputId, "_", seq_along(choices))
    
    tagList(
      div(
        class = "mb-2",
        tags$label(class = "form-label fw-bold", label),
        br(),
        tags$div(
          class = "btn-group",
          role = "group",
          lapply(seq_along(choices), function(i) {
            list(
              tags$input(
                type = "checkbox",
                class = "btn-check",
                id = ids[i],
                autocomplete = "off"
              ),
              tags$label(
                class = paste("btn ",ifelse(is.null(class[i]),'btn-outline-primary',class[i])),
                `for` = ids[i],
                choices[i]
              )
            )
          })
        ),
        # Hidden input that Shiny actually binds to
        tags$input(id = inputId, type = "hidden")
      )
    )
  }
  
  # --------------------------------------------------------------------
  # 3) Intervention selector module - UI
  # --------------------------------------------------------------------
  
  interventionSelectorUI <- function(id) {
    
  
    ns <- NS(id)
    
    div(
      tags$head(
        tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/ion-rangeslider/2.3.1/css/ion.rangeSlider.min.css"),
        tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/ion-rangeslider/2.3.1/js/ion.rangeSlider.min.js"),
        
        HTML('<style>
        
/* accordion  */ 


      
/* Slider */ 

.irs-line {
    /* background: linear-gradient(0.25turn, #3cb3716e, #ffa5007a, #ff00006b); */ 
    background: linear-gradient(0.25turn, #3cb371e8, white, #00b0ff) !important;
  }
  
 .irs-bar {
    background: #ffffffbf !important;
  }

  </style>

/* Selectize */ 

<style>
    span.label {
    padding:5px;
    font-size: 12px;
    color:mediumseagreen;
    }
    
 div[data-value="45-54"] span.label {
    color:var(--bs-warning);
    }

  div[data-value="55-64"] span.label {
    color:var(--bs-warning);
    font-weight:bold;
    }

  div[data-value="65-74"] span.label {
    color:var(--bs-danger);
    }

  div[data-value="75-110"] span.label {
    color:var(--bs-danger);
    font-weight:bold;
    }

     span.caption {
    padding:5px;
    position:absolute;
    left:50px;
    font-size: 8px;
    color: grey;
    }

               .selectize-control.contacts .selectize-input > div .email {
    opacity: 0.8;
  }
  .selectize-control.contacts .selectize-input > div .name + .email {
    margin-left: 5px;
  }
  
  .selectize-control.contacts .selectize-dropdown .caption {
    font-size: 6px;
    display: block;
    color: #a0a0a0;
  }
  </style>
  ')
      ),
      
      
  #     tags$head(
  #       HTML('<style>
  #   span.label {
  #   padding:5px;
  #   font-size: 14px;
  #   color: steelblue;
  #   color:mediumseagreen;
  #   }
  #    span.caption {
  #    padding:5px;
  #   font-size: 10px;
  #   color: grey;
  #   }
  # .selectize-control.contacts .selectize-input > div .email {
  #   opacity: 0.8;
  # }
  # .selectize-control.contacts .selectize-input > div .name + .email {
  #   margin-left: 5px;
  # }
  # .selectize-control.contacts .selectize-dropdown .caption {
  #   font-size: 12px;
  #   display: block;
  #   color: #a0a0a0;
  # }
  # </style>
  # ')
  #     ),
        div(style = 'width:400px;height:550px;overflow:hidden',
    bslib::accordion(
          # class=' bslib-accordion-input alert shiny-bound-input',
          
      id = ns("acc"),
      multiple = FALSE,
      class = "alert",
      
      # 1) Core demographics & deprivation ------------------------------------
      bslib::accordion_panel(
        icon = icon(class = 'me-3','person'),
            "Core demographics & deprivation",
        selectInput(
          ns("sex"),
          "Sex",
          choices = NULL,
          multiple = TRUE
        ),
        # selectInput(
        #   ns("age_group"),
        #   "Age group",
        #   choices = NULL,
        #   multiple = TRUE,
        # ),
        selectizeInput(
          ns("age_group"),
          "Age group",
          choices = NULL, # Populated via options below
          multiple = TRUE,
          options = list(
            persist = T,
            maxItems = NULL,
            valueField = "email",
            labelField = "name",
            render = I("{
    item: function(item, escape) {
      console.log(item);
      var name = item.email ? '<span class=\"name\">' + escape(item.email) + '</span>' : '';
      return '<div>' +  name + '</div>';
    },
    option: function(item, escape) {
      var label = item.name || item.email;
      var caption = item.name ? item.email : null;
      return '<div>' +
       (caption ? '<span class=\"label\">' + caption + '</span>' : '') + '<span class=\"caption\">' + label + '</span>' +

      '</div>';
    }
  }") 
          )
        ),
        selectInput(
          ns("ethnicity"),
          "Ethnicity",
          choices = NULL,
          multiple = TRUE
        ),
        selectInput(
          ns("deprivation"),
          "Deprivation",
          choices = NULL,
          multiple = TRUE
        ),
        # selectInput(
        #   ns("Townsend Material deprivation"),
        #   "Deprivation",
        #   choices = NULL,
        #   multiple = TRUE
        # ),
      ),
      
      # 2) Geography -----------------------------------------------------------
      bslib::accordion_panel(
        icon = icon(class = 'me-3',name = 'earth-americas'),
        "Contiguous and non-contiguous heirarchical geography ",
        selectInput(
          ns("hsct"),
          "HSCT",
          choices = NULL,
          multiple = TRUE
        ),
        
      #   selectInput(
      #     ns("lgd"),
      #     "LGD 2014",
      #     choices = NULL,
      #     multiple = TRUE
      #   ),
  
      div(style = 'height:100px;',
        selectizeInput(#size = 10,
          ns("dea"),
          "DEA 2014",
          size = 10,
          choices = NULL,
          # selectize = T,
          options= list(
          dropdownParent = 'body'#https://stackoverflow.com/questions/70808663/is-there-a-way-to-limit-the-max-height-of-a-selectize-field
          ),
          multiple = TRUE
        )
        ),
        selectInput(
          ns("settlement_name"),
          "Settlement (2015)",
          choices = NULL,
          multiple = TRUE
        ),
        selectInput(
          ns("urban_status"),
          "Urban / rural status",
          choices = NULL,
          multiple = TRUE
        )
      ),
      
      # # 3) Behavioural risk factors (selection: who is eligible) --------------
      bslib::accordion_panel(
        icon = HTML('<i class="me-3 fa-solid fa-smoking"></i><b>Behavioural Risk</b>'),
        
        "Modifiable",
        ordinalToggleInput(
          ns("bmi"),
          "BMI category",
          choices = c( "Normal", "Overweight", "Obese"),
          class = c( 'btn-outline-success opacity-75','btn-outline-warning opacity-75' , 'btn-outline-danger opacity-75')
        ),
        # radioButtons(inputId = 'bmi',label = 'BMI',c( "normal", "overweight", "obese")),
        
        ordinalToggleInput(
          ns("smoking"),
          "Smoking status",
          choices = c("Never Smoked", "Former", "Current"),
          class = c( 'btn-outline-success opacity-75','btn-outline-warning opacity-75' , 'btn-outline-danger opacity-75')
          
        ),
        ordinalToggleInput(
          ns("alcohol"),
          "Alcohol risk",
          choices = c("< 5 drinks a week", "> more than 5 drinks a week"),
          class = c( 'btn-outline-success opacity-75','btn-outline-warning opacity-75' )
          
        ),
        ordinalToggleInput(
          ns("diet"),
          "Fruit and Veg",
          choices = c("less than 5 a day", "meets 5"),
          class = c( 'btn-outline-warning opacity-75','btn-outline-success opacity-75' )
          
        ),
        ordinalToggleInput(
          ns("pa"),
          "Physical activity",
          choices = c("inactive", "meets recommendations"),
          class = c( 'btn-outline-warning opacity-75','btn-outline-success opacity-75' )
          
        )
      ),
      
      # 4) Clinical risk factors (selection) -----------------------------------
      bslib::accordion_panel(
        icon = HTML('<i class="class = me-3 fa-solid fa-heart-pulse"></i><b>Clinical risk factors</b>'),
        "Non-Modifable Comorbidities",
        # selectInput(
        #   ns("status"),
        #   "Hypertension status",
        #   choices = NULL,
        #   multiple = TRUE
        # ),
      #   sliderInput(
      #     ns("qrisk_score_range"),
      #     "QRISK score range",
      #     min = 0, max = 1, value = c(0, 1), step = 0.01
      #   ),
      # 
      #   sliderInput(
      #     ns("cmms"),
      #     "Cambridge Multimorbidity score range",
      #     min = 0, max = 1, value = c(0, 1), step = 0.01
      #   ),
      # 
      #   tags$hr(),
      div(class = 'float-right',
          input_switch(id = ns('demo_negate'),'',width = '100%'),
      
      # HTML('<input id="demo_negate" class="form-check-input shiny-bound-input" type="checkbox" role="switch">'),
          ),
      div(class= 'd-flex flex-column align-items-center px-5 mx-5',
        tags$strong("Toggle for inclusion or exclusion"),
        br(),
      br(),
      tags$div(
        class = "btn-group-vertical w-50",
        role = "group",
        `aria-label` = "Basic checkbox toggle button group",

        tags$input(
          type = "checkbox",
          class = "btn-check shiny-input-checkbox",
          id = ns("cond_cholesterol"),
          # checked = NA,
          autocomplete = "off"
        ),
        tags$label(
          class = "btn btn-outline-danger",
          `for` = ns("cond_cholesterol"),
          " Cholesterol "
        ),

        tags$input(
          type = "checkbox",
          class = "btn-check shiny-input-checkbox",
          id = ns("cond_hypertension"),
          # checked = NA,
          autocomplete = "off"
        ),
        tags$label(
          class = "btn btn-outline-danger",
          `for` = ns("cond_hypertension"),
          " Hypertension "
        ),

        tags$input(
          type = "checkbox",
          class = "btn-check shiny-input-checkbox",
          id = ns("cond_diabetes"),
          # checked = FALSE,
          autocomplete = "off"
        ),
        tags$label(
          class = "btn btn-outline-danger",
          `for` = ns("cond_diabetes"),
          " Diabetes "
        ),

        tags$input(
          type = "checkbox",
          class = "btn-check shiny-input-checkbox",
          id = ns("cond_af"),
          # checked = NA,
          autocomplete = "off"
        ),
        tags$label(
          class = "btn btn-outline-danger",
          `for` = ns("cond_af"),
          " Atrial Fibrillation "
        ),

        tags$input(
          type = "checkbox",
          class = "btn-check shiny-input-checkbox",
          id = ns("cond_pad"),
          # checked = NA,
          autocomplete = "off"
        ),
        tags$label(
          class = "btn btn-outline-danger",
          `for` = ns("cond_pad"),
          " Peripheral Arterial Disease "
        ),

        tags$input(
          type = "checkbox",
          class = "btn-check shiny-input-checkbox",
          id = ns("cond_sleep_apnea"),
          # checked = NA,
          autocomplete = "off"
        ),
        tags$label(
          class = "btn btn-outline-danger",
          `for` = ns("cond_sleep_apnea"),
          " Sleep Apnea "
        )
      )
      )
      # checkboxInput(
      #   ns("cond_hypertension"),
      #   "Hypertension",
      #   value = TRUE
      # ),
      #   checkboxInput(
      #     ns("cond_diabetes"),
      #     "Diabetes",
      #     value = TRUE
      #   ),
      #   checkboxInput(
      #     ns("cond_chol"),
      #     "Raised cholesterol / lipid treatment",
      #     value = TRUE
      #   ),
      #   checkboxInput(
      #     ns("cond_af"),
      #     "Atrial fibrillation",
      #     value = TRUE
      #   ),
      #   checkboxInput(
      #     ns("cond_pad"),
      #     "Peripheral Arterial Disease (PAD)",
      #     value = TRUE
      #   ),
      #   checkboxInput(
      #     ns("cond_sleep_apnea"),
      #     "Sleep Apnea",
      #     value = TRUE
      #   ),
      #   checkboxInput(
      #     ns("cond_af"),
      #     "Anxiety",
      #     value = TRUE
      #   ),
      # )
      ),
      
      br(),
  
      # bslib::accordion_panel(title =
      #    div("Run metadata", style = "font-weight: bold;"),
      #   selectInput(
      #     ns("nruns"),
      #     "nrun",
      #     choices = NULL,
      #     multiple = TRUE
      #   ),
      #   selectInput(
      #     ns("duration"),
      #     "duration",
      #     choices = NULL,
      #     multiple = TRUE
      #   )
  
      # # 5) Implementation / missed targets ------------------------------------
      # bslib::accordion_panel(
     
      # ),
      
      # 
    )
    ),
    
    div(class='p-3 m-3',
    h3("Implementation & efficiency"),
    br(),
    div(class = 'fs-6',
    #     checkboxInput(
    #   ns("enable_miss"),
    #   "Simulate missed targets (implementation inefficiency)",
    #   value = TRUE
    # )
    ),
    # sliderInput(  
    #   ns("miss_prob"),
    #   "Probability of missing an eligible person",
    #   min = 0, max = 1, value = 0.9, step = 0.01
    # ),

    
    HTML(sprintf('<input id = "%s" type="number" class="shiny-input-bound js-range-slider" name="my_range"   
    data-min="0"
        data-max="1" value = "0.9" 
                 data-from="0.9" data-step = "0.01" />',ns("miss_prob"))),
    # tags$input(
    #   id = "demand_2030",
    #   type = "number",
    #   step = 0.2,
    #   name = "demand_2030",
    #   value = 8
    # ),
    br(),
    br(),
      
    # HTML('<input id="htn_int-enable_miss" type="checkbox" class="btn shiny-bound-input shiny-input-checkbox" checked="checked">')
    div( class="btn-group-vertical", role="group",
         
      div(class = 'input-group mb-3',
      #    div(class = 'button-group',
        HTML(paste0(' <input type="checkbox" class="btn-check" id="',ns('enable_miss'),'" autocomplete="off">
           <label style = "border-radius: 10px 0px 0px 10px;" class="btn btn-outline-primary" for="',ns('enable_miss'),'"> <i class ="fa fa-check"></i> Enable Miss </label>')),
      actionButton(
      ns("apply"),
      div(HTML('<i class="fa-solid fa-filter"></i>'),
      "Update Intervention Target"),
      class = "btn btn-primary"
    )
    #)
    )
    ),
    # hr(),
    ),

div(class = "mt-2 alert alert-light",
    div(class = "d-flex justify-content-between",
        tags$strong("Total:"),
        textOutput(ns("summary_n_total"), inline = TRUE)
    ),
    div(class = "d-flex justify-content-between",
        tags$strong("Eligible:"),
        textOutput(ns("summary_n_eligible"), inline = TRUE)
    ),
    div(class = "d-flex justify-content-between",
        tags$strong("Reached:"),
        textOutput(ns("summary_n_reached"), inline = TRUE)
    ),
    div(class = "d-flex justify-content-between",
        tags$strong("Coverage:"),
        span(class='d-inline',
             textOutput(ns("summary_coverage_pct"), inline = TRUE),
             '/',
             textOutput(ns("summary_coverage_pct1"), inline = TRUE))
    )
)
  )
    
  }
  
  # --------------------------------------------------------------------
  # 4) Intervention selector module - Server
  # --------------------------------------------------------------------

  interventionSelectorServer <- function(id, data) {
    moduleServer(id, function(input, output, session) {
      # print(data())
      
      ns <- session$ns
      
      # store default selection spec once
      defaults <- reactiveVal(NULL)
      
      # helper to decode JSON from ordinalToggleInput hidden fields
      decodeOrdinal <- function(x) {
        if (is.null(x) || !nzchar(x)) return(NULL)
        tryCatch(jsonlite::fromJSON(x), error = function(e) NULL)
      }
      
      # 1) Populate UI choices once data is available ---------------------------
      observe({
        df <- data()
        
        print(count(df,age10))
        req(nrow(df) > 0)
        
        uniq <- function(x) sort(unique(x[!is.na(x)]))
        
        # Core
        updateSelectInput(session, "sex",
                          choices = uniq(df$sex),
                          selected = uniq(df$sex)
        )
        
        # updateSelectInput(session, "age_group",
        #                   choices = uniq(df$age10),
        #                   selected = uniq(df$age10)
        #                   # pop %>% count(age10) %>% as.list()
        # )
        
        updateSelectizeInput(session, 
                             inputId='age_group', 
                             selected = uniq(df$age10),
                             choices = df %>%
                               count(age10) %>%
                               t() %>% 
                               as.data.frame() %>% 
                               setNames(format(
                                 big.mark=",",
                                 as.numeric(.[2,])*model_specification$population$scale_down_factor
                               )) %>%  
                               sapply(FUN = function(x){
                                 list( 
                                   x[[1]])})
   
        )
                             
        updateSelectInput(session, "deprivation",
                          choices = unique(df$mdm_quintile_soa_name),
                          selected = unique(df$mdm_quintile_soa_name)
        )
        updateSelectInput(session, "ethnicity",
                          choices = uniq(df$broad_ethnicity),
                          selected = uniq(df$broad_ethnicity)
        )
        
        # Geography
        updateSelectInput(session, "hsct",
                          choices = uniq(df$HSCT),
                          selected = uniq(df$HSCT)
        )
        updateSelectInput(session, "lgd",
                          choices = uniq(df$LGD2014_name),
                          selected = uniq(df$LGD2014_name)
        )
        # updateSelectInput(session, "dea",
        #                   choices = uniq(df$DEA2014_name),
        #                   selected = uniq(df$DEA2014_name)
        # )
        
{   x <- df %>% count(HSCT,DEA2014_name) %>% count(HSCT,DEA2014_name,name='pop') %>% group_by(HSCT) %>% 
    summarise(dea = list(DEA2014_name),n=n(),nn=sum(pop), n_label = paste0(first(HSCT),' (',n,') ',format(nn,big.mark=','))) %>% 
    as.list();x[[2]] %>% set_names(x[['n_label']])
}
        updateSelectInput(session, "dea",
                          choices = x[[2]] %>% set_names(x[['n_label']]) ,
                          selected = uniq(df$DEA2014_name)
        )
        updateSelectInput(session, "settlement_name",
                          choices =   xx[[2]] %>% set_names(xx[[5]]),
                          selected = xx[[2]] %>% set_names(xx[[5]]) %>% unlist()#uniq(df$SETTLEMENT2015_name)
        )
        updateSelectInput(session, "urban_status",
                          choices = uniq(df$Urban_status),
                          selected = uniq(df$Urban_status)
        )
        # Clinical
        updateSelectInput(session, "status",
                          choices = uniq(df$status),
                          selected = uniq(df$status)
        )
        
        # QRISK range from data
        if (!all(is.na(df$qrisk_score))) {
          qs_min <- min(df$qrisk_score, na.rm = TRUE)
          qs_max <- max(df$qrisk_score, na.rm = TRUE)
          updateSliderInput(
            session, "qrisk_score_range",
            min = floor(qs_min * 100) / 100,
            max = ceiling(qs_max * 100) / 100,
            value = c(qs_min, qs_max)
          )
        }
        
        # Set defaults once (for selection / attributes) -----------------------
        if (is.null(defaults())) {
          qs_min <- if (!all(is.na(df$qrisk_score))) min(df$qrisk_score, na.rm = TRUE) else 0
          qs_max <- if (!all(is.na(df$qrisk_score))) max(df$qrisk_score, na.rm = TRUE) else 1
          
          defaults(list(
            sex                 = uniq(df$sex),
            age20           = uniq(df$age20),
            age                 = range(df$age),
            deprivation         = uniq(df$deprivation),
            ethnicity           = uniq(df$broad_ethnicity),
            hsct                = uniq(df$HSCT),
            LGD2014_name        = uniq(df$LGD2014_name),
            DEA2014_name        = uniq(df$DEA2014_name),
            SETTLEMENT2015_Band = uniq(df$SETTLEMENT2015_Band),
            Urban_status        = uniq(df$Urban_status),
            bmi                 = unique(df$bmi), #NULL,  # ordinal toggles: NULL = no filter
            smoking             = unique(df$smoking), #NULL,
            alcohol             = unique(df$alcohol), #NULL,
            diet                = unique(df$diet), #NULL,
            pa                  = unique(df$pa), #NULL,
            status              = uniq(df$status),
            # sleep_apnea
            # depression
            qrisk_score         = c(qs_min, qs_max),
            # cmms                = range(df$cmms,na.rm = T),
            hypertension = TRUE,#FALSE,
            diabetes            = TRUE,#FALSE,  #is.numeric(pop$cholesterol)
            cholesterol         = TRUE,#FALSE,
            atrial_fibrillation = TRUE,#FALSE,
            ckd = TRUE,#FALSE,
            pad                 = TRUE#FALSE
          ))
        }
      })
      
      # bmi_input <- reactive({
      #   print(input$`htn_int-bmi_1`)
      #   x <- c(ifelse(input$`htn_int-bmi_1`,'normal',NULL),
      #        ifelse(input$`htn_int-bmi_2`,'overweight',NULL),
      #               ifelse(input$`htn_int-bmi_3`,'obese',NULL))
      # print(paste('BMI:', x))
      # x
      # })
      
      bmi_input <- reactiveVal({c('normal',
                                  'overweight',
                                  'obese')})
      # c('normal',
      #   'overweight',
      #   'obese')
      
      # observeEvent(input$apply,{
      #   print('bmi triggered by input$apply')
      #   x <- c(ifelse(input$`bmi_1`,'normal',NULL),
      #                 ifelse(input$`bmi_2`,'overweight',NULL),
      #                 ifelse(input$`bmi_3`,'obese',NULL))
      #   print(x)
      #   bmi_input(x)
      # })
      
      observe({
        print('bmi triggered by input$bmi_1')
        print(input$bmi_1)
        
        bmi_gather <- c(
               ifelse(input$bmi_1,'normal',NA),
               ifelse(input$bmi_2,'overweight',NA),
               ifelse(input$bmi_3,'obese',NA))
        
        s <- sum(input$bmi_1,
            input$bmi_2,
            input$bmi_3 )
        
        print(bmi_gather)
        
        if(s!=0){  bmi_input(bmi_gather)      
          }else{      bmi_input(c('normal',
                                  'overweight',
                                  'obese'))
            }
        
      })
                            smoking_input <- reactiveVal(c('never_smoked', 'former', 'current_smoker'))

                            observe({
                              smoking_gather <- c(
                                ifelse(input$smoking_1, 'never_smoked', NA),
                                ifelse(input$smoking_2, 'former', NA),
                                ifelse(input$smoking_3, 'current_smoker', NA)
                              )

                              s <- sum(input$smoking_1, input$smoking_2, input$smoking_3)

                              if (s != 0) {
                                smoking_input(smoking_gather)
                              } else {
                                smoking_input(c('never_smoked', 'former', 'current_smoker'))
                              }
                            })

                            alcohol_input <- reactiveVal(c('no_risk', 'lower_risk', 'increased_risk', 'higher_risk'))

                            observe({
                              alcohol_gather <- c(
                                if (isTRUE(input$alcohol_1)) c('no_risk', 'lower_risk') else character(0),
                                if (isTRUE(input$alcohol_2)) c('increased_risk', 'higher_risk') else character(0)
                              )

                              s <- sum(input$alcohol_1, input$alcohol_2)

                              if (s != 0) {
                                alcohol_input(unique(alcohol_gather))
                              } else {
                                alcohol_input(c('no_risk', 'lower_risk', 'increased_risk', 'higher_risk'))
                              }
                            })

                            diet_input <- reactiveVal(c('below_5_a_day', 'meets_5_a_day'))

                            observe({
                              diet_gather <- c(
                                ifelse(input$diet_1, 'below_5_a_day', NA),
                                ifelse(input$diet_2, 'meets_5_a_day', NA)
                              )

                              s <- sum(input$diet_1, input$diet_2)

                              if (s != 0) {
                                diet_input(diet_gather)
                              } else {
                                diet_input(c('below_5_a_day', 'meets_5_a_day'))
                              }
                            })

                            pa_input <- reactiveVal(c('inactive', 'low_activity', 'some_activity', 'meets_rec'))

                            observe({
                              pa_gather <- c(
                                if (isTRUE(input$pa_1)) c('inactive', 'low_activity', 'some_activity') else character(0),
                                if (isTRUE(input$pa_2)) 'meets_rec' else character(0)
                              )

                              s <- sum(input$pa_1, input$pa_2)

                              if (s != 0) {
                                pa_input(unique(pa_gather))
                              } else {
                                pa_input(c('inactive', 'low_activity', 'some_activity', 'meets_rec'))
                              }
                            })
      
      # 2) Selection spec: current attribute selections ------------------------
      selection_spec <- reactive({
        list(
          sex                 = input$sex,
          age_group           = input$age_group,
          deprivation         = input$deprivation,
          hsct                = input$hsct,
          # LGD2014_name        = input$lgd,
          DEA2014_name        = input$dea,
          SETTLEMENT2015_Band = input$settlement_name,
          Urban_status        = input$urban_status,
          bmi                 = bmi_input(),#decodeOrdinal(input$bmi),
          smoking             = smoking_input(),
          alcohol             = alcohol_input(),
          diet                = diet_input(),
          pa                  = pa_input(),
          # status              = input$status,
          # qrisk_score         = input$qrisk_score_range,
          diabetes            = isTRUE(input$cond_diabetes),
          cholesterol         = isTRUE(input$cond_cholesterol),
          atrial_fibrillation = isTRUE(input$cond_af),
          pad                 = isTRUE(input$cond_pad)
        )
      })
      
      # 3) Which attributes differ from defaults? ------------------------------
      modified_attributes <- reactive({
        sel <- selection_spec()
        def <- defaults()
        if (is.null(def)) return(character(0))
        
        nm <- intersect(names(sel), names(def))
        nm[vapply(nm, function(nm1) !identical(sel[[nm1]], def[[nm1]]), logical(1))]
      })
      
      # 4) Deterministic eligibility -------------------------------------------
      
      eligible_df <- reactive({
        df <- data()
        
        req(nrow(df) > 0)
        
        idx <- rep(TRUE, nrow(df))
        
        # Core demographics & deprivation
        # print(df)

        print(paste('Sex',input$sex))
        # print(paste('deprivation',input$deprivation))
        print(paste('age_group',input$age_group))
        # print(paste('hsct',input$hsct))
        # print(head(df$sex))
        
        if (!is.null(input$sex) && length(input$sex) > 0) {
          idx <- idx & df$sex %in% input$sex
          print(input$sex)
        }
        if (!is.null(input$age_group) && length(input$age_group) > 0) {
          idx <- idx & df$age10 %in% input$age_group
        }
        if (!is.null(input$deprivation) && length(input$deprivation) > 0) {
          idx <- idx & df$deprivation %in% input$deprivation
        }
        
        # Geography
        if (!is.null(input$hsct) && length(input$hsct) > 0) {
          print(input$hsct)
          print(unique(df$hsct))
          idx <- idx & df$HSCT %in% input$hsct
        }
        # if (!is.null(input$lgd) && length(input$lgd) > 0) {
        #   idx <- idx & df$LGD2014_name %in% input$lgd
        # }
        if (!is.null(input$dea) && length(input$dea) > 0) {
          idx <- idx & df$DEA2014_name %in% input$dea
        }
        if (!is.null(input$settlement_name) && length(input$settlement_name) > 0) {
          idx <- idx & df$SETTLEMENT2015_name %in% input$settlement_name
        }
        if (!is.null(input$urban_status) && length(input$urban_status) > 0) {
          idx <- idx & df$Urban_status %in% input$urban_status
        }
        
        # Behavioural (ordinal toggles)
        
        # bmi_sel <- decodeOrdinal(input$bmi)
        # print(bmi_sel)
        
        print(bmi_input())
        if (!is.null(bmi_input()) && length(bmi_input()) > 0) {
          idx <- idx & tidyr::replace_na(df$bmi, 'normal') %in% bmi_input()#bmi_sel
        }
        smoking_sel <- smoking_input()
        if (!is.null(smoking_sel) && length(smoking_sel) > 0) {
          idx <- idx & tidyr::replace_na(df$smoking, 'never_smoked') %in% smoking_sel
        }
        alcohol_sel <- alcohol_input()
        if (!is.null(alcohol_sel) && length(alcohol_sel) > 0) {
          idx <- idx & tidyr::replace_na(df$alcohol, 'lower_risk') %in% alcohol_sel
        }
        diet_sel <- diet_input()
        if (!is.null(diet_sel) && length(diet_sel) > 0) {
          idx <- idx & tidyr::replace_na(df$diet, 'meets_5_a_day') %in% diet_sel
        }
        pa_sel <- pa_input()
        if (!is.null(pa_sel) && length(pa_sel) > 0) {
          idx <- idx & tidyr::replace_na(df$pa, 'meets_rec') %in% pa_sel
        }
        
        # Clinical risk factors
        # if (!is.null(input$status) && length(input$status) > 0) {
        #   idx <- idx & df$status %in% input$status
        # }
        # if (!is.null(input$qrisk_score_range)) {
        #   rng <- input$qrisk_score_range
        #   idx <- idx &
        #     !is.na(df$qrisk_score) &
        #     df$qrisk_score >= rng[1] &
        #     df$qrisk_score <= rng[2]
        # }
        
        # Condition tick-boxes (assume non-missing & != 0 means present)
        if (isTRUE(input$cond_diabetes) && "diabetes_status" %in% names(df)) {
          diabetes_match <- df$diabetes_status != "no_diabetes"
          if (isTRUE(input$demo_negate)) {
            diabetes_match <- !diabetes_match
          }
          idx <- idx & !is.na(df$diabetes_status) & diabetes_match
        }
        if (isTRUE(input$cond_cholesterol) && "cholesterol_status" %in% names(df)) {
          cholesterol_match <- df$cholesterol_status != "normal_cholesterol"
          if (isTRUE(input$demo_negate)) {
            cholesterol_match <- !cholesterol_match
          }
          idx <- idx & !is.na(df$cholesterol_status) & cholesterol_match
        }
        
        if (isTRUE(input$cond_hypertension) && "hypertension_status" %in% names(df)) {
          hypertension_match <- df$hypertension_status %in% c(
            "hypertensive_uncontrolled",
            "hypertensive_untreated",
            "hypertensive_controlled"
          )
          if (isTRUE(input$demo_negate)) {
            hypertension_match <- !hypertension_match
          }
          idx <- idx & !is.na(df$hypertension_status) & hypertension_match
        }
        if (isTRUE(input$cond_af) && "af_status" %in% names(df)) { # atrial_fibrillation
          af_match <- df$af_status == "af"
          if (isTRUE(input$demo_negate)) {
            af_match <- !af_match
          }
          idx <- idx & !is.na(df$af_status) & af_match
        }
        if (isTRUE(input$cond_pad) && "pad_status" %in% names(df)) {
          pad_match <- df$pad_status == "pad"
          if (isTRUE(input$demo_negate)) {
            pad_match <- !pad_match
          }
          idx <- idx & !is.na(df$pad_status) & pad_match
        }
        if (isTRUE(input$cond_sleep_apnea) && "sleep" %in% names(df)) {
          sleep_apnea_match <- df$sleep == "sleep_apnea"
          if (isTRUE(input$demo_negate)) {
            sleep_apnea_match <- !sleep_apnea_match
          }
          idx <- idx & !is.na(df$sleep) & sleep_apnea_match
        }
        
        
        df$intervention_target <- idx
        # df
        # print('df')
        # print('###################')
        # print(df)
        # print('###################')
        df
      })
      
      # 5) Apply missed-target simulation -------------------------------------
      assigned_df <- eventReactive(
        input$apply,
        ignoreNULL = T,
        #ignoreInit = T,
        {
          print(input$htn_int_miss_prob) 
          print(input$miss_prob) # print(input$htn_int_miss_prob) # print(input$enable_miss) # print(input$miss_prob) # if (is.null(input$htn_int_miss_prob)) return(eligible_df()) # if (is.null(input$enable_miss)) return(eligible_df()) # if (is.null(input$miss_prob)) return(eligible_df())
          print('running')
          df <- eligible_df()
          
          if (isTRUE(input$enable_miss)) {
            p_miss <- input$miss_prob %||% 0
            p_miss <- max(0, min(1, p_miss))
            
            # reached <- with(
            #   df,
            #   intervention_target &
            #     stats::rbinom(n = nrow(df), size = 1, prob = 1 - p_miss) == 1
            # )
            # print(class(df))
            # print(df)
            reached <- df %>% mutate(p=intervention_target &
                stats::rbinom(n = n(), size = 1, prob = 1 - p_miss) == 1) %>% pull(p)
            
            
            df$intervention_reached <- reached
          } else {
            df$intervention_reached <- df$intervention_target
          }
          # print(df)
          df
        }
      )
      
      # 6) Risk factor prevalence specification -------------------------------
      risk_spec <- reactive({     
        list(
          bmi = list(
            active        = isTRUE(input$rf_bmi_active),
            target_var    = "bmi",
            target_level  = "obese",
            rel_reduction = (input$rf_bmi_relred %||% 0) / 100
          ),
          smoking = list(
            active        = isTRUE(input$rf_smoking_active),
            target_var    = "smoking",
            target_level  = "current",
            rel_reduction = (input$rf_smoking_relred %||% 0) / 100
          ),
          alcohol = list(
            active        = isTRUE(input$rf_alcohol_active),
            target_var    = "alcohol",
            target_level  = "increased_risk",
            rel_reduction = (input$rf_alcohol_relred %||% 0) / 100
          ),
          pa = list(
            active        = isTRUE(input$rf_pa_active),
            target_var    = "pa",
            target_level  = "inactive",
            rel_reduction = (input$rf_pa_relred %||% 0) / 100
          ),
          diet = list(
            active        = isTRUE(input$rf_diet_active),
            target_var    = "diet",
            target_level  = "below_5_a_day",
            rel_reduction = (input$rf_diet_relred %||% 0) / 100
          )
        )
      })
      
      # 7) Summary in the UI ---------------------------------------------------
      output$summary_n_total <- renderText({
        df <- assigned_df()
        format(nrow(df)*model_specification$population$scale_down_factor, big.mark = ",")
      })
  
      output$summary_n_eligible <- renderText({
        df <- assigned_df()
        format(sum(df$intervention_target*model_specification$population$scale_down_factor, na.rm = TRUE), big.mark = ",")
      })
  
      output$summary_n_reached <- renderText({
        df <- assigned_df()
        format(sum(df$intervention_reached*model_specification$population$scale_down_factor, na.rm = TRUE), big.mark = ",")
      })
  
      output$summary_coverage_pct1 <- renderText({
        df <- assigned_df()
        n_eligible <- nrow(df)
        n_reached  <- sum(df$intervention_reached, na.rm = TRUE)
        if (n_eligible > 0) paste0(round(100 * n_reached / n_eligible, 2), "%") else "NA"
      })
        
      output$summary_coverage_pct <- renderText({
        df <- assigned_df()
        n_eligible <- sum(df$intervention_target, na.rm = TRUE)
        n_reached  <- sum(df$intervention_reached, na.rm = TRUE)
        if (n_eligible > 0) paste0(round(100 * n_reached / n_eligible, 2), "%") else "NA"
      })
      
      
      
      # 8) Reset all selection controls ---------------------------------------
      reset_all <- function() {
        def <- defaults()
        if (is.null(def)) return(invisible())
        
        # Core
        updateSelectInput(session, "sex",        selected = def$sex)
        updateSelectInput(session, "age_group",  selected = def$age10)
        updateSelectInput(session, "deprivation",selected = def$deprivation)
        
        # Geography
        updateSelectInput(session, "hsct",           selected = def$hsct)
        updateSelectInput(session, "lgd",            selected = def$LGD2014_name)
        updateSelectInput(session, "dea",            selected = def$DEA2014_name)
        updateSelectInput(session, "settlement_name",selected = def$SETTLEMENT2015_name)
        updateSelectInput(session, "urban_status",   selected = def$Urban_status)
            
        # Clinical selections
        updateSelectInput(session, "status", selected = def$status)
        updateSliderInput(session, "qrisk_score_range", value = def$qrisk_score)
        
        updateCheckboxInput(session, "cond_diabetes", value = def$diabetes)
        updateCheckboxInput(session, "cond_cholesterol", value = def$cholesterol)
        updateCheckboxInput(session, "cond_af",       value = def$atrial_fibrillation)
        updateCheckboxInput(session, "cond_pad",      value = def$pad)
        
        # Ordinal toggles: reset via custom message
        session$sendCustomMessage("ordinal-reset", list(id = ns("bmi"),     values = def$bmi     %||% character(0)))
        session$sendCustomMessage("ordinal-reset", list(id = ns("smoking"), values = def$smoking %||% character(0)))
        session$sendCustomMessage("ordinal-reset", list(id = ns("alcohol"), values = def$alcohol %||% character(0)))
        session$sendCustomMessage("ordinal-reset", list(id = ns("diet"),    values = def$diet    %||% character(0)))
        session$sendCustomMessage("ordinal-reset", list(id = ns("pa"),      values = def$pa      %||% character(0)))
        
        # Risk prevalence controls: reset to 0 / FALSE
        updateCheckboxInput(session, "rf_bmi_active",      value = FALSE)
        updateSliderInput(  session, "rf_bmi_relred",      value = 0)
        updateCheckboxInput(session, "rf_smoking_active",  value = FALSE)
        updateSliderInput(  session, "rf_smoking_relred",  value = 0)
        updateCheckboxInput(session, "rf_alcohol_active",  value = FALSE)
        updateSliderInput(  session, "rf_alcohol_relred",  value = 0)
        updateCheckboxInput(session, "rf_pa_active",       value = FALSE)
        updateSliderInput(  session, "rf_pa_relred",       value = 0)
        updateCheckboxInput(session, "rf_diet_active",     value = FALSE)
        updateSliderInput(  session, "rf_diet_relred",     value = 0)
      }
      

      # 9) Return API ----------------------------------------------------------
      list(
        data                = assigned_df,          # reactive df with flags
        spec                = selection_spec,       # reactive list (names = attributes)
        modified_attributes = modified_attributes,  # reactive character vector
        risk_spec           = risk_spec,            # reactive list for prevalence mods
        reset_all           = reset_all             # function
      )
    })
  }
  
  # --------------------------------------------------------------------
  # 5) UI: page_fluid + JS for ordinal toggles & reset
  # --------------------------------------------------------------------
