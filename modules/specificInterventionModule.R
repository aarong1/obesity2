  # app.R ---------------------------------------------------------------
  library(shiny)
  library(bslib)
  
  # --------------------------------------------------------------------
  # 1) Small helpers
  # --------------------------------------------------------------------
  
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  
  # --------------------------------------------------------------------
  # 2) Ordinal toggle input (Bootstrap checkbox button group)
  # --------------------------------------------------------------------
  
  ordinalToggleInput <- function(inputId, label, choices) {
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
                class = "btn btn-outline-primary",
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
        div(style = 'width:400px;height:500px;',
            
      
    bslib::accordion(
          # class=' bslib-accordion-input alert shiny-bound-input',
          
      id = ns("acc"),
      multiple = FALSE,
      class = "alert",
      
      # 1) Core demographics & deprivation ------------------------------------
      bslib::accordion_panel(
        
        icon = icon('person'),
        
            "Core demographics & deprivation"
            ,
        selectInput(
          ns("sex"),
          "Sex",
          choices = NULL,
          multiple = TRUE
        ),
        selectInput(
          ns("age_group"),
          "Age group",
          choices = NULL,
          multiple = TRUE,
          
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
        "Geography",
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
      # 
      # # 3) Behavioural risk factors (selection: who is eligible) --------------
      bslib::accordion_panel(
        "Behavioural risk factors",
        ordinalToggleInput(
          ns("bmi"),
          "BMI category",
          choices = c( "Normal", "Overweight", "Obese")
        ),
        # radioButtons(inputId = 'bmi',label = 'BMI',c( "normal", "overweight", "obese")),
        
        ordinalToggleInput(
          ns("smoking"),
          "Smoking status",
          choices = c("Never Smoked", "Former", "Current")
        ),
        ordinalToggleInput(
          ns("alcohol"),
          "Alcohol risk",
          choices = c("< 5 drinks a week", "> more than 5 drinks a week")
        ),
        ordinalToggleInput(
          ns("diet"),
          "Fruit and Veg",
          choices = c("less than 5 a day", "meets 5")
        ),
        ordinalToggleInput(
          ns("pa"),
          "Physical activity",
          choices = c("inactive", "meets recommendations")
        )
      ),
      
      # 4) Clinical risk factors (selection) -----------------------------------
      bslib::accordion_panel(
        "Clinical risk factors",
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
        tags$strong("Target existing conditions (for selection)"),
        br(),
      
      
      div(class = 'float-right',
          input_switch('demo_negate','',width = '100%'),
      
      # HTML('<input id="demo_negate" class="form-check-input shiny-bound-input" type="checkbox" role="switch">'),
          ),
      br(),
      checkboxInput(
        ns("cond_hypertension"),
        "Hypertension",
        value = TRUE
      ),
        checkboxInput(
          ns("cond_diabetes"),
          "Diabetes",
          value = TRUE
        ),
        checkboxInput(
          ns("cond_chol"),
          "Raised cholesterol / lipid treatment",
          value = TRUE
        ),
        checkboxInput(
          ns("cond_af"),
          "Atrial fibrillation",
          value = TRUE
        ),
        checkboxInput(
          ns("cond_pad"),
          "Peripheral Arterial Disease (PAD)",
          value = TRUE
        ),
      #   checkboxInput(
      #     ns("cond_af"),
      #     "Anxiety",
      #     value = TRUE
      #   ),
        checkboxInput(
          ns("cond_sleep_apnea"),
          "Sleep Apnea",
          value = TRUE
        ),
        # checkboxInput(
        #   ns("cond_pad"),
        #   "Peripheral arterial disease (PAD)",
        #   value = FALSE
        # )
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
    
    div(class='alert alert-light',
    h3("Implementation & efficiency"),
    checkboxInput(
      ns("enable_miss"),
      "Simulate missed targets (implementation inefficiency)",
      value = TRUE
    ),
    sliderInput(
      ns("miss_prob"),
      "Probability of missing an eligible person",
      min = 0, max = 1, value = 0.9, step = 0.01
    ),
    actionButton(
      ns("apply"),
      "Update Intervention Target",
      class = "btn btn-primary"
    ),
    hr(),
    div(class = "mt-2",
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
  )
    
  }
  
  # --------------------------------------------------------------------
  # 4) Intervention selector module - Server
  # --------------------------------------------------------------------
  
  interventionSelectorServer <- function(id, data) {
    moduleServer(id, function(input, output, session) {
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
        req(nrow(df) > 0)
        
        uniq <- function(x) sort(unique(x[!is.na(x)]))
        
        # Core
        updateSelectInput(session, "sex",
                          choices = uniq(df$sex),
                          selected = uniq(df$sex)
        )
        updateSelectInput(session, "age_group",
                          choices = uniq(df$age_group),
                          selected = uniq(df$age_group)
                          # pop %>% count(age10) %>% as.list()
        )
        updateSelectInput(session, "deprivation",
                          choices = unique(df$deprivation),
                          selected = unique(df$deprivation)
        )
        updateSelectInput(session, "ethnicity",
                          choices = uniq(df$broad_ethnicity),
                          selected = uniq(df$broad_ethnicity)
        )
        
        
        # Geography
        updateSelectInput(session, "hsct",
                          choices = uniq(df$hsct),
                          selected = uniq(df$hsct)
        )
        updateSelectInput(session, "lgd",
                          choices = uniq(df$LGD2014_name),
                          selected = uniq(df$LGD2014_name)
        )
        updateSelectInput(session, "dea",
                          choices = uniq(df$DEA2014_name),
                          selected = uniq(df$DEA2014_name)
        )
        updateSelectInput(session, "settlement_name",
                          choices = uniq(df$SETTLEMENT2015_name),
                          selected = uniq(df$SETTLEMENT2015_name)
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
            hsct                = uniq(df$hsct),
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
      
      bmi_input <- reactive({
        c(ifelse(input$`htn_int-bmi_1`,'Normal',NULL),
             ifelse(input$`htn_int-bmi_2`,'Overweight',NULL),
                    ifelse(input$`htn_int-bmi_3`,'Obese',NULL))
      print(paste('BMI:',input$bmi))
      })
      
      smoking_input <- reactive({c(ifelse(input$`htn_int-smoking_1`,'Never',NULL),
                            ifelse(input$`htn_int-smoking_2`,'Former',NULL),
                            ifelse(input$`htn_int-smoking_3`,'Current',NULL)
                            )}
                            )
      
      alcohol_input <- reactive({c(ifelse(input$`htn_int-alcohol_1`,'Lower Risk ',NULL),
                            ifelse(input$`htn_int-alcohol_2`,'Increased Risk',NULL)
                            )}
                            )
      
      diet_input <- reactive({c(ifelse(input$`htn_int-diet_1`,'Less than 5',NULL),
                         ifelse(input$`htn_int-diet_2`,'5 a Day ',NULL)
                         )}
                         )
      
      pa_input <- reactive({c(ifelse(input$`htn_int-pa_1`,'Inactive',NULL),
                       ifelse(input$`htn_int-pa_2`,'Meets Recommendations',NULL)
                       )}
                       )
      
      # 2) Selection spec: current attribute selections ------------------------
      selection_spec <- reactive({
        list(
          sex                 = input$sex,
          age_group           = input$age_group,
          deprivation         = input$deprivation,
          hsct                = input$hsct,
          LGD2014_name        = input$lgd,
          DEA2014_name        = input$dea,
          SETTLEMENT2015_Band = input$settlement_name,
          Urban_status        = input$urban_status,
          bmi                 = bmi_input(),#decodeOrdinal(input$bmi),
          smoking             = decodeOrdinal(input$smoking),
          alcohol             = decodeOrdinal(input$alcohol),
          diet                = decodeOrdinal(input$diet),
          pa                  = decodeOrdinal(input$pa),
          status              = input$status,
          qrisk_score         = input$qrisk_score_range,
          diabetes            = isTRUE(input$cond_diabetes),
          cholesterol         = isTRUE(input$cond_chol),
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
        # 
        print(paste('Sex',input$sex))
        # print(paste('deprivation',input$deprivation))
        # print(paste('age_group',input$age_group))
        # 
        # print(head(df$sex))
        
        if (!is.null(input$sex) && length(input$sex) > 0) {
          idx <- idx & df$sex %in% input$sex
          print(input$sex)
        }
        if (!is.null(input$age_group) && length(input$age_group) > 0) {
          idx <- idx & df$age_group %in% input$age_group
        }
        if (!is.null(input$deprivation) && length(input$deprivation) > 0) {
          idx <- idx & df$deprivation %in% input$deprivation
        }
        
        # Geography
        # if (!is.null(input$hsct) && length(input$hsct) > 0) {
        #   idx <- idx & df$hsct %in% input$hsct
        # }
        # if (!is.null(input$lgd) && length(input$lgd) > 0) {
        #   idx <- idx & df$LGD2014_name %in% input$lgd
        # }
        # if (!is.null(input$dea) && length(input$dea) > 0) {
        #   idx <- idx & df$DEA2014_name %in% input$dea
        # }
        # if (!is.null(input$settlement_name) && length(input$settlement_name) > 0) {
        #   idx <- idx & df$SETTLEMENT2015_Band %in% input$settlement_name
        # }
        # if (!is.null(input$urban_status) && length(input$urban_status) > 0) {
        #   idx <- idx & df$Urban_status %in% input$urban_status
        # }
        
        # Behavioural (ordinal toggles)
        # bmi_sel <- decodeOrdinal(input$bmi)
        # if (!is.null(bmi_sel) && length(bmi_sel) > 0) {
        #   idx <- idx & df$bmi %in% bmi_sel
        # }
        # smoking_sel <- decodeOrdinal(input$smoking)
        # if (!is.null(smoking_sel) && length(smoking_sel) > 0) {
        #   idx <- idx & df$smoking %in% smoking_sel
        # }
        # alcohol_sel <- decodeOrdinal(input$alcohol)
        # if (!is.null(alcohol_sel) && length(alcohol_sel) > 0) {
        #   idx <- idx & df$alcohol %in% alcohol_sel
        # }
        # diet_sel <- decodeOrdinal(input$diet)
        # if (!is.null(diet_sel) && length(diet_sel) > 0) {
        #   idx <- idx & df$diet %in% diet_sel
        # }
        # pa_sel <- decodeOrdinal(input$pa)
        # if (!is.null(pa_sel) && length(pa_sel) > 0) {
        #   idx <- idx & df$pa %in% pa_sel
        # }
        
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
        # if (isTRUE(input$cond_diabetes) && "diabetes_status" %in% names(df)) {
        #   idx <- idx & !is.na(df$diabetes_status) & df$diabetes_status != 'no_diabetes'
        # }
        # if (isTRUE(input$cond_chol) && "cholesterol_status" %in% names(df)) {
        #   idx <- idx & !is.na(df$cholesterol_status) & df$cholesterol_status != 'normal_cholesterol'
        # }
        # if (isTRUE(input$cond_af) && "af_status" %in% names(df)) {#atrial_fibrillation
        #   idx <- idx & !is.na(df$af_status) & df$af_status == 'af'
        # }
        # if (isTRUE(input$cond_pad) && "pad_status" %in% names(df)) {
        #   idx <- idx & !is.na(df$pad_status) & df$pad_status  == 'pad'
        # }
        
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
            print(class(df))
            print(df)
            reached <- df %>% mutate(p=intervention_target &
                stats::rbinom(n = n(), size = 1, prob = 1 - p_miss) == 1) %>% pull(p)
            
            
            df$intervention_reached <- reached
          } else {
            df$intervention_reached <- df$intervention_target
          }
          print(df)
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
        updateSelectInput(session, "age_group",  selected = def$age_group)
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
        updateCheckboxInput(session, "cond_chol",     value = def$cholesterol)
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
