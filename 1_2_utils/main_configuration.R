
model_specification <- list(
  population = list(scale_down_factor = 10 * 2#950
  ),
  model = list(
    start_year = 2021,
    duration = 5,
    number_of_runs = 4
    ),
  meta = list(
    random_seed = {invisible(set.seed(22));22},
    time_configuration_defined = Sys.time(),
    time_configuration_updated = Sys.time(),
    last_time_configuration_run_in_main_def = NULL),
  
  # risk = list(
  #   alcohol = 'two_state_transition_probability',
  #   smoking = 'two_state_transition_probability',
  #   bmi = 'three_state_rank_stability',
  #   diet = 'two_state_transition_probability',
  #   activity = 'two_state_transition_probability',
  #   
  #   cholesterol = 'two_state_transition_probability',
  #   atrial_fibrillation = 'two_state_transition_probability',
  #   hypertension = 'two_state_transition_probability',
  #   diabetes = 'two_state_transition_probability',
  #   peripheral_arterial_disease = 'two_state_transition_probability'
  #   
  #   # pollution = correlation_linear,
  #   # sleep = correlation_linear
  #   
  # ),
  
  intervention = list(
    
    impact_scalar = list('2020' = 1),
    
    target_geography = list(
      trust = list(),
      lgd = list(),
      urban = list()
    ),
    target_demographics = list(
      age = list(),
      sex = list(),
      townsend_score = list(),
      mdm_decile = list()
    ),
    target_risk = list(
      alcohol = list(),
      smoking = list(),
      bmi = list(),
      diet = list(),
      activity = list()
    ),
    target_column_attributes = list(
      column = list('attribute')
    ),
    target_morbidity = 'stroke' )
)


### # model_specification$target = input
### input$demographics$age  = c(15,110)
### input$demographics$sex = c('Males','Females')
### input$demographics$townsend_score = range(townsend$townsend_TDS)
### input$demographics$mdm_decile = 1:10
### input$demographics$broad_ethnicity = c("minority", "white")
### input$demographics$HSCT = count(initial_time_zero_population,HSCT) |> pull(HSCT)
### input$risk$bmi = c('normal','overweight','obese')
### input$risk$bmi_percentile = c( 0.6, 1 )
### input$risk$qrisk = c( 0.25, 1 )



test_specification <- list(
  
  population = list(
    
    scale_down_factor =  1900 #475 -
    
    
    
    
  ),
  
  model = list(
    start_year = 2020, #2024 #2018,
    duration = 5,
    
    number_of_runs = 5
    
  ),
  
  random_seed = {invisible(set.seed(22));22} ,
  
  intervention = list(
    
    impact_scalar = c('2020' = 1),
    
    # target_geography = list(
    #   trust = list(),
    #   lgd = list(),
    #   urban = list()
    # ),
    
    target_demographics = list(
      age = c(30:70)
    ),
    
    target_morbidity = 'stroke'
    
  )
)

sppg_specification <- list(
  population = list(
    scale_down_factor =  1900/4 #475 -4000
  ),
  model = list(
    start_year = 2023, #2024 #2018,
    duration = 15,
    number_of_runs = 15),
  
  random_seed = {invisible(set.seed(22));22},
  
  intervention = NULL
)

# model_specification <- sppg_specification
# model_specification <- test_specification

model_specification <- model_specification
