
  lost_productivity_estimator <- function(past_populations) {
  
    morb_cols <- c('cvd'='pad', 
                   'msk'='osteoporosis', 
                   'cancer'='cancer', 
                   'msk'='osteoarthritis',   
                   'msk'='rheumatoid_arthritis',  
                   'other'='epilepsy',          
                   'other'='hypothyroidism',
                   'resp'='asthma',       
                   'resp'='copd',           
                   'other'='depression',       
                   'cvd'='non_diabetic_hyperglycaemia',
                   'cancer'='colorectal_cancer',       
                   'cancer'='prostate_cancer',      
                   'cancer'='female_breast_cancer',  
                   'cancer'='kidney_cancer',         
                   'cancer'='oesophageal_cancer',    
                   'cancer'='stomach_cancer',       
                   'cancer'='osteogastric_cancer',   
                   'cancer'='oral_cancer',             
                   'cancer'='pancreatic_cancer',       
                   'cancer'='uterine_cancer',   
                   'cancer'='blood_multiple_myeloma',     
                   'cancer'='blood_lymphoma',         
                   'cancer'='blood_leukaemia',        
                   'cancer'='blood_cancer',          
                   'cancer'='ovarian_cancer',      
                   'cancer'='lung_cancer',
                   'cvd'='stroke', 
                   'cvd'='chd',
                   'cvd'='diabetes',
                   'other'='dementia',
                   'cvd'='heart_failure',
                   'cvd'='atrial_fibrillation',
                   'cvd'='hypertension',
                   'cvd'='chronic_kidney_disease')
    
    sick_days_matrix <- tibble::tribble(
      ~broad, ~sick_spells_per_case, ~days_lost_per_case, ~cost_per_case,
      "resp",         0.0025372668,        0.113786804,     16.219132,
      "cvd",         0.0007710649,        0.007355829,      1.048497,
      "cancer",         0.0031595995,        0.020359984,      2.902105,
      "msk",         0.0020023097,        0.071843454,     10.240542
    ) |> 
    mutate(sick_spells_per_case = sick_spells_per_case/10 ) |> 
    mutate(days_lost_per_case = days_lost_per_case/10 ) |> 
    mutate(cost_per_case = cost_per_case/10 )
    
    
    
    sick_days_matrix
    x <- past_populations |>
      group_by(run,intervention,year) |> 
      summarise(
        across(
          all_of(unname(morb_cols)),
          \(x) sum(x, na.rm = TRUE)
        )
      )
    
    x <- pivot_longer(x, -c(run,intervention,year))
    
    x <- x |> 
      left_join(data.frame(class = names(morb_cols), name = morb_cols)) 
    
    x <- x |> 
      ungroup() |> 
      # filter(run==min(run) ) |>
      # filter(intervention == first(intervention)) |> 
      group_by(class,intervention,run, year) |> 
      summarise(value = sum(value,na.rm=T)) |> 
      group_by(class,intervention, year) |> 
      summarise(value = mean(value,na.rm=T)) 
    
    
    x <- x |> 
      left_join(sick_days_matrix, by =c( 'class' = 'broad')) |> 
      # left_join(bed_days_df, by= c('class' = 'broad')) |> 
      mutate(sick_spells = value * sick_spells_per_case) |>
      mutate(days_lost = value * days_lost_per_case) |> 
      mutate(cost = value * cost_per_case) |> 
      
      mutate(year = as.character(year)) |> 
      group_by(intervention,year) |>
      summarise( sick_spells = sum(sick_spells, na.rm=T)*model_specification$population$scale_down_factor,
                 days_lost = sum(days_lost, na.rm=T)*model_specification$population$scale_down_factor,
                 cost = sum(cost, na.rm=T)*model_specification$population$scale_down_factor) 
    
    return(x)

  }
                
  
  # lost_productivity_estimator(total_pop) |> 
  #   group_by(intervention) |> 
  #   e_charts(year) |> 
  #   e_line(cost) |> 
  #   e_tooltip()
