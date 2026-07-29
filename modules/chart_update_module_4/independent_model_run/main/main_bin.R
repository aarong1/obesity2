do_not_run_moved_to_loop <- function(){
  
  #1
  stroke_incidence <- show_incidence(initial_time_zero_population, stroke_year_risk, stroke,stroke, mdm_quintile, year, HSCT, sex, age20, run) %>% #LGD2014NAME,
    mutate(morbidity = 'stroke')
  stroke_outcome_incidence <- rbind(stroke_outcome_incidence,stroke_incidence)
  
  #2  
  diabetes_incidence <- show_incidence(initial_time_zero_population, hypertension_year_risk, hypertension, mdm_quintile, year, HSCT, sex, age20, run) %>% #LGD2014NAME,
    mutate(morbidity = 'diabetes')
  diabetes_outcome_incidence <- rbind(diabetes_outcome_incidence, diabetes_incidence)
  
  #3  
  chd_incidence <- show_incidence(initial_time_zero_population, chd_year_risk, chd, mdm_quintile, year, HSCT, sex, age20, run) %>% #LGD2014NAME,
    mutate(morbidity= 'chd')
  chd_outcome_incidence <- rbind(chd_outcome_incidence, chd_incidence)
  
  #4  
  af_incidence <- show_incidence(initial_time_zero_population, atrial_fibrillation_year_risk, atrial_fibrillation, mdm_quintile, year, HSCT, sex, age20, run) %>% #LGD2014NAME,
    mutate(morbidity='atrial_fibrillation')
  af_outcome_incidence <- rbind(af_outcome_incidence, af_incidence)
  
  #5
  hypertension_incidence <- show_incidence(initial_time_zero_population, hypertension_year_risk, hypertension, mdm_quintile, year, HSCT, sex, age20, run) %>% #LGD2014NAME,
    mutate(morbidity = 'hypertension')
  hypertension_outcome_incidence <- rbind(hypertension_outcome_incidence, hypertension_incidence)
  
  #6
  ckd_incidence <- show_incidence(initial_time_zero_population, chronic_kidney_disease_year_risk, chronic_kidney_disease, mdm_quintile, year, HSCT, sex, age20, run) %>% #LGD2014NAME,
    mutate(morbidity = 'chronic_kidney_disease')
  ckd_outcome_incidence <- rbind(ckd_outcome_incidence, ckd_incidence)
  
  #7
  lungcancer_incidence <- show_incidence(initial_time_zero_population,lung_cancer_year_risk,lung_cancer, year,mdm_quintile, HSCT,sex,age20,run) |> 
    mutate(morbidity='lung_cancer')
  lungcancer_outcome_incidence <- rbind(lungcancer_outcome_incidence, lungcancer_incidence)
  
  #8
  dementia_incidence <- show_incidence(initial_time_zero_population,dementia_year_risk,dementia,  year, mdm_quintile,  HSCT, sex, age20, run) |> 
    mutate(morbidity = 'dementia')
  dementia_outcome_incidence <- rbind(dementia_outcome_incidence, dementia_incidence)
  
  #9
  heart_failure_incidence <- show_incidence(current_population, heart_failure_year_risk, heart_failure,  year, mdm_quintile,  HSCT, sex, age20, run) |> 
    mutate(morbidity = 'heart_failure')
  
  heart_failure_outcome_incidence <- rbind(heart_failure_outcome_incidence, heart_failure_incidence)
  
}