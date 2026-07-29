library(tidyverse)
library(fst)
library(data.table)
library(qs)

source('./modules/chart_update_module_3/main/main_configuration.R') # model_specification list
source('./modules/chart_update_module_3/main/main_utils_2_4.R',echo = T) 
# source('./modules/chart_update_module_3/main/pre_main_2.4.R')

source("./modules/chart_update_module_3/apply_CHD_risk.R")
source("./modules/chart_update_module_3/apply_sleep_apnea.R")
source("./modules/chart_update_module_3/transform_probability.R")

source("./modules/chart_update_module_3/disease_engines/asthma_engine.R")
source("./modules/chart_update_module_3/disease_engines/copd_engine.R")
# source("./modules/chart_update_module_3/ILD_engine.R")
source("./modules/chart_update_module_3/disease_engines/AAA_engine.R")

source("./modules/chart_update_module_3/disease_engines/epilepsy_engine.R")
source('./modules/chart_update_module_3/disease_engines/hypothyroid_engine.R')
source("./modules/chart_update_module_3/disease_engines/RA_engine.R")
source("./modules/chart_update_module_3/disease_engines/osteoporosis_engine.R")
source("./modules/chart_update_module_3/disease_engines/osteoarthritis_engine.R")

source('./modules/chart_update_module_3/deaths_module/apply_case_death.R',echo = T,chdir = T)
# source('./modules/chart_update_module_3/Births_module/births.R')
source('./modules/chart_update_module_3/Births_module/births_by_fertility_projections.R',echo = T,chdir = T)

source('./modules/chart_update_module_3/disease_engines/cancer_engine.R')

source('./modules/chart_update_module_3/disease_equation/risk_qcancer_lungcancer.R')

source("./modules/chart_update_module_3/disease_equation/risk_qcancer_colorectal.R")


# ORAL
source("./modules/chart_update_module_3/disease_equation/site_cancers/oral_cancer.R")
# PANCREATIC
source("./modules/chart_update_module_3/disease_equation/site_cancers/pancreatic_cancer.R")
# UTERINE (F)
source("./modules/chart_update_module_3/disease_equation/site_cancers/uterine_female_cancer.R")
# BLOOD CANCER
source("./modules/chart_update_module_3/disease_equation/site_cancers/blood_cancer.R")
# OVARIAN (F)
source("./modules/chart_update_module_3/disease_equation/site_cancers/ovarian_female_cancer.R")
# OESPHAGEAL-GASTRIC
source("./modules/chart_update_module_3/disease_equation/site_cancers/oesteogastric_cancer.R")
# PROSTATE (M)
source("./modules/chart_update_module_3/disease_equation/site_cancers/prostate_male_cancer.R")
# BREAST CANCER (F)
source("./modules/chart_update_module_3/disease_equation/site_cancers/breast_female_cancer.R")
# RENAL 
source("./modules/chart_update_module_3/disease_equation/site_cancers/renal_cancer.R")

#load population
past_populations = read.fst('./modules/chart_update_module_3/past_populations_new_schama.fst')
past_populations$intervention = 'non-intervention'

message('opened file')
#re initialise configuration
# initial_time_zero_population <- initial_time_zero_population %>% slice_sample(prop=0.01)
model_specification$population$scale_down_factor = model_specification$population$scale_down_factor/0.01
model_specification$model$duration = 5
model_specification$model$number_of_runs = 2

prevalence_hsct_new <- prevalence_hsct  %>% 
  arrange(Disease,HSCT, Year) %>% 
  fill(prob,.direction = 'down') 

trusts <- c("BHSCT", "NHSCT","SHSCT", "WHSCT","SEHSCT")
morbidities <- c( 'non_diabetic_hyperglycaemia', 'copd', 'asthma', 'depression', 'atrial_fibrillation', 'cancer', 
                  'chronic_kidney_disease', 'chd', 'dementia', 'hypertension', 'stroke', 'diabetes', 'heart_failure',
                  'rheumatoid_arthritis'#, #'epilepsy', 'pad'
                  # 'osteoporosis','hypothyroidism'
)

# intervention_shape_df <- qread('draggable_data.qs')
# past_populations = read.fst('./modules/chart_update_module_3/past_populations_new_schama.fst')
# 
# targeted_ids <- past_populations %>% 
#   filter(year == min(year)) %>% 
#   mutate(intervention_reached = sample(x = c(T,F), prob = c(0.1,1-0.1), replace = T,size = n())) %>%
#   filter(intervention_reached==T) %>% 
#   pull(id)
# 
# past_populations <-  past_populations%>% 
#   mutate(intervention_reached = ifelse(id %in% targeted_ids,T,F ))
# 
# initial_time_zero_population <-  past_populations%>% 
#   filter(year == min(year))
# 
# target_population <- initial_time_zero_population%>% 
#   filter(intervention_reached == T)
#
# session = list(sendCustomMessage=function(x,c){ return() })

run_model <- function(target_population, session, intervention_shape_df) {
  
  intervened_population <- data.frame()
  
  # target_population <- target_population %>% 
  #   filter(intervention_reached)
  
  if(is.null( intervention_shape_df[2])){
    intervention_shape_df <- data.frame(year=1:40,intervention = 1)
  }else{
    intervention_shape_df <- intervention_shape_df[[2]]  %>%
      unlist() %>%
      matrix(ncol=2,byrow = T) %>%
      as.data.frame() %>%
      setNames(c("year","intervention")) %>%
      mutate(intervention = ifelse(0.05 < abs(1-intervention),intervention,1))
  }
  
  for(run1 in 1:(model_specification$model$number_of_runs)) { #(model_specification$model$number_of_runs)
    
    cat(paste('################################### \n run : ', run1, ' \n###################################### \n'))
    
    print(target_population %>% 
            filter(intervention_reached) %>%
            nrow()
    )
    
    current_population <- target_population %>% 
      # filter(intervention_reached) %>% 
      filter(year==min(year)) %>% 
      filter(run == run1)
    
    print('Adding the current population to the past populations data structure')
    
      current_population$intervention <- 'intervention'
    
    # 
    # intervened_population <- bind_rows(intervened_population, current_population%>%
    #                                 mutate(run = run1))
    
    for (time in 1:model_specification$model$duration){
      
      # time=1
      cat(paste('###################################### \n Time, t : ', time, '\n Run, r:', run1,'\n###################################### \n'))
      
      current_population <- current_population %>%
        mutate(age = age + 1) %>% 
        mutate(year = year + 1) %>% 
        mutate(
          age20 = cut(age,include.lowest = T,
                      breaks = seq(0,120,20),
                      labels = c('0-20',
                                 '20-40',
                                 '40-60',
                                 '60-80',
                                 '80-100',
                                 '100-120')
          ))
      
      print('Apply and Partition deaths')
      
      current_population <- current_population %>% 
        apply_age_sex_death(apply_death = T) #%>% 
      # apply_qmortality_mortality(apply_death = T)
      
      # current_population_who_died <- current_population %>%
      #   filter( !is.na(death) & !is.null(death) & !death==0 )
      # 
      # dead_population <- bind_rows(dead_population, current_population_who_died)
      # 
      # current_population_alive <- current_population %>%
      #   filter(is.na(death)| is.null(death)| death==0)
      # 
      # current_population <- current_population_alive
      
      print('Apply Births')
      
      # current_population <- current_population %>% 
      #   asfr_births(fertility)
      
      
      
        current_population$intervention <- 'intervention'
        
        print('Shouldnt enter normally')
        message('entered intervention loop')
        
        print(intervention_shape_df[[2]])
        print(class(intervention_shape_df[[2]]))
        print(intervention_shape_df[2])
        print(class(intervention_shape_df[2]))
        print(intervention_shape_df)
        print(class(intervention_shape_df))
        
        
        print(intervention_shape_df)
        
        reduce_by <- intervention_shape_df %>% 
          mutate(year = year - (min(year)-1)) %>%
          filter(year == time) %>%
          pull(intervention)
        
        lag_reduce_by <- intervention_shape_df %>% 
          mutate(year = year - (min(year)-1)) %>%
          filter(year == (time-1)) %>%
          pull(intervention)
        
        if(length(reduce_by)==0) reduce_by = 1
        
        if(length(lag_reduce_by)==0) lag_reduce_by = 1
        
        
        print(reduce_by)
        
        current_population <- current_population %>% 
          # bmi_down_one_level(subset=T)
          # bmi_obese_overweight(subset = T)
          reduce_all_precentile(by = (1/lag_reduce_by), subset=T, subset_name = 'intervention_reached') %>% 
          reduce_all_precentile(by = (1-reduce_by), subset=T, subset_name = 'intervention_reached')
        

      
      if(time%/%3==0){
        
        print('Recalibrating risk perecentiles and Risk States every 3 years')
        
        # current_population <- reindex_risk_percentile(current_population)
        # 
        # current_population <- current_population %>% 
        #   apply_bmi_lifestyle_parameter_3State_rank_stability(bmi_stratified_prevalence) %>% 
        #   apply_child_bmi_lifestyle_parameter_3State_rank_stability(child_bmi_stratified_prevalence) %>% 
        #   combine_child_adult_bmi() %>% 
        #   apply_smoking_lifestyle_parameter_rank_stability(smoking_results_df) %>%
        #   apply_alcohol_lifestyle_parameter_rank_stability(alcohol_stratified_prevalence) %>%
        #   apply_diet_lifestyle_parameter_rank_stability(diet_stratified_prevalence) %>%
        #   apply_pa_lifestyle_parameter_rank_stability(pa_stratified_prevalence) %>%
        #   apply_wellbeing_depression_lifestyle_parameter_rank_stability(wellbeing_results_df) %>% #count(wellbeing)
        #   apply_pollution_lifestyle_parameter_geography_constant(lookup_dz_raster_cell) %>% 
        #   apply_sleep_lifestyle_parameter_rank_stability() %>% 
        #   apply_cholesterol_physiological_parameter_rank_stability(chol_perc) %>%
        #   apply_hypertension_physiological_parameter_rank_stability(hypertension_results_df) %>%
        #   apply_diabetes_physiological_parameter_rank_stability(diabetes_joint_estimation_results_df) %>% 
        #   apply_granular_cholesterol_measure_posthoc_overlay(base_population_w_physiological_and_modifiable_risk_factors,special_cholesterol) %>% 
        #   base_population_w_physiological_and_modifiable_risk_factors %>% 
        #   apply_ckd_physiological_parameter_rank_stability(ckd_prevalence) %>% 
        #   apply_pad_physiological_parameter_rank_stability(pad_prevalence) %>% 
        #   apply_vte_physiological_parameter_rank_stability(vte_prevalence) %>% 
        #   apply_af_physiological_parameter_rank_stability(af_prevalence)
        
      }
      
      print('entered non intervention loop')
      
      # current_population <- reindex_percentile(current_population)
      
      # current_population <- current_population %>% 
      #   apply_bmi_lifestyle_parameter_3State_rank_stability(bmi_stratified_prevalence) %>% 
      #   apply_child_bmi_lifestyle_parameter_3State_rank_stability(child_bmi_stratified_prevalence) %>% 
      #   combine_child_adult_bmi() %>% 
      #   apply_smoking_lifestyle_parameter_rank_stability(smoking_results_df) %>%
      #   apply_alcohol_lifestyle_parameter_rank_stability(alcohol_stratified_prevalence) %>%
      #   apply_diet_lifestyle_parameter_rank_stability(diet_stratified_prevalence) %>%
      #   apply_pa_lifestyle_parameter_rank_stability(pa_stratified_prevalence) %>%
      #   apply_wellbeing_depression_lifestyle_parameter_rank_stability(wellbeing_results_df) %>% #count(wellbeing)
      #   apply_pollution_lifestyle_parameter_geography_constant(lookup_dz_raster_cell) %>% 
      #   apply_sleep_lifestyle_parameter_rank_stability() %>% 
      #   apply_cholesterol_physiological_parameter_rank_stability(chol_perc) %>%
      #   apply_hypertension_physiological_parameter_rank_stability(hypertension_results_df) %>%
      #   apply_diabetes_physiological_parameter_rank_stability(diabetes_joint_estimation_results_df) %>% 
      #   apply_granular_cholesterol_measure_posthoc_overlay(base_population_w_physiological_and_modifiable_risk_factors,special_cholesterol) %>% 
      #   base_population_w_physiological_and_modifiable_risk_factors %>% 
      #   apply_ckd_physiological_parameter_rank_stability(ckd_prevalence) %>% 
      #   apply_pad_physiological_parameter_rank_stability(pad_prevalence) %>% 
      #   apply_vte_physiological_parameter_rank_stability(vte_prevalence) %>% 
      #   apply_af_physiological_parameter_rank_stability(af_prevalence)
      
      if(time %/%2==0){
        
        print('updating morbidity risk every 2 years')
        
        current_population <- current_population %>% #count(rheumatoid_arthritis) 
          # apply_osteoarthritis_risk(osteoarthritis_incidence)
          calculate_risk_of_morbidity() %>% #count(rheumatoid_arthritis) 
          apply_osteoporosis_risk() %>% 
          apply_hypothyroid_risk() %>% 
          
          apply_cancer_risk() %>% 
          
          apply_breast_cancer_risk_wo_risk_factors() %>%
          mutate(female_breast_cancer_year_risk = transform_probability_to_1y(breast_cancer_risk, tot_years = 5)) %>% 
          
          apply_prostate_cancer_risk_wo_risk_factors() %>%
          mutate(prostate_cancer_year_risk = transform_probability_to_1y(prostate_cancer_risk, tot_years = 5)) %>% 
          
          apply_pancreatic_cancer_risk_wo_risk_factors() %>%
          mutate(pancreatic_cancer_year_risk = transform_probability_to_1y(pancreatic_cancer_risk, tot_years = 5)) %>% 
          
          apply_renal_cancer_risk_wo_risk_factors() %>%
          mutate(renal_cancer_year_risk = transform_probability_to_1y(renal_cancer_risk, tot_years = 5)) %>% 
          
          apply_uterian_cancer_risk_wo_risk_factors() %>%
          mutate(uterine_cancer_year_risk = transform_probability_to_1y(uterine_cancer_risk, tot_years = 5)) %>% 
          
          apply_ovarian_cancer_risk_wo_risk_factors() %>%
          mutate(ovarian_cancer_year_risk = transform_probability_to_1y(ovarian_cancer_risk, tot_years = 5)) %>% 
          
          apply_blood_cancer_risk_wo_risk_factors() %>%
          mutate(blood_cancer_year_risk = transform_probability_to_1y(blood_cancer_risk, tot_years = 5)) %>% 
          
          apply_osteogastric_cancer_risk_wo_risk_factors() %>%
          mutate(osteogastric_cancer_year_risk = transform_probability_to_1y(osteogastric_cancer_risk, tot_years = 5)) %>% 
          
          apply_oral_cancer_risk_wo_risk_factors() %>%
          mutate(oral_cancer_year_risk = transform_probability_to_1y(oral_cancer_risk, tot_years = 5))
      }
      
      print('Applying absolute morbidity onset')
      
      current_population <- current_population %>% 
        declare_absolute_incident_morbidity(morbidity = "stroke") %>% 
        declare_absolute_incident_morbidity(morbidity = "chd") %>% 
        declare_absolute_incident_morbidity(morbidity = "diabetes") %>% 
        declare_absolute_incident_morbidity(morbidity = "dementia") %>% 
        declare_absolute_incident_morbidity(morbidity = "heart_failure") %>% 
        declare_absolute_incident_morbidity(morbidity = "atrial_fibrillation") %>% 
        declare_absolute_incident_morbidity(morbidity = "hypertension") %>% 
        declare_absolute_incident_morbidity(morbidity = "chronic_kidney_disease") %>% 
        declare_absolute_incident_morbidity(morbidity = "lung_cancer") %>% 
        
        declare_absolute_incident_morbidity(morbidity = "asthma") %>% 
        declare_absolute_incident_morbidity(morbidity = "copd") %>% 
        declare_absolute_incident_morbidity(morbidity = "depression") %>% 
        declare_absolute_incident_morbidity(morbidity = "non_diabetic_hyperglycaemia") %>%
        declare_absolute_incident_morbidity(morbidity = "colorectal_cancer") %>% 
        
        declare_absolute_incident_morbidity(morbidity = "epilepsy") %>% 
        declare_absolute_incident_morbidity(morbidity = "rheumatoid_arthritis") %>%
        declare_absolute_incident_morbidity(morbidity = "osteoarthritis") %>%
        
        declare_absolute_incident_morbidity(morbidity = "osteoporosis") %>% 
        declare_absolute_incident_morbidity(morbidity = "hypothyroidism") %>% 
        declare_absolute_incident_morbidity(morbidity = "pad") 
      
      # current_population$osteogastric_cancer <- NA
      # current_population$renal_cancer <- NA
      
      current_population$cancer_recovered <- NA
      current_population$osteogastric_cancer_recovered <- NA
      current_population$prostate_cancer_recovered <- NA
      current_population$female_breast_cancer_recovered <- NA
      current_population$renal_cancer_recovered <- NA
      # current_population$oesophageal_cancer_recovered <- NA
      # current_population$oesphageal_cancer_recovered <- NA
      current_population$oral_cancer_recovered <- NA
      current_population$pancreatic_cancer_recovered <- NA
      current_population$uterine_cancer_recovered <- NA
      current_population$blood_cancer_recovered <- NA
      current_population$ovarian_cancer_recovered <- NA
      
      current_population <- current_population %>% 
        
        # "lung_cancer"               
        # declare_absolute_incident_morbidity("stomach_cancer") %>%  
        declare_absolute_incident_morbidity("cancer") %>%
        
        declare_absolute_incident_morbidity("osteogastric_cancer") %>%
        declare_absolute_incident_morbidity("prostate_cancer") %>% 
        declare_absolute_incident_morbidity("female_breast_cancer") %>%      
        declare_absolute_incident_morbidity("renal_cancer") %>% 
        # declare_absolute_incident_morbidity("oesophageal_cancer") %>%        
        # declare_absolute_incident_morbidity("oesphageal_gastric_cancer") %>% 
        declare_absolute_incident_morbidity("oral_cancer") %>%               
        declare_absolute_incident_morbidity("pancreatic_cancer") %>% 
        declare_absolute_incident_morbidity("uterine_cancer") %>%            
        declare_absolute_incident_morbidity("blood_cancer") %>%              
        declare_absolute_incident_morbidity("ovarian_cancer") 
      
      current_population <- current_population %>%
        apply_case_death( morbidity = 'cancer') %>%
        apply_case_death( morbidity = 'stroke') %>%
        apply_case_death( morbidity = 'chd') %>%
        
        apply_case_death( morbidity = 'diabetes') %>%
        apply_case_death( morbidity = 'asthma') %>%
        apply_case_death( morbidity = 'copd') %>%
        
        apply_case_death( morbidity = 'chronic_kidney_disease') %>%
        apply_case_death( morbidity = 'dementia') %>%
        apply_case_death( morbidity = 'heart_failure')
      
      # current_population <- current_population %>%
      # apply_case_death( morbidity = 'lung_cancer') %>%
      # apply_case_death( morbidity = 'colorectal_cancer') %>%
      # apply_case_death( morbidity = 'oral_cancer') %>%
      # apply_case_death( morbidity = 'pancreatic_cancer') %>%
      # apply_case_death( morbidity = 'uterine_cancer') %>%
      # apply_case_death( morbidity = 'blood_cancer') %>%
      # apply_case_death( morbidity = 'ovarian_cancer') %>%
      # apply_case_death( morbidity = 'osteogastric_cancer') %>%
      # apply_case_death( morbidity = 'prostate_cancer') %>%
      # apply_case_death( morbidity = 'female_breast_cancer') %>%
      # apply_case_death( morbidity = 'renal_cancer')
      
      current_population_who_died <- current_population %>%
        filter( !is.na(death) & !is.null(death) & !death==0 )
      
      dead_population <- bind_rows(dead_population, current_population_who_died)
      
      current_population_alive <- current_population %>%
        filter(is.na(death)| is.null(death)| death==0)
      
      current_population <- current_population_alive
      
      print('Adding the current population to the past populations data structure')
      intervened_population <- bind_rows(intervened_population, current_population %>%
                                      mutate(run = run1))
      
      #------------------------------------------------------------------------------------------
      #dummy list update ------------------------------------------------------------------------
      #------------------------------------------------------------------------------------------
      
      #incidence of stroke
      print('Calculating the incidence of stroke')
      df <- count(intervened_population,stroke  = (stroke==year), run, year) |>
        filter(stroke ==TRUE) |> 
        mutate(n = n * model_specification$population$scale_down_factor)
      
      print('Calculating the prevalence of stroke')
      
      df <- count(intervened_population,stroke  = (stroke != 0), run, year) |>
        filter(stroke ==TRUE) |> 
        mutate(n = n * model_specification$population$scale_down_factor)
      # print(df)
      
      df_json <- jsonlite::toJSON(
        unname(as.list(df[c('year','n')])),  # prevent named keys like "year", "incidence"
        dataframe = "columns",
        auto_unbox = TRUE
      )
      print(df_json)
      
      print('Calculating the json')
      
      series_list <- df %>%
        group_by(run) %>%
        summarise(data = list(map2(as.character(year), n, ~ list(.x, .y)))) %>%
        mutate(name = paste0("run ", run)) %>%
        transmute(name, data) %>%
        jsonlite::toJSON(auto_unbox = TRUE)
      
      print(series_list)
      print('calculating the series list')
      
      session$sendCustomMessage("updateChart", series_list)
      
      #dummy(series_list)
    }
  }
  
  message('main 2.4 done')
  
  # print(intervened_population$intervention_target)
  return(intervened_population)
}

# 
# intervention_shape_df <- qread('draggable_data.qs')
# past_populations = read.fst('./modules/chart_update_module_3/past_populations_new_schama.fst')
# 
# targeted_ids <- past_populations %>%
#   filter(year == min(year)) %>%
#   mutate(intervention_reached = sample(x = c(T,F), prob = c(0.1,1-0.1), replace = T,size = n())) %>%
#   filter(intervention_reached==T) %>%
#   pull(id)
# 
# past_populations <-  past_populations%>%
#   mutate(intervention_reached = ifelse(id %in% targeted_ids,T,F ))
# 
# initial_time_zero_population <-  past_populations%>%
#   filter(year == min(year))
# 
# target_population <- initial_time_zero_population%>%
#   filter(intervention_reached == T)
# 
# session = list(sendCustomMessage=function(x,c){ return() })
# 
# target_populations_df  <- run_model(target_population,
#     session = session,
#     intervention_shape_df)
# 
#   past_populations$intervention = 'non-intervention'
# 
#   t_past_populations <- past_populations %>%
#     filter(run <= model_specification$model$number_of_runs ) %>%
#     filter(year <= max(target_populations_df$year) &
#              year >= min(target_populations_df$year)
#            ) %>%
#     filter(!intervention_reached) %>%
#     bind_rows(target_populations_df ) %>%
#     mutate(intervention = 'intervention')
# 
# 
#   total_pop <- bind_rows(
#     past_populations %>%
#       filter(run <= max(target_populations_df$run) ) %>%
#       filter(year <= max(target_populations_df$year) &
#                year >= min(target_populations_df$year)
#       ),
#     t_past_populations
#     )
# 
# #   
# # 
