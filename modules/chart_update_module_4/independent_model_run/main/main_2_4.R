library(tidyverse)
library(fst)
library(data.table)

source('./main/main_configuration.R') # model_specification list
source('./main/main_utils_2_4.R') 
# source('./main/pre_main_2.4.R')

source("./disease_engines/asthma_engine.R")
source("./disease_engines/copd_engine.R")
# source("./ILD_engine.R")
source("./disease_engines/AAA_engine.R")

source("./disease_engines/epilepsy_engine.R")
source('./disease_engines/hypothyroid_engine.R')
source("./disease_engines/RA_engine.R")
source("./disease_engines/osteoporosis_engine.R")
source("./disease_engines/osteoarthritis_engine.R")

source('./disease_engines/low_birth_weight_engine.R')

source('./deaths_module/apply_case_death.R')

# source('./Births_module/births.R')
source('./Births_module/births_by_fertility_projections.R')

source('./populate_time_zero_prevalence/populate_osteoarthritis.R')
source('./populate_time_zero_prevalence/populate_epilepsy_prevalence.R')

source('./populate_time_zero_cancers_prevalence/populate_all_cancers.R') 


source('./populate_time_zero_cancers_prevalence/populate_lung_cancer.R') 
source('./populate_time_zero_cancers_prevalence/populate_colorectal_cancer.R')
source('./populate_time_zero_cancers_prevalence/populate_stomach_cancer.R') 
source('./populate_time_zero_cancers_prevalence/populate_prostate_cancer.R')
source('./populate_time_zero_cancers_prevalence/populate_female_breast_cancer.R')
source('./populate_time_zero_cancers_prevalence/populate_kidney_cancer.R')
source('./populate_time_zero_cancers_prevalence/populate_oesophageal_cancer.R')
source('./populate_time_zero_cancers_prevalence/populate_stomach_cancer.R')
source('./populate_time_zero_cancers_prevalence/populate_oral_cancer.R')
source('./populate_time_zero_cancers_prevalence/populate_pancreatic_cancer.R')
source('./populate_time_zero_cancers_prevalence/populate_uterine_cancer.R')
source('./populate_time_zero_cancers_prevalence/populate_blood_multiple_myeloma.R')
source('./populate_time_zero_cancers_prevalence/populate_blood_lymphoma.R')
source('./populate_time_zero_cancers_prevalence/populate_blood_leukaemia.R')
source('./populate_time_zero_cancers_prevalence/populate_ovarian_cancer.R')

source('./disease_engines/cancer_engine.R')

source('./disease_equation/risk_qcancer_lungcancer.R')

source("./disease_equation/risk_qcancer_colorectal.R")


# ORAL
source("./disease_equation/site_cancers/oral_cancer.R")
# PANCREATIC
source("./disease_equation/site_cancers/pancreatic_cancer.R")
# UTERINE (F)
source("./disease_equation/site_cancers/uterine_female_cancer.R")
# BLOOD CANCER
source("./disease_equation/site_cancers/blood_cancer.R")
# OVARIAN (F)
source("./disease_equation/site_cancers/ovarian_female_cancer.R")
# OESPHAGEAL-GASTRIC
source("./disease_equation/site_cancers/oesteogastric_cancer.R")
# PROSTATE (M)
source("./disease_equation/site_cancers/prostate_male_cancer.R")
# BREAST CANCER (F)
source("./disease_equation/site_cancers/breast_female_cancer.R")
# RENAL 
source("./disease_equation/site_cancers/renal_cancer.R")


#load population
initial_time_zero_population = read.fst('./main/initial_time_zero_population10down.fst')

#re initialise configuration
initial_time_zero_population <- initial_time_zero_population %>% slice_sample(prop=0.1)
model_specification$population$scale_down_factor = model_specification$population$scale_down_factor/0.1
model_specification$model$duration = 5
model_specification$model$number_of_runs = 10

#SEt population start year
initial_time_zero_population$year <- model_specification$model$start_year 

# apply osteoarthritis risk

initial_time_zero_population <- initial_time_zero_population %>% 
  apply_osteoporosis_risk() %>% 
  apply_hypothyroid_risk() #%>% 
# apply_epilepsy_risk() %>%  # Qof
# apply_pad_risk_wo_risk_factors() %>%  
# mutate(pad_year_risk = transform_probability_to_1y(pad_risk, tot_years = 4))

# initial_time_zero_population <- apply_osteoarthritis_risk(initial_time_zero_population ,osteoarthritis_incidence) 
# initial_time_zero_population <- apply_osteoarthritis_prevalence(initial_time_zero_population ) 
# initial_time_zero_population <- populate_epilepsy_prevalence(initial_time_zero_population)

# initial_time_zero_population <- initial_time_zero_population %>% 
#   apply_doh_disease_prevalence('osteoporosis') %>% 
#   apply_doh_disease_prevalence('hypothyroidism') %>% 
#   apply_doh_disease_prevalence('pad') 

# initial_time_zero_population <- initial_time_zero_population %>%
#   setDT() %>%
#   apply_low_birth_weight_risk()

# initial_time_zero_population %>% count(age==0,low_birth_weight)

initial_time_zero_population <- initial_time_zero_population %>%
  apply_breast_cancer_risk_wo_risk_factors() %>%
  mutate(female_breast_cancer_year_risk = transform_probability_to_1y(breast_cancer_risk, tot_years = 5))
initial_time_zero_population <- initial_time_zero_population %>%
  apply_prostate_cancer_risk_wo_risk_factors() %>%
  mutate(prostate_cancer_year_risk = transform_probability_to_1y(prostate_cancer_risk, tot_years = 5))
initial_time_zero_population <- initial_time_zero_population %>%
  apply_pancreatic_cancer_risk_wo_risk_factors() %>%
  mutate(pancreatic_cancer_year_risk = transform_probability_to_1y(pancreatic_cancer_risk, tot_years = 5))
initial_time_zero_population <- initial_time_zero_population %>%
  apply_renal_cancer_risk_wo_risk_factors() %>%
  mutate(renal_cancer_year_risk = transform_probability_to_1y(renal_cancer_risk, tot_years = 5))
initial_time_zero_population <- initial_time_zero_population %>%
  apply_uterian_cancer_risk_wo_risk_factors() %>%
  mutate(uterine_cancer_year_risk = transform_probability_to_1y(uterine_cancer_risk, tot_years = 5))
initial_time_zero_population <- initial_time_zero_population %>%
  apply_ovarian_cancer_risk_wo_risk_factors() %>%
  mutate(ovarian_cancer_year_risk = transform_probability_to_1y(ovarian_cancer_risk, tot_years = 5))
initial_time_zero_population <- initial_time_zero_population %>%
  apply_blood_cancer_risk_wo_risk_factors() %>%
  mutate(blood_cancer_year_risk = transform_probability_to_1y(blood_cancer_risk, tot_years = 5))
initial_time_zero_population <- initial_time_zero_population %>%
  apply_osteogastric_cancer_risk_wo_risk_factors() %>%
  mutate(osteogastric_cancer_year_risk = transform_probability_to_1y(osteogastric_cancer_risk, tot_years = 5))
initial_time_zero_population <- initial_time_zero_population %>% 
  apply_oral_cancer_risk_wo_risk_factors() %>%
  mutate(oral_cancer_year_risk = transform_probability_to_1y(oral_cancer_risk, tot_years = 5))


dead_population <- data.frame()
initial_time_zero_population$death <- NA
initial_time_zero_population$death_reason <- NA
initial_time_zero_population$qmortality_risk <- NA
initial_time_zero_population$qx <- NA

prevalence_hsct_new <- prevalence_hsct  %>% 
  # group_by(Disease,) %>% 
  arrange(Disease,HSCT, Year) %>% 
  fill(prob,.direction = 'down') # %>% 

# stroke_outcome_incidence = data.frame()
# diabetes_outcome_incidence = data.frame()
# chd_outcome_incidence = data.frame()
# af_outcome_incidence = data.frame()
# hypertension_outcome_incidence = data.frame()
# ckd_outcome_incidence = data.frame()
# lungcancer_outcome_incidence = data.frame()
# dementia_outcome_incidence = data.frame()
# heart_failure_outcome_incidence = data.frame()

# stroke_outcome_prevalence = data.frame()
# diabetes_outcome_prevalence = data.frame()
# chd_outcome_prevalence = data.frame()
# af_outcome_prevalence = data.frame()
# hypertension_outcome_prevalence = data.frame()
# ckd_outcome_prevalence = data.frame()
# lungcancer_outcome_prevalence = data.frame()
# dementia_outcome_prevalence = data.frame()
# heart_failure_outcome_prevalence = data.frame()


initial_time_zero_population$pad = 0
initial_time_zero_population$rheumatoid_arthritis = 0
initial_time_zero_population$copd = 0
initial_time_zero_population$asthma = 0
initial_time_zero_population$copd = 0
initial_time_zero_population$depression = 0
initial_time_zero_population$non_diabetic_hyperglycaemia = 0
initial_time_zero_population$osteoporosis = 0
# initial_time_zero_population$osteoarthritis = 0

initial_time_zero_population$hypothyroidism_recovered = NA
initial_time_zero_population$pad_recovered <- NA
initial_time_zero_population$osteoporosis_recovered <- NA
initial_time_zero_population$osteoarthritis_recovered <- NA
initial_time_zero_population$rheumatoid_arthritis_recovered = NA
initial_time_zero_population$epilepsy_recovered = NA
initial_time_zero_population$colorectal_cancer_recovered = NA
initial_time_zero_population$asthma_recovered = NA
initial_time_zero_population$copd_recovered = NA
initial_time_zero_population$depression_recovered = NA
initial_time_zero_population$non_diabetic_hyperglycaemia_recovered = NA

trusts <- c("BHSCT", "NHSCT","SHSCT", "WHSCT","SEHSCT")
morbidities <- c( 'non_diabetic_hyperglycaemia', 'copd', 'asthma', 'depression', 'atrial_fibrillation', 'cancer', 
                  'chronic_kidney_disease', 'chd', 'dementia', 'hypertension', 'stroke', 'diabetes', 'heart_failure',
                  'rheumatoid_arthritis'#, #'epilepsy', 'pad'
                  # 'osteoporosis','hypothyroidism'
)

# Model run ----
initial_time_zero_population <- initial_time_zero_population %>% 
  mutate(.before = 3, intervention = 'non-intervention')

count(initial_time_zero_population,rheumatoid_arthritis)
count(initial_time_zero_population,wt = rheumatoid_arthritis_year_risk)
# x1 <- initial_time_zero_population

# initial_time_zero_population <- x1
past_populations <- data.frame() #initial_time_zero_population)

for(run1 in 1:(model_specification$model$number_of_runs)) { #(model_specification$model$number_of_runs)
  
  cat(paste('################################### \n run : ', run1, ' \n###################################### \n'))
  
  base_population <- populate_all_cancers_prevalence(initial_time_zero_population)
  
  base_population <- populate_prostate_cancer(base_population)
  base_population <- populate_female_breast_cancer(base_population)
  base_population <- populate_renal_cancer_prevalence(base_population)
  base_population <- populate_oesophageal_cancer_prevalence(base_population)
  base_population <- populate_stomach_cancer_prevalence(base_population)
  
  base_population <- base_population %>% 
    mutate(osteogastric_cancer = case_when( oesophageal_cancer != 0 ~ oesophageal_cancer, 
                                            stomach_cancer != 0 ~ stomach_cancer,
                                            T ~0 ))
  # count(base_population,stomach_cancer,oesophageal_cancer,osteogastric_cancer)
  
  base_population <- populate_oral_cancer_prevalence(base_population)
  base_population <- populate_pancreatic_cancer_prevalence(base_population)
  base_population <- populate_uterine_cancer(base_population);count(base_population,uterine_cancer)
  
  base_population <- populate_blood_multiple_myeloma_prevalence(base_population)
  base_population <- populate_blood_lymphoma_prevalence(base_population)
  base_population <- populate_blood_leukaemia_prevalence(base_population)
  
  base_population <- base_population %>% 
    mutate(blood_cancer = case_when( blood_multiple_myeloma != 0 ~ blood_multiple_myeloma,
                                     blood_lymphoma != 0 ~ blood_lymphoma,
                                     blood_leukaemia != 0 ~ blood_leukaemia,
                                     T ~ 0  ))
  
  # count(base_population, blood_multiple_myeloma, blood_lymphoma, blood_leukaemia, blood_cancer)
  
  base_population <- populate_ovarian_cancer(base_population)
  base_population <- populate_colorectal_cancer_prevalence(base_population)
  base_population <- populate_lung_cancer_prevalence(base_population)
  #count(base_population,lung_cancer)
  
  base_population <- apply_osteoarthritis_prevalence(base_population ) 
  base_population <- populate_epilepsy_prevalence(base_population)
  
  population_w_established_prevalence <- reduce2(
    .x = rep(trusts,length(morbidities)),
    .y = rep(morbidities,each = length(trusts)),
    .init = base_population,
    .f = function(pop, trust, morbidity) {
      assign_year_minus_one_prevalence(
        input_population = pop,
        trust = trust,
        morbidity = morbidity,
        #year = 2017,
        prevalence_df = prevalence_hsct_new,
        configuration = model_specification
      )
    }
  )
  # population_w_established_prevalence %>% filter(year ==2023) %>% count(year,HSCT,stroke) %>% add_count(year,HSCT,wt=n) %>% mutate(n/nn)
  
  count(population_w_established_prevalence,rheumatoid_arthritis)
  count(population_w_established_prevalence,wt= rheumatoid_arthritis_year_risk)
  #print(2023%in%count(population_w_established_prevalence,rheumatoid_arthritis)$rheumatoid_arthritis)
  
  population_w_established_prevalence <- population_w_established_prevalence %>% 
    apply_doh_disease_prevalence(morbidity = 'osteoporosis') %>% 
    apply_doh_disease_prevalence('hypothyroidism') %>% 
    apply_doh_disease_prevalence('pad') 

  current_population <- population_w_established_prevalence %>% 
    mutate(run = run1)
  
  current_population <- current_population %>% 
    mutate(bern_trial = runif(n()))
  
  count(current_population,rheumatoid_arthritis)
  count(current_population,wt= rheumatoid_arthritis_year_risk)
  #print(2023%in%count(current_population,rheumatoid_arthritis)$rheumatoid_arthritis)
  
  print('Adding the current population to the past populations data structure')
  past_populations <- bind_rows(past_populations, current_population%>%
                                  mutate(run = run1))
  
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
    
    current_population <- current_population %>% 
      asfr_births(fertility)
    
    print(2023%in%count(current_population,rheumatoid_arthritis)$rheumatoid_arthritis)
    
    if (F) {  # run > ( round(model_specification$model$,number_of_runs/2))
      current_population$intervention <- 'intervention'
      
      print('Shouldnt enter normally')
      print('entered intervention loop')
      
    }else{ # baseline always runs
      
      if(time%/%3==0){
        
        print('Recalibrating risk perecentiles and Risk States every 3 years')
        
        current_population <- reindex_risk_percentile(current_population)
        
        current_population <- current_population %>% 
          apply_bmi_lifestyle_parameter_3State_rank_stability(bmi_stratified_prevalence) %>% 
          apply_child_bmi_lifestyle_parameter_3State_rank_stability(child_bmi_stratified_prevalence) %>% 
          combine_child_adult_bmi() %>% 
          apply_smoking_lifestyle_parameter_rank_stability(smoking_results_df) %>%
          apply_alcohol_lifestyle_parameter_rank_stability(alcohol_stratified_prevalence) %>%
          apply_diet_lifestyle_parameter_rank_stability(diet_stratified_prevalence) %>%
          apply_pa_lifestyle_parameter_rank_stability(pa_stratified_prevalence) %>%
          apply_wellbeing_depression_lifestyle_parameter_rank_stability(wellbeing_results_df) %>% #count(wellbeing)
          apply_pollution_lifestyle_parameter_geography_constant(lookup_dz_raster_cell) %>% 
          apply_sleep_lifestyle_parameter_rank_stability() %>% 
          apply_cholesterol_physiological_parameter_rank_stability(chol_perc) %>%
          apply_hypertension_physiological_parameter_rank_stability(hypertension_results_df) %>%
          apply_diabetes_physiological_parameter_rank_stability(diabetes_joint_estimation_results_df) %>% 
          apply_granular_cholesterol_measure_posthoc_overlay(base_population_w_physiological_and_modifiable_risk_factors,special_cholesterol) %>% 
          base_population_w_physiological_and_modifiable_risk_factors %>% 
          apply_ckd_physiological_parameter_rank_stability(ckd_prevalence) %>% 
          apply_pad_physiological_parameter_rank_stability(pad_prevalence) %>% 
          apply_vte_physiological_parameter_rank_stability(vte_prevalence) %>% 
          apply_af_physiological_parameter_rank_stability(af_prevalence)
        
      }

      print('entered non intervention loop')
      
      current_population <- reindex_percentile(current_population)
      
      current_population <- current_population %>% 
        apply_bmi_lifestyle_parameter_3State_rank_stability(bmi_stratified_prevalence) %>% 
        apply_child_bmi_lifestyle_parameter_3State_rank_stability(child_bmi_stratified_prevalence) %>% 
        combine_child_adult_bmi() %>% 
        apply_smoking_lifestyle_parameter_rank_stability(smoking_results_df) %>%
        apply_alcohol_lifestyle_parameter_rank_stability(alcohol_stratified_prevalence) %>%
        apply_diet_lifestyle_parameter_rank_stability(diet_stratified_prevalence) %>%
        apply_pa_lifestyle_parameter_rank_stability(pa_stratified_prevalence) %>%
        apply_wellbeing_depression_lifestyle_parameter_rank_stability(wellbeing_results_df) %>% #count(wellbeing)
        apply_pollution_lifestyle_parameter_geography_constant(lookup_dz_raster_cell) %>% 
        apply_sleep_lifestyle_parameter_rank_stability() %>% 
        apply_cholesterol_physiological_parameter_rank_stability(chol_perc) %>%
        apply_hypertension_physiological_parameter_rank_stability(hypertension_results_df) %>%
        apply_diabetes_physiological_parameter_rank_stability(diabetes_joint_estimation_results_df) %>% 
        apply_granular_cholesterol_measure_posthoc_overlay(base_population_w_physiological_and_modifiable_risk_factors,special_cholesterol) %>% 
        base_population_w_physiological_and_modifiable_risk_factors %>% 
        apply_ckd_physiological_parameter_rank_stability(ckd_prevalence) %>% 
        apply_pad_physiological_parameter_rank_stability(pad_prevalence) %>% 
        apply_vte_physiological_parameter_rank_stability(vte_prevalence) %>% 
        apply_af_physiological_parameter_rank_stability(af_prevalence)

      if(time %/%2==0){
          
      print('updating morbidity risk every 2 years')
        print(2023%in%count(current_population,rheumatoid_arthritis)$rheumatoid_arthritis)
        
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
      
    }
    
    current_population_who_died <- current_population %>%
      filter( !is.na(death) & !is.null(death) & !death==0 )

    dead_population <- bind_rows(dead_population, current_population_who_died)

    current_population_alive <- current_population %>%
      filter(is.na(death)| is.null(death)| death==0)

    current_population <- current_population_alive

    print('Adding the current population to the past populations data structure')
    past_populations <- bind_rows(past_populations, current_population%>%
                                    mutate(run = run1))
    
  }
  
  
}

message('main 2.4 done')

# count(past_populations,year,run)
# count(past_populations,year,run,asthma)
# count(past_populations,year,run,stroke)
# count(past_populations,year,asthma)

# count(past_populations,year,colorectal_cancer)

# filter(past_populations,id == 46615) %>% select(year,run,stroke,chd,chronic_kidney_disease)

# print(paste('Adding incidence of run',run,'over', time,'years to holding dataframe'))

# #1
# stroke_incidence <- show_incidence(current_population, stroke_year_risk, stroke, mdm_quintile, year, HSCT, sex, age20, run) %>% #LGD2014NAME,
#   mutate(morbidity = 'stroke')

# stroke_outcome_incidence <- rbind(stroke_outcome_incidence,stroke_incidence)

# #2  
# diabetes_incidence <- show_incidence(current_population, hypertension_year_risk, hypertension, mdm_quintile, year, HSCT, sex, age20, run) %>% #LGD2014NAME,
#   mutate(morbidity = 'diabetes')

# diabetes_outcome_incidence <- rbind(diabetes_outcome_incidence, diabetes_incidence)

# #3  
# chd_incidence <- show_incidence(current_population, chd_year_risk, chd, mdm_quintile, year, HSCT, sex, age20, run) %>% #LGD2014NAME,
#   mutate(morbidity= 'chd')

# chd_outcome_incidence <- rbind(chd_outcome_incidence, chd_incidence)

# #4  
# af_incidence <- show_incidence(current_population, atrial_fibrillation_year_risk, atrial_fibrillation, mdm_quintile, year, HSCT, sex, age20, run) %>% #LGD2014NAME,
#   mutate(morbidity='atrial_fibrillation')

# af_outcome_incidence <- rbind(af_outcome_incidence, af_incidence)

# #5
# hypertension_incidence <- show_incidence(current_population, hypertension_year_risk, hypertension, mdm_quintile, year, HSCT, sex, age20, run) %>% #LGD2014NAME,
#   mutate(morbidity = 'hypertension')
# hypertension_outcome_incidence <- rbind(hypertension_outcome_incidence, hypertension_incidence)

# #6
# ckd_incidence <- show_incidence(current_population, chronic_kidney_disease_year_risk, chronic_kidney_disease, mdm_quintile, year, HSCT, sex, age20, run) %>% #LGD2014NAME,
#   mutate(morbidity = 'chronic_kidney_disease')

# ckd_outcome_incidence <- rbind(ckd_outcome_incidence, ckd_incidence)

# #7
# lungcancer_incidence <- show_incidence(current_population,lung_cancer_year_risk,lung_cancer, year,mdm_quintile, HSCT,sex,age20,run) %>% 
#   mutate(morbidity='lung_cancer')

# lungcancer_outcome_incidence <- rbind(lungcancer_outcome_incidence, lungcancer_incidence)

# #8
# dementia_incidence <- show_incidence(current_population,dementia_year_risk,dementia,  year, mdm_quintile,  HSCT, sex, age20, run) %>% 
#   mutate(morbidity = 'dementia')

# dementia_outcome_incidence <- rbind(dementia_outcome_incidence, dementia_incidence)

# #9
# heart_failure_incidence <- show_incidence(current_population, heart_failure_year_risk, heart_failure,  year, mdm_quintile,  HSCT, sex, age20, run) %>% 
#   mutate(morbidity = 'heart_failure')

# heart_failure_outcome_incidence <- rbind(heart_failure_outcome_incidence, heart_failure_incidence)

# #10
# heart_failure_incidence <- show_incidence(current_population, heart_failure_year_risk, heart_failure,  year, mdm_quintile,  HSCT, sex, age20, run) %>% 
#   mutate(morbidity = 'heart_failure')
 
# heart_failure_outcome_incidence <- rbind(heart_failure_outcome_incidence, heart_failure_incidence)

# #11
# asthma_incidence <- show_incidence(current_population, asthma_year_risk, heart_failure,  year, mdm_quintile,  HSCT, sex, age20, run) %>% 
#   mutate(morbidity = 'heart_failure')

# asthma_outcome_incidence <- rbind(asthma_outcome_incidence, asthma_incidence)

# #12
# copd_incidence <- show_incidence(current_population, copd_year_risk, heart_failure,  year, mdm_quintile,  HSCT, sex, age20, run) %>% 
#   mutate(morbidity = 'heart_failure')

# copd_outcome_incidence <- rbind(copd_outcome_incidence, copd_incidence)

# #13
# non_diabetic_hyperglycaemia_incidence <- show_incidence(current_population, non_diabetic_hyperglycaemia_year_risk, heart_failure,  year, mdm_quintile,  HSCT, sex, age20, run) %>% 
#   mutate(morbidity = 'heart_failure')

# non_diabetic_hyperglycaemia_outcome_incidence <- rbind(non_diabetic_hyperglycaemia_outcome_incidence, non_diabetic_hyperglycaemia_incidence)

# #14
# depression_incidence <- show_incidence(current_population, depression_year_risk, heart_failure,  year, mdm_quintile,  HSCT, sex, age20, run) %>% 
#   mutate(morbidity = 'heart_failure')

# depression_outcome_incidence <- rbind(depression_outcome_incidence, depression_incidence)

