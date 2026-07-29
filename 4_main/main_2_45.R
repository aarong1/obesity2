library(tidyverse)
library(DBI)
library(fst)
library(data.table)

library(foreach)
library(doParallel)
library(fst)

registerDoParallel(4L)
threads_fst(5)
data.table::setDTthreads(5) #this is so that don't use all the processors

source('./1_2_utils/main_configuration.R') # model_specification list
source('./1_2_utils/main_utils.R')

source('./1_2_utils/prevalence_qof.R')
# source('./1_2_utils/disease_prevalence.R')

source('./1_2_utils/reindex_risk_percentile.R')
source('./1_2_utils/obesity_intervention/engine_bmi.R')

# source('./3_pre_main/main_duckdb.R')

source('./risk_exposure_prevalence/apply_ckd_physiological_parameter_rank_stability.R') 
source('./risk_exposure_prevalence/apply_pad_physiological_parameter_rank_stability.R')
source('./risk_exposure_prevalence/apply_vte_physiological_parameter_rank_stability.R')
source('./risk_exposure_prevalence/apply_af_physiological_parameter_rank_stability.R')

source('./risk_exposure_prevalence/diet/apply_diet_lifestyle_parameter_rank_stability_long_form.R')
source('./risk_exposure_prevalence/bmi/apply_bmi_lifestyle_parameter_3State_rank_stability_long_form.R')
source('./risk_exposure_prevalence/apply_child_bmi_lifestyle_parameter_3State.R')
source('./risk_exposure_prevalence/alcohol/apply_alcohol_lifestyle_parameter_rank_stability_long_form.R')
source('./risk_exposure_prevalence/smoking/apply_smoking_lifestyle_parameter_rank_stability_long_form.R')
source('./risk_exposure_prevalence/ecigarettes/apply_ecigarette_lifestyle_parameter_rank_stability_long_form.R')
source('./risk_exposure_prevalence/PA/apply_pa_lifestyle_parameter_rank_stability_long_form.R')
source('./risk_exposure_prevalence/wellbeing_depression/apply_wellbeing_depression_lifestyle_parameter_rank_stability_long_form.R')

source('./risk_exposure_prevalence/diabetes/apply_diabetes_physiological_parameter_rank_stability_long_form.R')
source('./risk_exposure_prevalence/cholesterol/apply_cholesterol_physiological_parameter_rank_stability_long_form.R')
source('./risk_exposure_prevalence/hypertension/apply_hypertension_physiological_parameter_rank_stability_long_form.R')

source('./risk_exposure_prevalence/apply_pollution_risk_environmental_risk_factor.R')
source('./risk_exposure_prevalence/apply_sleep_apnea.R')

source('risk_exposure_prevalence/indicators/apply_general_health_parameter_rank_stability_long_form.R')
source('risk_exposure_prevalence/indicators/apply_loneliness_parameter_rank_stability_long_form.R')
source('risk_exposure_prevalence/indicators/apply_long_term_parameter_rank_stability_long_form.R')
source('risk_exposure_prevalence/indicators/apply_long_term_limiting_parameter_rank_stability_long_form.R')

# Incidence -----
# source('./main/pre_main_2.4.R')

source('./disease_engines/stroke_engine.R')
source('./disease_engines/chd_engine.R')
source('./disease_engines/heart_failure_engine.R')
source('./disease_engines/dementia_engine.R')
source('./disease_engines/diabetes_engine.R')
source('./disease_engines/kidney_disease_engine.R')

source('./disease_engines/atrial_fibrillation_engine.R')
source('./disease_engines/hypertension_engine.R')
source('./disease_engines/diabetes_type_1_engine.R')

source("./disease_engines/asthma_engine.R")
source("./disease_engines/copd_engine.R")

source("./disease_engines/aortic_aneurysm_engine.R")

source("./disease_engines/epilepsy_engine.R")
source('./disease_engines/hypothyroid_engine.R')
source("./disease_engines/RA_engine.R")
source("./disease_engines/osteoporosis_engine.R")
source("./disease_engines/osteoarthritis_engine.R")

source('./disease_engines/fracture_engine.R')
source('./disease_engines/Intersistal_lung_disease_engine.R')
source('./disease_engines/ibd_engine.R')
source('./disease_engines/systemic_lupus_erythematosus_engine.R')

source('./disease_engines/alcoholic_related_liver_disease.R')
source('./disease_engines/non_alcoholic_fatty_liver_disease.R')

source('./disease_engines/cancer_engine.R')

source('./disease_engines/cancer/brain_cancer.R')
source('./disease_engines/cancer/cervical_cancer.R')
source('./disease_engines/cancer/colorectal_cancer_engine.R')
source('./disease_engines/cancer/female_breast_engine.R')
source('./disease_engines/cancer/gallbladder_cancer_engine.R')
source('./disease_engines/cancer/kidney_cancer_engine.R')

source('./disease_engines/cancer/lung_cancer_engine.R')

source('./disease_engines/cancer/oesophageal_cancer.R')
source('./disease_engines/cancer/oral_cancer_engine.R')
source('./disease_engines/cancer/ovarian_cancer.R')
source('./disease_engines/cancer/pancreatic_cancer.R')

source('./disease_engines/cancer/prostate_cancer.R')
source('./disease_engines/cancer/uterine_cancer.R')

read_theoretical_minimum_files()
read_joint_distribution_files()

# Deaths ----
# source('./Births_module/births.R')

fatality_wide <- read.fst('./1_2_utils/data/wide_fatality.fst')
other_deaths_df <- read.fst('./1_2_utils/data/other_deaths_df.fst')

# source('./3_pre_main/pre_main_35_deaths.R')
# source('./3_pre_main/pre_main_deaths_15.R')
source('./3_pre_main/main_deaths_dt.R')

# source('./deaths_module/apply_age_sex_death.R')

# Births ----
# source('./Births_module/births.R')
# source('./births_module/births_by_fertility_projections.R')


source('./1_2_utils/main_configuration.R') # model_specification list
model_specification$population$scale_down_factor = model_specification$population$scale_down_factor/0.01
model_specification$model$duration = 5#12
model_specification$model$number_of_runs = 5

function(){
# initial_time_zero_population <- x1
target_populations <- data.frame() #initial_time_zero_population) # REMOVED - now using DuckDB

con <- dbConnect(duckdb::duckdb(), dbdir ='past_populations_db/past_populations.duckdb', read_only = FALSE); con
cat("DuckDB database initialized for storing population data\n")

latest_tbl <- sort(decreasing = T,dbListTables(con))[1]

# x <- dbSendQuery(con, paste0('SELECT * FROM past_populations.',latest_tbl,' USING SAMPLE 60 PERCENT (bernoulli);'))  # Set cache size to 2MB
x <- dbSendQuery(
  con,
  paste0(
    "SELECT *
     FROM past_populations.", latest_tbl, "
     WHERE year = (
       SELECT MIN(year)
       FROM past_populations.", latest_tbl, "
     );"
  )
)

# past_populations_20260116_015236

first_population <- dbFetch(x)
count(first_population,death_reason,age20,year)

dbClearResult(x)
dbDisconnect(con, shutdown=TRUE)
write.fst(first_population, './3_pre_main/intermediate_populations/first_population.fst')
}

first_population <- read.fst( './3_pre_main/intermediate_populations/first_population.fst')

setDT(first_population)
# past_populations <- read.fst( './3_pre_main/intermediate_populations/full_history_past_populations.fst')
# past_populations[year == min(year)]

# first_population$year <- first_population$year - 1
first_population$age <- first_population$age - 1

first_population[,.N,by=target]

first_population <- first_population[target == TRUE, ]

for(run1 in 1:(model_specification$model$number_of_runs)) { 
  
  cat(paste('################################### \n run : ', run1, ' \n###################################### \n'))
  
  # base_population <- populate_all_cancers_prevalence(initial_time_zero_population)
  # 
  # base_population <- populate_prostate_cancer(base_population)
  # base_population <- populate_female_breast_cancer(base_population)
  # base_population <- populate_renal_cancer_prevalence(base_population)
  # base_population <- populate_oesophageal_cancer_prevalence(base_population)
  # base_population <- populate_stomach_cancer_prevalence(base_population)
  # 
  # base_population <- base_population %>% 
  #   mutate(osteogastric_cancer = case_when( oesophageal_cancer != 0 ~ min(year), 
  #                                           stomach_cancer != 0 ~ min(year),
  #                                           T ~0 ))
  # # count(base_population,stomach_cancer,oesophageal_cancer,osteogastric_cancer)
  # 
  # base_population <- populate_oral_cancer_prevalence(base_population)
  # base_population <- populate_pancreatic_cancer_prevalence(base_population)
  # base_population <- populate_uterine_cancer_prevalence(base_population);count(base_population,uterine_cancer)
  # 
  # base_population <- populate_blood_multiple_myeloma_prevalence(base_population)
  # base_population <- populate_blood_lymphoma_prevalence(base_population)
  # base_population <- populate_blood_leukaemia_prevalence(base_population)
  # 
  # base_population <- base_population %>% 
  #   mutate(blood_cancer = case_when( blood_multiple_myeloma != 0 ~ min(year),
  #                                    blood_lymphoma != 0 ~ min(year),
  #                                    blood_leukaemia != 0 ~ min(year),
  #                                    T ~ 0  ))
  # 
  # base_population <- populate_ovarian_cancer(base_population)
  # base_population <- populate_colorectal_cancer_prevalence(base_population)
  # base_population <- populate_lung_cancer_prevalence(base_population)
  # 
  # 
  # 
  # base_population <- apply_qof_prevalence(base_population)
  # 
  # base_population <- populate_copd(base_population)
  # base_population <- populate_stroke(base_population)
  # base_population <- populate_dementia(base_population)
  # 
  # base_population <- apply_nafld_disease_prevalence(base_population) 
  # base_population <- apply_arld_prevalence(base_population)  
  # base_population <- apply_ild_prevalence(base_population) 
  # base_population <- populate_aortic_aneursym(base_population) 
  # base_population <- apply_sle_prevalence(base_population) 
  # base_population <- populate_inflammatory_bowel_disease(base_population) 
  # 
  # base_population <- apply_osteoarthritis_prevalence(base_population ) 
  # base_population <- populate_epilepsy_prevalence(base_population)
  # 
  # population_w_established_prevalence <- base_population %>% 
  #   apply_doh_disease_prevalence(morbidity = 'osteoporosis') %>% 
  #   apply_doh_disease_prevalence(morbidity = 'hypothyroidism') %>% 
  #   apply_doh_disease_prevalence(morbidity = 'pad') 
  # 
  # current_population <- population_w_established_prevalence %>% 
  #   mutate(run = run1)
  #   target_populations <- bind_rows(target_populations, current_population%>%
  #         mutate(run = run1))
  # current_population <- current_population %>% 
  #   mutate(bern_trial = runif(n()))
  # 
  
  # ############################################################################
  
  current_population <- first_population %>% filter(run == run1)
  
  
  target_populations <- bind_rows(target_populations, current_population%>%
                                  mutate(run = run1))
  
  
  current_population_alive <- current_population %>%
    filter( death_reason == 'survive')
  
  current_population <- current_population_alive
 
  ############################################################################
  
  for (time in 1:(model_specification$model$duration-1)){
  #   
   current_population_alive <- current_population %>%
    filter( death_reason == 'survive')

  current_population <- current_population_alive
  
    # time=1
    cat(paste('###################################### \n Time, t : ', time, '\n Run, r:', run1,'\n###################################### \n'))
    
    current_population <- current_population %>%
      mutate(age = age + 1) %>% 
      mutate(year = year + 1) %>% 
      mutate(
        age20 = cut(age, include.lowest = T,
                    breaks = seq(0,120,20),
                    labels = c('0-20',
                               '20-40',
                               '40-60',
                               '60-80',
                               '80-100',
                               '100-120')
        ),
        ageKid = cut(age, include.lowest = T,
                     breaks = c(-Inf, 1, 10, 15, Inf),
                     labels = c('0-1', '2-10', '11-15', NA),
                     right = TRUE),
        age10 = cut(age, include.lowest = T,
                    breaks = c(-Inf, 15, 34, 44, 54, 64, 74, Inf),
                    labels = c('0-15', '16-34', '35-44', '45-54', '55-64', '65-74', '75-110'),
                    right = TRUE),
        age_band_death10 = cut(age, include.lowest = T,
                               breaks = c(-Inf, 9, 19, 29, 39, 49, 59, 69, 79, 89, Inf),
                               labels = c('1-9', '10-19', '20-29', '30-39', '40-49', '50-59', '60-69', '70-79', '80-89', '90+'),
                               right = TRUE)
      )
    
    if(time%%3 == 1){
      
      # current_population <- reindex_risk_percentile(current_population)
      
      print('Recalibrating risk percentiles and Risk States every 3 years')
      
      current_population <- current_population %>%
        reduce_bmi_percentile(subset=T)

      current_population <- current_population %>% 
        apply_bmi_lifestyle_parameter_3State_rank_stability(bmi_stratified_prevalence) %>% 
        apply_child_bmi_lifestyle_parameter_3State_rank_stability(child_bmi_stratified_prevalence) %>% 
        combine_child_adult_bmi() %>% 
        apply_smoking_lifestyle_parameter_rank_stability(smoking_results_df) %>%
        apply_alcohol_lifestyle_parameter_rank_stability(alcohol_stratified_prevalence) %>%
        apply_diet_lifestyle_parameter_rank_stability(diet_stratified_prevalence) %>%
        apply_pa_lifestyle_parameter_rank_stability(pa_stratified_prevalence) %>%
        apply_wellbeing_depression_lifestyle_parameter_rank_stability(wellbeing_stratified_prevalence) %>% 
        #count(wellbeing)
        apply_pollution_lifestyle_parameter_geography_constant(lookup_dz_raster_cell) %>% 
        
        apply_sleep_lifestyle_parameter_rank_stability() %>% 
        
        apply_cholesterol_physiological_parameter_rank_stability(cholesterol_stratified_prevalence) %>%
        apply_granular_cholesterol_measure_posthoc_overlay(special_cholesterol) %>% 
        apply_hypertension_physiological_parameter_rank_stability(hypertension_stratified_prevalence) %>%
        apply_diabetes_physiological_parameter_rank_stability(diabetes_stratified_prevalence) %>% 
        apply_ckd_physiological_parameter_rank_stability(ckd_prevalence) %>% 
        apply_pad_physiological_parameter_rank_stability(pad_prevalence) %>% 
        apply_vte_physiological_parameter_rank_stability(vte_prevalence) %>% 
        apply_af_physiological_parameter_rank_stability(af_prevalence)
      
      current_population <- current_population %>% 
        apply_general_health_indicator_parameter_rank_stability(general_health_stratified_prevalence) %>%
        apply_loneliness_indicator_parameter_rank_stability(loneliness_stratified_prevalence) %>%
        apply_long_term_limiting_indicator_parameter_rank_stability(limiting_stratified_prevalence) %>%
        apply_long_term_indicator_parameter_rank_stability(disability_stratified_prevalence)
      
    }
    


    current_population <- apply_cervical_risk_factors(current_population, cervical_theoretical_minimum)
    current_population <- apply_crc_risk_factors(current_population, crc_theoretical_minimum)
    
    current_population <- apply_breast_risk_factors(current_population, breast_theoretical_minimum)
    
    current_population <- apply_gallbladder_risk_factors(current_population, gallbladder_theoretical_minimum)
    current_population <- apply_kidney_cancer_risk_factors(current_population, kidney_cancer_theoretical_minimum)
    
    current_population <- apply_lung_cancer_risk_factors(current_population, lung_cancer_theoretical_minimum)
    
    current_population <- apply_oesophageal_risk_factors(current_population, oesophageal_theoretical_minimum)
    current_population <- apply_oral_risk_factors(current_population, oral_theoretical_minimum)
    current_population <- apply_ovarian_risk_factors(current_population, ovarian_theoretical_minimum)
    current_population <- apply_pancreatic_risk_factors(current_population, pancreatic_theoretical_minimum)
    current_population <- apply_uterine_risk_factors(current_population, uterine_theoretical_minimum)
    
    current_population <- apply_prostate_risk_engine_age_sex(current_population )
    current_population <- apply_brain_cancer_risk_wo_risk_factors(current_population )
    
    current_population <- apply_af_risk_factors(current_population, af_theoretical_minimum)
    current_population <- apply_chd_risk_factors(current_population, chd_theoretical_minimum)
    current_population <- apply_heart_failure_risk_factors(current_population, heart_failure_theoretical_minimum)
    current_population <- apply_stroke_risk_factors(current_population, stroke_theoretical_minimum)
    current_population <- apply_diabetes_risk_factors(current_population, diabetes_theoretical_minimum)
    
    current_population <- apply_dementia_risk_factors(current_population, dementia_theoretical_minimum)
    current_population <- apply_kidney_disease_risk_factors(current_population, kidney_disease_theoretical_minimum)
    
    current_population <- apply_asthma_risk_factors(current_population, asthma_theoretical_minimum)
    current_population <- apply_copd_risk_factors(current_population, copd_theoretical_minimum)
    
    current_population <- apply_aaa_risk_factors(current_population,aaa_theoretical_minimum )
    
    current_population <- apply_osteoporotic_fracture_risk_factors(current_population, osteoporotic_fracture_theoretical_minimum)
    current_population <- apply_hip_fracture_risk_factors(current_population, hip_fracture_theoretical_minimum)
    
    current_population <- apply_ra_risk_factors(current_population, rheumatoid_arthritis_theoretical_minimum)
    current_population <- apply_oa_risk_factors(current_population, osteoarthritis_theoretical_minimum )
    
    current_population <- apply_type1_diabetes_risk_engine_age_sex(current_population)
    
    current_population <- apply_hypothyroid_risk(current_population )
    current_population <- apply_epilepsy_risk(current_population )
    
    current_population <- apply_ILD_risk(current_population )
    current_population <- apply_sle_risk(current_population )
    
    current_population <-  apply_ibd_risk(current_population )
    
    current_population <- apply_osteoporosis_risk(current_population )
    
    current_population <- apply_arld_risk(current_population)
    current_population <- apply_nafld_risk(current_population)
    
    ###########################################
    ###########################################
    
    print('Applying absolute morbidity onset')
    
    current_population <- current_population %>% 
      declare_absolute_incident_morbidity_alt(morbidity = "stroke") %>% 
      declare_absolute_incident_morbidity_alt(morbidity = "chd") %>% 
      declare_absolute_incident_morbidity_alt(morbidity = "diabetes") %>% 
      declare_absolute_incident_morbidity_alt(morbidity = "dementia") %>% 
      declare_absolute_incident_morbidity_alt(morbidity = "heart_failure") %>% 
      declare_absolute_incident_morbidity_alt(morbidity = "atrial_fibrillation") %>% 
      declare_absolute_incident_morbidity_alt(morbidity = "hypertension") %>% 
      declare_absolute_incident_morbidity_alt(morbidity = "chronic_kidney_disease") %>% 
      declare_absolute_incident_morbidity_alt(morbidity = "lung_cancer") %>%
      declare_absolute_incident_morbidity_alt(morbidity = "asthma") %>%
      declare_absolute_incident_morbidity_alt(morbidity = "copd") %>%
      declare_absolute_incident_morbidity_alt(morbidity = "depression") %>%
      declare_absolute_incident_morbidity_alt(morbidity = "non_diabetic_hyperglycaemia") %>%
      declare_absolute_incident_morbidity_alt(morbidity = "colorectal_cancer") %>%
      declare_absolute_incident_morbidity_alt(morbidity = "rheumatoid_arthritis") %>%
      declare_absolute_incident_morbidity_alt(morbidity = "osteoarthritis") %>%
      declare_absolute_incident_morbidity_alt(morbidity = "osteoporosis") %>% 
      declare_absolute_incident_morbidity_alt(morbidity = "hypothyroidism") %>% 
      declare_absolute_incident_morbidity_alt(morbidity = "pad") %>%
      declare_absolute_incident_morbidity_alt(morbidity = "epilepsy") #%>%
    # declare_absolute_incident_morbidity_alt(morbidity = "ibd") %>%
    # declare_absolute_incident_morbidity_alt(morbidity = "sle") %>%
    # declare_absolute_incident_morbidity_alt(morbidity = "AAA") #%>%
    # declare_absolute_incident_morbidity_alt(morbidity = "liver_disease")
    
    # current_population$osteogastric_cancer <- NA
    # current_population$renal_cancer <- NA
    
    current_population <- current_population %>% 
      
      # "lung_cancer"               
      # declare_absolute_incident_morbidity_alt("stomach_cancer") %>%
      declare_absolute_incident_morbidity_alt("cancer") %>%
      declare_absolute_incident_morbidity_alt("osteogastric_cancer") %>%
      declare_absolute_incident_morbidity_alt("prostate_cancer") %>% 
      declare_absolute_incident_morbidity_alt("female_breast_cancer") %>%      
      declare_absolute_incident_morbidity_alt("renal_cancer") %>% 
      # declare_absolute_incident_morbidity_alt("oesophageal_cancer") %>%        
      # declare_absolute_incident_morbidity_alt("oesphageal_gastric_cancer") %>% 
      declare_absolute_incident_morbidity_alt("oral_cancer") %>%               
      declare_absolute_incident_morbidity_alt("pancreatic_cancer") %>% 
      declare_absolute_incident_morbidity_alt("uterine_cancer") %>%            
      declare_absolute_incident_morbidity_alt("blood_cancer") %>%              
      declare_absolute_incident_morbidity_alt("ovarian_cancer") %>% 
      declare_absolute_incident_morbidity_alt("brain_cancer") %>%              
      declare_absolute_incident_morbidity_alt("cervical_cancer") 
    
    current_population <- current_population %>%
      apply_deaths_modelled_deaths(fatality_wide = fatality_wide, other_deaths_df = other_deaths_df)

    #########################################################
    target_populations <- bind_rows(target_populations, current_population %>%
                                    mutate(run = run1))
    #########################################################
    
    print(' Partition Apply deaths')

    # current_population_alive <- current_population %>%
    #   filter( death_reason == 'survive')
    # 
    # current_population <- current_population_alive
    
  }
  # target_populations <- bind_rows(target_populations, current_population %>%
  #                                 mutate(run = run1))
}


message('main 2.4d done')

