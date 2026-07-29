library(tidyverse)
library(magrittr)
library(data.table)
library(readxl)
library(fst)

#################### Version Control Comments #######################
########################    WHATS CHANGED   #########################  

# A major re release of this module was determined because of:

#   1. The new data file from 2011 census of population counts by SYA and gender.
#   2. Finally partitioning and holding death and death reason
#   3. Calculating prevalence as well as incidence, whether these are
#        calculated together is yet to be determined

###############################################
############# SYNTHETIC POPULATION ############
source('./2_synthetic_population/1_10_temp_synthetic_population.R')

###############################################
############ Model Specification ##############
###############################################

source('./1_2_utils/main_utils.R') # model_specification list
source('./1_2_utils/main_configuration.R') # model_specification list
#source('./Births_module/births_by_fertility_projections.R')

###############################################
############ Population Actualization #########
###############################################

base_population <- instantiate_base_pop( model_specification)

# ~/Documents/SIB/PHM/PHModel/synthetic_population/age_synthetic_population.R
# save_yearly_populations <- read.fst('./births_module/yearly_start_populations.fst')

# base_population <- save_yearly_populations %>% 
#   filter(year == model_specification$model$start_year)

base_population <- base_population %>% 
  mutate(SETTLEMENT2015_name = ifelse(is.na(SETTLEMENT2015_name),
                                      paste(HSCT,'country'),
                                      SETTLEMENT2015_name))

base_population <- base_population %>% 
  mutate( mdm_quintile_soa_verbose = case_when(mdm_quintile_soa_name == 'Most Deprived'~  'Most Deprived',
                                                 mdm_quintile_soa_name == 'Quintile 2' ~ '2nd Most deprived',
                                                 mdm_quintile_soa_name == 'Quintile 3' ~ 'Middle',
                                                 mdm_quintile_soa_name == 'Quintile 4' ~ '2nd Least Deprived',
                                                 mdm_quintile_soa_name == 'Least Deprived'~ 'Least Deprived'
  ))


## Add risk factors -----------------------------------

base_population_w_correlated_percentiles <- apply_correlated_quantiles(base_population,
                                                                       risks_to_include = c('bmi',
                                                                                            'smoking',
                                                                                            'depression',
                                                                                            'alcohol',
                                                                                            'diet',
                                                                                            'sleep',
                                                                                            'hypertension', 
                                                                                            'diabetes',
                                                                                            'cholesterol',
                                                                                            'physical_activity'), #depression is linked to wellbeing#
                                                                       model_configuration_list = model_specification)

write.fst(base_population_w_correlated_percentiles,
          paste0('./3_pre_main/intermediate_populations/base_population_w_correlated_percentiles.fst'))


### Simple two state transition probability strategy -----------------------------------

# base_population_w_risk_factors <- base_population %>%
#  apply_smoking_lifestyle_parameter() %>%
#  apply_cholesterol_physiological_parameter() %>%
#  apply_hypertension_physiological_parameter() %>%
#  apply_diabetes_physiological_parameter() %>%
#  apply_atrial_fibrillation_physiological_parameter() %>%
#  apply_bmi_lifestyle_parameter() 

#names(base_population_w_correlated_percentiles)

# dir( full.names = TRUE, './joint_estimation' ) %>% 
#   sapply(.,source)

# source('./joint_estimation/smoking.R') 
# source('./joint_estimation/alcohol.R') 

# source('./joint_estimation/adult_obesity.R') 
# source('./joint_estimation/child_obesity.R') 

# source('./joint_estimation/PA.R') 
# source('./joint_estimation/5veg.R')
# source('./joint_estimation/wellbeing.R')

source('./risk_joint_estimation/smoking_2.R') #check
source('./risk_joint_estimation/alcohol.R') #check
source('./risk_joint_estimation/adult_obesity.R') #check
source('./risk_joint_estimation/child_obesity.R') #check
source('./risk_joint_estimation/PA.R') 
source('./risk_joint_estimation/5veg.R')
source('./risk_joint_estimation/wellbeing.R')

source('./risk_joint_estimation/electronic_cigarettes.R')

source('./risk_joint_estimation/hypertension.R') #checks
source('./risk_joint_estimation/diabetes.R')
source('./risk_joint_estimation/cholesterol.R') #check
source('./risk_joint_estimation/cholesterol_granular.R')

# source('./risk_joint_estimation/VTE.R')
# source('./risk_joint_estimation/PAD.R')
# source('./risk_joint_estimation/chronic_kidney_disease.R')
# source('./risk_joint_estimation/atrial_fibrillation.R')

source('./risk_exposure_prevalence/diet/apply_diet_lifestyle_parameter_rank_stability_long_form.R')
source('./risk_exposure_prevalence/bmi/apply_bmi_lifestyle_parameter_3State_rank_stability_long_form.R')
source('./risk_exposure_prevalence/apply_child_bmi_lifestyle_parameter_3State.R')
source('./risk_exposure_prevalence/alcohol/apply_alcohol_lifestyle_parameter_rank_stability_long_form.R')
source('./risk_exposure_prevalence/smoking/apply_smoking_lifestyle_parameter_rank_stability_long_form.R')
source('./risk_exposure_prevalence/ecigarettes/apply_ecigarette_lifestyle_parameter_rank_stability_long_form.R')
source('./risk_exposure_prevalence/PA/apply_pa_lifestyle_parameter_rank_stability_long_form.R')
source('./risk_exposure_prevalence/wellbeing_depression/apply_wellbeing_depression_lifestyle_parameter_rank_stability_long_form.R')

source('./risk_exposure_prevalence/apply_sleep_apnea.R')


########################################################
########################################################

source('./risk_exposure_prevalence/diabetes/apply_diabetes_physiological_parameter_rank_stability_long_form.R')
source('./risk_exposure_prevalence/cholesterol/apply_cholesterol_physiological_parameter_rank_stability_long_form.R')
# source('./risk_exposure_prevalence/apply_cholesterol_physiological_parameter.R')

source('./risk_exposure_prevalence/hypertension/apply_hypertension_physiological_parameter_rank_stability_long_form.R')
source('./usecases/pollution/apply_pollution_risk_environmental_risk_factor.R')

###  Two/three stability rank strategy -----------------------------------

base_population_w_modifiable_risk_factors <- base_population_w_correlated_percentiles %>%
  #bmi
  apply_bmi_lifestyle_parameter_3State_rank_stability(bmi_stratified_prevalence) %>% 
  apply_child_bmi_lifestyle_parameter_3State_rank_stability(child_bmi_stratified_prevalence) %>% 
  combine_child_adult_bmi() %>% 
  #smoking
  apply_smoking_lifestyle_parameter_rank_stability(smoking_results_df) %>%
  apply_ecigarette_lifestyle_parameter_rank_stability(ecigarette_stratified_prevalence) %>% 
  # alcohol
  apply_alcohol_lifestyle_parameter_rank_stability(alcohol_stratified_prevalence) %>% 
  apply_diet_lifestyle_parameter_rank_stability(diet_stratified_prevalence) %>% 
  apply_pa_lifestyle_parameter_rank_stability(pa_stratified_prevalence) %>%
  apply_wellbeing_depression_lifestyle_parameter_rank_stability(wellbeing_stratified_prevalence) %>% #count(wellbeing)
  apply_pollution_lifestyle_parameter_geography_constant(lookup_dz_raster_cell) %>%  
  apply_sleep_lifestyle_parameter_rank_stability() 

###

base_population_w_physiological_and_modifiable_risk_factors <- base_population_w_modifiable_risk_factors %>% 
  apply_cholesterol_physiological_parameter_rank_stability(cholesterol_stratified_prevalence) %>%
  apply_hypertension_physiological_parameter_rank_stability(hypertension_stratified_prevalence) %>%
  apply_diabetes_physiological_parameter_rank_stability(diabetes_stratified_prevalence)

base_population_w_physiological_and_modifiable_risk_factors <- apply_granular_cholesterol_measure_posthoc_overlay(base_population_w_physiological_and_modifiable_risk_factors,special_cholesterol)

###

source('./risk_joint_estimation/limiting_long_term_disability.R')
source('./risk_joint_estimation/long_term_disability.R')
source('./risk_joint_estimation/loneliness.R')
source('./risk_joint_estimation/general_health.R')

source('risk_exposure_prevalence/indicators/apply_general_health_parameter_rank_stability_long_form.R')
source('risk_exposure_prevalence/indicators/apply_loneliness_parameter_rank_stability_long_form.R')
source('risk_exposure_prevalence/indicators/apply_long_term_parameter_rank_stability_long_form.R')
source('risk_exposure_prevalence/indicators/apply_long_term_limiting_parameter_rank_stability_long_form.R')

base_population_w_physiological_and_modifiable_risk_factors <- base_population_w_physiological_and_modifiable_risk_factors %>%
  mutate(general_health_percentile = runif(n())) %>%
  mutate(loneliness_percentile = runif(n())) %>%
  mutate(limiting_percentile = runif(n())) %>%
  mutate(disability_percentile = runif(n()))

base_population_w_physiological_and_modifiable_risk_factors <- base_population_w_physiological_and_modifiable_risk_factors %>% 
  apply_general_health_indicator_parameter_rank_stability(general_health_stratified_prevalence) %>%
  apply_loneliness_indicator_parameter_rank_stability(loneliness_stratified_prevalence) %>%
  apply_long_term_limiting_indicator_parameter_rank_stability(limiting_stratified_prevalence) %>%
  apply_long_term_indicator_parameter_rank_stability(disability_stratified_prevalence)



base_population_w_physiological_and_modifiable_risk_factors$stroke = 0
base_population_w_physiological_and_modifiable_risk_factors$heart_failure = 0
base_population_w_physiological_and_modifiable_risk_factors$diabetes = 0
base_population_w_physiological_and_modifiable_risk_factors$non_diabetic_hyperglycaemia = 0
base_population_w_physiological_and_modifiable_risk_factors$chd = 0
base_population_w_physiological_and_modifiable_risk_factors$dementia = 0
base_population_w_physiological_and_modifiable_risk_factors$chronic_kidney_disease = 0
base_population_w_physiological_and_modifiable_risk_factors$atrial_fibrillation = 0
base_population_w_physiological_and_modifiable_risk_factors$hypertension = 0
base_population_w_physiological_and_modifiable_risk_factors$copd = 0
base_population_w_physiological_and_modifiable_risk_factors$asthma = 0

base_population_w_physiological_and_modifiable_risk_factors$depression = 0
base_population_w_physiological_and_modifiable_risk_factors$pad = 0
base_population_w_physiological_and_modifiable_risk_factors$vte = 0

base_population_w_physiological_and_modifiable_risk_factors$epilepsy = 0
base_population_w_physiological_and_modifiable_risk_factors$hypothyroidism = 0
base_population_w_physiological_and_modifiable_risk_factors$osteoporosis = 0
base_population_w_physiological_and_modifiable_risk_factors$rheumatoid_arthritis = 0

base_population_w_physiological_and_modifiable_risk_factors$abdominal_aortic_aneurysm = 0
base_population_w_physiological_and_modifiable_risk_factors$nafld_disease = 0
base_population_w_physiological_and_modifiable_risk_factors$arld_disease = 0
base_population_w_physiological_and_modifiable_risk_factors$ibd = 0
base_population_w_physiological_and_modifiable_risk_factors$sle = 0
base_population_w_physiological_and_modifiable_risk_factors$interstitial_lung_disease = 0
base_population_w_physiological_and_modifiable_risk_factors$osteoarthritis = 0
base_population_w_physiological_and_modifiable_risk_factors$fracture4 = 0
base_population_w_physiological_and_modifiable_risk_factors$nof = 0


base_population_w_physiological_and_modifiable_risk_factors$cervical_cancer = 0
base_population_w_physiological_and_modifiable_risk_factors$prostate_cancer = 0
base_population_w_physiological_and_modifiable_risk_factors$female_breast_cancer = 0
base_population_w_physiological_and_modifiable_risk_factors$renal_cancer = 0
base_population_w_physiological_and_modifiable_risk_factors$kidney_cancer = 0

base_population_w_physiological_and_modifiable_risk_factors$oral_cancer = 0
base_population_w_physiological_and_modifiable_risk_factors$pancreatic_cancer = 0
base_population_w_physiological_and_modifiable_risk_factors$uterine_cancer = 0
base_population_w_physiological_and_modifiable_risk_factors$blood_cancer = 0
base_population_w_physiological_and_modifiable_risk_factors$ovarian_cancer = 0
base_population_w_physiological_and_modifiable_risk_factors$brain_cancer = 0

base_population_w_physiological_and_modifiable_risk_factors$osteogastric_cancer = 0
base_population_w_physiological_and_modifiable_risk_factors$oesophageal_cancer = 0
base_population_w_physiological_and_modifiable_risk_factors$stomach_cancer = 0

base_population_w_physiological_and_modifiable_risk_factors$lung_cancer <- 0
base_population_w_physiological_and_modifiable_risk_factors$colorectal_cancer <- 0

base_population_w_physiological_and_modifiable_risk_factors$pad_status <- 'no_pad'
base_population_w_physiological_and_modifiable_risk_factors$vte_status <- 'no_vte'
base_population_w_physiological_and_modifiable_risk_factors$ckd_status <- 'no_ckd'
base_population_w_physiological_and_modifiable_risk_factors$af_status <- 'no_af'

write.fst(base_population_w_physiological_and_modifiable_risk_factors,
          paste0('./3_pre_main/intermediate_populations/base_population_w_physiological_and_modifiable_risk_factors.fst'))

# write.fst(bmi_stratified_prevalence, './3_pre_main/joint_distributions/bmi_stratified_prevalence.fst')
# write.fst(child_bmi_stratified_prevalence, './3_pre_main/joint_distributions/child_bmi_stratified_prevalence.fst')
# write.fst(smoking_results_df, './3_pre_main/joint_distributions/smoking_results_df.fst')
# write.fst(ecigarette_stratified_prevalence, './3_pre_main/joint_distributions/ecigarette_stratified_prevalence.fst')
# write.fst(alcohol_stratified_prevalence, './3_pre_main/joint_distributions/alcohol_stratified_prevalence.fst')
# write.fst(diet_stratified_prevalence, './3_pre_main/joint_distributions/diet_stratified_prevalence.fst')
# write.fst(pa_stratified_prevalence, './3_pre_main/joint_distributions/pa_stratified_prevalence.fst')
# write.fst(wellbeing_stratified_prevalence, './3_pre_main/joint_distributions/wellbeing_stratified_prevalence.fst')
# write.fst(cholesterol_stratified_prevalence, './3_pre_main/joint_distributions/cholesterol_stratified_prevalence.fst')
# write.fst(hypertension_stratified_prevalence, './3_pre_main/joint_distributions/hypertension_stratified_prevalence.fst')
# write.fst(diabetes_stratified_prevalence, './3_pre_main/joint_distributions/diabetes_stratified_prevalence.fst')
# write.fst(general_health_stratified_prevalence, './3_pre_main/joint_distributions/general_health_stratified_prevalence.fst')
# write.fst(loneliness_stratified_prevalence, './3_pre_main/joint_distributions/loneliness_stratified_prevalence.fst')
# write.fst(limiting_stratified_prevalence, './3_pre_main/joint_distributions/limiting_stratified_prevalence.fst')
# write.fst(disability_stratified_prevalence, './3_pre_main/joint_distributions/disability_stratified_prevalence.fst')
# qsave(special_cholesterol , './3_pre_main/joint_distributions/special_cholesterol.fst')

message('done pre main 1')

