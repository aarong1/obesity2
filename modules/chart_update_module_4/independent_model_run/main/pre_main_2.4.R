library(tidyverse)
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

###############################################
############ Model Specification ##############
###############################################

source('./main/main_utils_2_4.R') # model_specification list

source('./main/main_configuration.R') # model_specification list

###############################################
############ Population Actualization #########
###############################################

base_population <- instantiate_base_pop( model_specification)

# ~/Documents/SIB/PHM/PHModel/synthetic_population/age_synthetic_population.R
# save_yearly_populations <- read.fst('./births_module/yearly_start_populations.fst')

# base_population <- save_yearly_populations |> 
#   filter(year == model_specification$model$start_year)

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

source('./risk_joint_estimation/smoking.R') #check
source('./risk_joint_estimation/alcohol.R') #check
source('./risk_joint_estimation/adult_obesity.R') #check
source('./risk_joint_estimation/child_obesity.R') #check
source('./risk_joint_estimation/PA.R') 
source('./risk_joint_estimation/5veg.R')
source('./risk_joint_estimation/wellbeing.R')
source('./risk_joint_estimation/electronic_cigarettes.R')

source('./risk_joint_estimation/hypertension.R') #check
source('./risk_joint_estimation/diabetes.R')
source('./risk_joint_estimation/cholesterol.R') #check

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
source('./apply_sleep_apnea.R')


########################################################
########################################################

source('./risk_exposure_prevalence/diabetes/apply_diabetes_physiological_parameter_rank_stability_long_form.R')
source('./risk_exposure_prevalence/cholesterol/apply_cholesterol_physiological_parameter_rank_stability_long_form.R')
# source('./risk_exposure_prevalence/apply_cholesterol_physiological_parameter.R')

source('./risk_exposure_prevalence/hypertension/apply_hypertension_physiological_parameter_rank_stability_long_form.R')

source('./pollution/apply_pollution_risk_environmental_risk_factor.R')



###  Two/three stability rank strategy -----------------------------------

base_population_w_modifiable_risk_factors <- base_population_w_correlated_percentiles %>%
  #bmi
  apply_bmi_lifestyle_parameter_3State_rank_stability(bmi_stratified_prevalence) %>% 
  apply_child_bmi_lifestyle_parameter_3State_rank_stability(child_bmi_stratified_prevalence) %>% 
  combine_child_adult_bmi() |> 
  #smoking
  apply_smoking_lifestyle_parameter_rank_stability(smoking_results_df) %>%
  # apply_ecigarette_lifestyle_parameter_rank_stability(ecigarette_stratified_prevalence) %>%
  #alcohol
  apply_alcohol_lifestyle_parameter_rank_stability(alcohol_stratified_prevalence) %>%
  apply_diet_lifestyle_parameter_rank_stability(diet_stratified_prevalence) %>%
  apply_pa_lifestyle_parameter_rank_stability(pa_stratified_prevalence) %>%
  apply_wellbeing_depression_lifestyle_parameter_rank_stability(wellbeing_results_df) |> #count(wellbeing)
  apply_pollution_lifestyle_parameter_geography_constant(lookup_dz_raster_cell) |> 
  apply_sleep_lifestyle_parameter_rank_stability()

###  Two/three stability rank strategy -----------------------------------
#Physiological ------

base_population_w_physiological_and_modifiable_risk_factors <- base_population_w_modifiable_risk_factors |> 
  apply_cholesterol_physiological_parameter_rank_stability(chol_perc) |>
  apply_hypertension_physiological_parameter_rank_stability(hypertension_results_df) |>
  apply_diabetes_physiological_parameter_rank_stability(diabetes_joint_estimation_results_df)

base_population_w_physiological_and_modifiable_risk_factors <- apply_granular_cholesterol_measure_posthoc_overlay(base_population_w_physiological_and_modifiable_risk_factors,special_cholesterol)

# names(base_population_w_modifiable_risk_factors)

##############################################
################### Checks ###################
##############################################

# base_population_w_modifiable_risk_factors |> 
#   count(diet, age) |> 
#   filter(is.na(diet)) #|> count(age)
# base_population_w_modifiable_risk_factors |> 
#   count(wellbeing,age)  |> filter(is.na(wellbeing)) 
# base_population_w_modifiable_risk_factors |> 
#   count(pa,age) |> filter(is.na(pa)) 
# base_population_w_modifiable_risk_factors |> 
#   count(alcohol, age) |> filter(is.na(alcohol)) 
# base_population_w_modifiable_risk_factors |> 
#   count(smoking, age)
# base_population_w_modifiable_risk_factors |> 
#   count(bmi,age) |> filter(is.na(bmi))
# base_population_w_modifiable_risk_factors |> 
#   count(pm25g) |> filter(is.na(pm25g))

base_population_w_physiological_and_modifiable_risk_factors |> 
  count(diabetes_status)
base_population_w_physiological_and_modifiable_risk_factors |> 
  count(hypertension_status)
base_population_w_physiological_and_modifiable_risk_factors |> 
  count(cholesterol_status)


#############################################
# This is age-wise stuff we were going to put in - very simple
#############################################

# source('./data_pipeline/pipeline.R')

# base_population_w_risk_factors <- 
#   base_population_w_modifiable_risk_factors |> 
#   apply_atrial_fibrillation_physiological_parameter(age_sex_risk) |>
#   apply_hypertension_physiological_parameter(age_sex_risk) |>
#   apply_pad_physiological_parameter(pad_prevalence) |>
#   apply_cholesterol_physiological_parameter(cholesterol_prevalence) |>
#   apply_diabetes_physiological_parameter(diabetes_prevalence) |> 
#   apply_chronic_kidney_disease_physiological_parameter(ckd_prevalence) 
#############################################

base_population_w_physiological_and_modifiable_risk_factors <-
  base_population_w_physiological_and_modifiable_risk_factors |> 
  apply_af_risk_wo_risk_factors() |>
  apply_ckd_risk_wo_risk_factors() |> 
  apply_pad_risk_wo_risk_factors() |> 
  apply_vte_risk_wo_risk_factors()

source('risk_exposure_prevalence/apply_ckd_physiological_parameter_rank_stability.R') 
source('risk_exposure_prevalence/apply_pad_physiological_parameter_rank_stability.R')
source('risk_exposure_prevalence/apply_vte_physiological_parameter_rank_stability.R')
source('risk_exposure_prevalence/apply_af_physiological_parameter_rank_stability.R')

# current_population <- base_population_w_physiological_and_modifiable_risk_factors
base_population_w_risk_factors <- 
  base_population_w_physiological_and_modifiable_risk_factors |> 
  apply_ckd_physiological_parameter_rank_stability(ckd_prevalence) |> 
  apply_pad_physiological_parameter_rank_stability(pad_prevalence) |> 
  apply_vte_physiological_parameter_rank_stability(vte_prevalence) |> 
  apply_af_physiological_parameter_rank_stability(af_prevalence)

##########################################
# build the risks that have no correlated percentile as a function of
# risks of other risks and constrained to age-sex-deprivation above
##########################################

#names(base_population_w_risk_factors)
# OR skip .... 
#base_population_w_risk_factors <- base_population

##########################################
##########################################
# build the risks that have no correlated percentile as a function of
# risks of other risks and constrained to age-sex-deprivation above
##########################################
##########################################


 write.fst(ungroup(base_population_w_risk_factors),'base_population_w_risk_factors.fst')
# base_population_w_risk_factors <- read.fst('base_population_w_risk_factors.fst')

initial_time_zero_population <- ungroup(base_population_w_risk_factors)
##########################################

# #New source of death - QMORTALITY
# source('./incidence_operators/qmortality.R')

# initial_time_zero_population <- initial_time_zero_population |>
#   #apply_age_sex_death() |> 
#   apply_qmortality_mortality() 

# ggplot(initial_time_zero_population) +
#   geom_point(aes(age,
#                  qmortality_risk, #af_risk, hypertension_risk, hf_risk, dementia_risk, chd_risk,#stroke_risk, #qrisk_score
#                  col=bmi)) +
#   facet_grid(~sex)

##########################################
#names(initial_time_zero_population)

print('Calculating morbidity risk to initial time zero population')

#CVD
initial_time_zero_population <- initial_time_zero_population %>% 
  #apply_cvd_risk() %>%.
  apply_cvd_risk_wo_risk_factors() |> 
  mutate(qrisk_year_risk = transform_10y_probability_to_1y(qrisk_score)) 

# initial_time_zero_population <- initial_time_zero_population %>%
#   apply_cvd_risk_wo_risk_factors_dt()
# # initial_time_zero_population$qrisk_score  = NULL
# initial_time_zero_population <- initial_time_zero_population [
#   , qrisk_year_risk := transform_10y_probability_to_1y(qrisk_score)
# ]

#graph_inspect_apply_risk(initial_time_zero_population, age, qrisk_year_risk, bmi, facet_formula = ~sex)

#STROKE 1
initial_time_zero_population <- initial_time_zero_population |> 
  # apply_stroke_risk() |>
  apply_stroke_risk_wo_risk_factors() |> 
  #rowwise() |> 
  mutate(stroke_year_risk = transform_10y_probability_to_1y(stroke_risk))# |> 
# declare_absolute_incident_morbidity(morbidity = 'stroke')
## function to convert risk of morbidity each year to the absolute declaration of state occupancy ----

#CHD 2
initial_time_zero_population <- initial_time_zero_population |> 
  apply_chd_risk() |>
  # rowwise() |> 
  mutate(chd_year_risk = transform_10y_probability_to_1y(chd_risk)) 

#DIABETES 3
initial_time_zero_population <- initial_time_zero_population |>
  apply_diabetes_risk_wo_risk_factors() |>
  # rowwise() |>
  mutate(diabetes_year_risk = transform_10y_probability_to_1y(diabetes_risk))

#NDH 11

initial_time_zero_population <- initial_time_zero_population |>
  mutate(non_diabetic_hyperglycaemia_year_risk = diabetes_year_risk)

#DEMENTIA 4 - Two implementations
#  - UKBDRS
#  - DRS

initial_time_zero_population <- initial_time_zero_population |>
  # apply_dementia_ukbdrs_14yr_risk_wo_risk_factors() |>
  apply_dementia_drs_5yr_risk_wo_risk_factors() |>
  # rowwise() |>
  # mutate(dementia_year_risk = transform_probability_to_1y(dementia_risk, tot_years = 14))
  mutate(dementia_year_risk = transform_probability_to_1y(dementia_risk, tot_years = 5))

##HEART FAILURE 5
initial_time_zero_population <- initial_time_zero_population |>
  #apply_hf_risk() |>
  apply_hf_risk_wo_risk_factors() |> 
  # rowwise() |>
  mutate(heart_failure_year_risk = transform_probability_to_1y(hf_risk, tot_years = 4))

##HYPERTENSION 6
initial_time_zero_population <- initial_time_zero_population |>
  apply_hypertension_risk_wo_risk_factors() |>
  # rowwise() |>
  mutate(hypertension_year_risk = transform_probability_to_1y(hypertension_risk, tot_years = 4))

##ATRIAL FIBRILLATION  7
initial_time_zero_population <- initial_time_zero_population |>
  #apply_AF_risk() |>
  apply_af_risk_wo_risk_factors() |>
  # rowwise() |>
  mutate(atrial_fibrillation_year_risk = transform_probability_to_1y(af_risk, tot_years = 10))

#CHRONIC KIDNEY DISEASE 8
initial_time_zero_population <- initial_time_zero_population |>
  apply_ckd_risk_wo_risk_factors() |>
  # rowwise() |>
  mutate(chronic_kidney_disease_year_risk = transform_probability_to_1y(ckd_risk, tot_years = 5))

############  Respiratory ############ 

# # asthma

initial_time_zero_population <- initial_time_zero_population |>
  apply_asthma_risk()

# ggplot(initial_time_zero_population) +
#   geom_point(aes(age,
#                  asthma_year_risk, #af_risk, hypertension_risk, hf_risk, dementia_risk, chd_risk,#stroke_risk, #qrisk_score
#                  col=paste(bmi,smoking))) +
#   facet_grid(~sex)+
#   lims(y=c(0,NA))

# # COPD

initial_time_zero_population<- initial_time_zero_population |>
  apply_copd_risk()

# library(plotly)
# (ggplot(initial_time_zero_population) +
#     geom_point(aes(age,
#                    copd_year_risk, #af_risk, hypertension_risk, hf_risk, dementia_risk, chd_risk,#stroke_risk, #qrisk_score
#                    col=smoking
#     )) +
#     facet_grid(~sex) +
#     lims(y=c(0,NA))) #%>% 
# ggplotly()

################# Other CVD ################# 


##PERIPHERAL ARTERIAL DISEASE
initial_time_zero_population<- initial_time_zero_population |>
  apply_pad_risk_wo_risk_factors() |>
 rowwise() |>
 mutate(pad_year_risk = transform_probability_to_1y(pad_risk, tot_years = 4))


##VENOUS THROMBOEMBELISM
# initial_time_zero_population <- initial_time_zero_population |>
#   apply_vte_risk_wo_risk_factors() |>
#  rowwise() |>
#  mutate(vte_year_risk = transform_probability_to_1y(vte_risk, tot_years = 5))

# ggplot(initial_time_zero_population) +
#   geom_point(aes(age,
#                  nof_year_risk, #af_risk, hypertension_risk, hf_risk, dementia_risk, chd_risk,#stroke_risk, #qrisk_score
#                  col=bmi)) +
#   facet_grid(~sex)

################# Other  ################# 

# # Depression

initial_time_zero_population <- initial_time_zero_population |>
  mutate(depression_year_risk = depression_percentile)

# # EPILEPSY

initial_time_zero_population <- initial_time_zero_population |>
  apply_epilepsy_risk()

#LIVER DISEASE
# initial_time_zero_population <- initial_time_zero_population |>
  # apply_liver_disease_risk_wo_risk_factors() |>

#  mutate(liver_year_risk = transform_probability_to_1y(liver_risk, tot_years = 10))

# source("./risk_correct_eq/risk_qfracture_hip_wrist_shoulder_spine.R")

# # OESTEOPOROSIS FRACTURE OF THE HIP ( WRIST, SHOULDER, HIP, SPINE)
# https://fingertips.phe.org.uk/static-reports/health-trends-in-england/England/musculoskeletal_health.html
# initial_time_zero_population <- initial_time_zero_population |> #select(-c("lung_cancer_risk","lung_cancer_year_risk" ))
#   apply_fracture4_risk_wo_risk_factors() |> 
#   mutate(fracture4_year_risk = transform_probability_to_1y(fracture4_risk, tot_years = 10))

# ggplot(initial_time_zero_population) +
#   geom_point(aes(age,
#                  fracture4_year_risk, #af_risk, hypertension_risk, hf_risk, dementia_risk, chd_risk,#stroke_risk, #qrisk_score
#                  col=bmi)) +
#   facet_grid(~sex)

# # OESTEOPOROSIS FRACTURE OF THE NECK OF FEMUR
# source("./risk_correct_eq/risk_qfracture_neck_of_femur.R")

# initial_time_zero_population <- initial_time_zero_population |> #select(-c("lung_cancer_risk","lung_cancer_year_risk" ))
#   ungroup() |> 
#   apply_nof_risk_wo_risk_factors() |> 
#   mutate(nof_year_risk = transform_probability_to_1y(nof_risk, tot_years = 10))

#  ABDOMINABLE AORTIC ANEURYSM

# INTERSISTAL LUND DISEASE

# initial_time_zero_population <- initial_time_zero_population |>
#   apply_ILD_arthritis_risk()

# HYPOTHYROIDISM
# use prevalence

# T1DM

# PCOS

# OSTEOPOROSIS
# https://fingertips.phe.org.uk/static-reports/health-trends-in-england/England/musculoskeletal_health.html

# RHEUMATOID ARTHRITIS

initial_time_zero_population <- initial_time_zero_population |>
  apply_rheumatoid_arthritis_risk(rheumatoid_arthritis_incidence)

# OSTEOARTHRITIS

initial_time_zero_population <- initial_time_zero_population |>
  apply_osteoarthritis_risk(osteoarthritis_incidence)

# hist( initial_time_zero_population$oesteoarthritis_year_risk )

# GLAUCOMA

# CATARACTS
# https://jamanetwork.com/journals/jamaophthalmology/fullarticle/261561

############### CANCERS ################# 

initial_time_zero_population <- initial_time_zero_population |> #select(-c("lung_cancer_risk","lung_cancer_year_risk" ))
  apply_cancer_risk() 

# initial_time_zero_population |> count(wt = cancer_year_risk)

#LUNG CANCER 9
initial_time_zero_population <- initial_time_zero_population |> #select(-c("lung_cancer_risk","lung_cancer_year_risk" ))
  apply_lungcancer_risk_wo_risk_factors() |> 
  mutate(lung_cancer_year_risk = transform_probability_to_1y(lungcancer_risk, tot_years = 5))

# ggplot(initial_time_zero_population) +
#   geom_point(aes(age,
#                  lung_cancer_year_risk, #af_risk, hypertension_risk, hf_risk, dementia_risk, chd_risk,#stroke_risk, #qrisk_score
#                  col=bmi)) +
#   facet_grid(~sex)

# source('./risk_correct_eq/risk_qcancer_colorectal.R')

# # #COLORECTAL CANCER
initial_time_zero_population <- initial_time_zero_population |>
  apply_colorectal_cancer_risk_wo_risk_factors() |>
  mutate(colorectal_cancer_year_risk = transform_probability_to_1y(colorectal_cancer_risk, tot_years = 5))

# hist(initial_time_zero_population$colorectal_cancer_year_risk)

# ggplot(initial_time_zero_population) +
#   geom_point(aes(age,
#                  colorectal_year_risk, #af_risk, hypertension_risk, hf_risk, dementia_risk, chd_risk,#stroke_risk, #qrisk_score
#                  col=bmi)) +
#   facet_grid(~sex)

# # BREAST CANCER (F)

# source("./risk_correct_eq/site_cancers/breast_female_cancer.R")
# 
# initial_time_zero_population <- initial_time_zero_population |>
#   risk_qcancer_breastcancer_wo_risk_factors() |>
#   mutate(female_breast_cancer_year_risk = transform_probability_to_1y(female_breast_cancer_risk, tot_years = 5))

# ggplot(initial_time_zero_population) +
#   geom_point(aes(age,
#                  female_breast_cancer_year_risk, #af_risk, hypertension_risk, hf_risk, dementia_risk, chd_risk,#stroke_risk, #qrisk_score
#                  col=bmi)) +
#   facet_grid(~sex)

# # PROSTATE (M)

# source("./risk_correct_eq/site_cancers/prostate_male_cancer.R")
# 
# initial_time_zero_population <- initial_time_zero_population |>
#   apply_prostate_cancer_risk_wo_risk_factors() |>
#   mutate(prostate_cancer_year_risk = transform_probability_to_1y(prostate_cancer_risk, tot_years = 5))

# ggplot(initial_time_zero_population) +
#   geom_point(aes(age,
#                  prostate_cancer_year_risk, #af_risk, hypertension_risk, hf_risk, dementia_risk, chd_risk,#stroke_risk, #qrisk_score
#                  col=bmi)) +
#   facet_grid(~sex)

# # PANCREATIC

# source("./risk_correct_eq/site_cancers/pancreatic_cancer.R")
# 
# initial_time_zero_population <- initial_time_zero_population |>
#   apply_pancreatic_cancer_risk_wo_risk_factors() |>
#   mutate(pancreatic_cancer_year_risk = transform_probability_to_1y(pancreatic_cancer_risk, tot_years = 5))

# ggplot(initial_time_zero_population) +
#   geom_point(aes(age,
#                  pancreatic_cancer_year_risk, #af_risk, hypertension_risk, hf_risk, dementia_risk, chd_risk,#stroke_risk, #qrisk_score
#                  col=bmi)) +
#   facet_grid(~sex)

# # RENAL 
# source("./risk_correct_eq/site_cancers/renal_cancer.R")
# 
# initial_time_zero_population <- initial_time_zero_population |>
#   apply_renal_cancer_risk_wo_risk_factors() |>
#   mutate(renal_cancer_year_risk = transform_probability_to_1y(renal_cancer_risk, tot_years = 5))

# ggplot(initial_time_zero_population) +
#   geom_point(aes(age,
#                  renal_cancer_year_risk, #af_risk, hypertension_risk, hf_risk, dementia_risk, chd_risk,#stroke_risk, #qrisk_score
#                  col=bmi)) +
#   facet_grid(~sex)

# # UTERINE (F)

# source("./risk_correct_eq/site_cancers/uterine_female_cancer.R")

# initial_time_zero_population <- initial_time_zero_population |>
#   apply_uterian_cancer_risk_wo_risk_factors() |>
#   mutate(uterine_cancer_year_risk = transform_probability_to_1y(uterine_cancer_risk, tot_years = 5))

# ggplot(initial_time_zero_population) +
#   geom_point(aes(age,
#                  uterine_cancer_year_risk, #af_risk, hypertension_risk, hf_risk, dementia_risk, chd_risk,#stroke_risk, #qrisk_score
#                  col=bmi)) +
#   facet_grid(~sex)

# # OVARIAN (F)

# source("./risk_correct_eq/site_cancers/ovarian_female_cancer.R")

# initial_time_zero_population <- initial_time_zero_population |>
#   apply_ovariancancer_risk_wo_risk_factors() |>
#   rowwise() |>
#   mutate(ovariancancer_year_risk = transform_probability_to_1y(ovariancancer_risk, tot_years = 5))

# ggplot(initial_time_zero_population) +
#   geom_point(aes(age,
#                  ovariancancer_year_risk, #af_risk, hypertension_risk, hf_risk, dementia_risk, chd_risk,#stroke_risk, #qrisk_score
#                  col=bmi)) +
#   facet_grid(~sex)

# # BLOOD CANCER

# source("./risk_correct_eq/site_cancers/blood_cancer.R")

# initial_time_zero_population <- initial_time_zero_population |>
#   apply_bloodcancer_risk_wo_risk_factors() |>
#   rowwise() |>
#   mutate(bloodcancer_year_risk = transform_probability_to_1y(bloodcancer_risk, tot_years = 5))

# ggplot(initial_time_zero_population) +
#   geom_point(aes(age,
#                  bloodcancer_year_risk, 
#                  col=bmi)) +
#   facet_grid(~sex)

# # OESPHAGEAL-GASTRIC
# source("./risk_correct_eq/site_cancers/oesteogastric_cancer.R")
# initial_time_zero_population <- initial_time_zero_population |>
#   apply_osteogastric_cancer_risk_wo_risk_factors() |>
#   rowwise() |>
#   mutate(osteogastric_year_risk = transform_probability_to_1y(osteogastric_risk, tot_years = 5))

# ggplot(initial_time_zero_population) +
#   geom_point(aes(age,
#                  osteogastric_year_risk, #af_risk, hypertension_risk, hf_risk, dementia_risk, chd_risk,#stroke_risk, #qrisk_score
#                  col=bmi)) +
#   facet_grid(~sex)

# # ORAL
# source("./risk_correct_eq/site_cancers/oral_cancer.R")

# initial_time_zero_population <- initial_time_zero_population |> 
#   apply_oralcancer_risk_wo_risk_factors() |>
#   rowwise() |>
#   mutate(oralcancer_year_risk = transform_probability_to_1y(oralcancer_risk, tot_years = 5))

# ggplot(initial_time_zero_population) +
#   geom_point(aes(age,
#                  oralcancer_year_risk, #af_risk, hypertension_risk, hf_risk, dementia_risk, chd_risk,#stroke_risk, #qrisk_score
#                  col=bmi)) +
#   facet_grid(~sex)

###########----------------------------------------
################# morbidity end ##################
###########----------------------------------------

# write.fst(initial_time_zero_population,
#               paste0('./main/initial_time_zero_population10down.fst'))
write.fst(initial_time_zero_population,
          paste0('./main/initial_time_zero_population10down.fst'))
# write.fst(initial_time_zero_population,
#           paste0('./main/initial_time_zero_population1000down.fst'))

print('done')

