message('start pre main 2')
##########################################

#CVD/Dementia/Fracture/LiverDisease

source("./disease_equation/risk_qstroke_stroke_1_3.R")
source("./disease_equation/risk_qdiabetes_diabetes.R")
source("./disease_equation/risk_framingham_congestive_heart_failure.R")
source("./disease_equation/risk_qkidney_chronic_kidney_disease_severe.R")
source("./disease_equation/risk_framingham_hypertension.R")
source("./disease_equation/risk_framingham_atrial_fibrillation.R")
source("./disease_equation/risk_qthrombosis_venal_thromboembelism.R")
source("./disease_equation/risk_framingham_peripheral_arterial_disease.R")

source('./disease_equation/apply_qrisk_score.R') 

source("./disease_equation/risk_ukbdrs_dementia.R")
source("./disease_equation/risk_drs_dementia.R")

source("./disease_equation/risk_qfracture_fracture.R")
source('./disease_equation/risk_qfracture_neck_of_femur.R')

source("./disease_equation/risk_framingham_liver_disease.R")

#Cancer
source('./disease_equation/risk_qcancer_lungcancer.R')
source("./disease_equation/site_cancers/renal_cancer.R")
source("./disease_equation/site_cancers/prostate_male_cancer.R")
source("./disease_equation/site_cancers/pancreatic_cancer.R")
source("./disease_equation/site_cancers/ovarian_female_cancer.R")
source("./disease_equation/site_cancers/oral_cancer.R")
source("./disease_equation/site_cancers/oesteogastric_cancer.R")
source("./disease_equation/site_cancers/breast_female_cancer.R")
source("./disease_equation/risk_qcancer_colorectal.R")
source("./disease_equation/site_cancers/colorectal_cancer.R")
source("./disease_equation/site_cancers/blood_cancer.R")
source("./disease_equation/site_cancers/uterine_female_cancer.R")
 
source('./disease_engines/alcoholic_related_liver_disease.R')
source('./disease_engines/non_alcoholic_fatty_liver_disease.R')

source('./populate_time_zero_prevalence/populate_stroke.R')
source('./populate_time_zero_prevalence/populate_copd.R')
source('./populate_time_zero_prevalence/populate_asthma.R')
source('./populate_time_zero_prevalence/populate_dementia.R')

print('Calculating morbidity risk to initial time zero population')

###  Two/three stability rank strategy -----------------------------------
#Physiological ------
base_population_w_physiological_and_modifiable_risk_factors <- read.fst('./3_pre_main/intermediate_populations/base_population_w_physiological_and_modifiable_risk_factors.fst')
# population_w_established_prevalence <- read.fst('./3_pre_main/population_w_established_prevalence.fst')

count(base_population_w_physiological_and_modifiable_risk_factors, bmi)


base_population_w_physiological_and_modifiable_risk_factors <-
  base_population_w_physiological_and_modifiable_risk_factors %>% 
  apply_af_risk_wo_risk_factors() %>%
  apply_ckd_risk_wo_risk_factors() %>% 
  apply_pad_risk_wo_risk_factors() %>% 
  apply_vte_risk_wo_risk_factors()
  
# source('./joint_risk_estimation/kidney_disease.R')
# source('./joint_risk_estimation/atrial_fibrillation.R')

source('risk_exposure_prevalence/apply_ckd_physiological_parameter_rank_stability.R') 
source('risk_exposure_prevalence/apply_pad_physiological_parameter_rank_stability.R')
source('risk_exposure_prevalence/apply_vte_physiological_parameter_rank_stability.R')
source('risk_exposure_prevalence/apply_af_physiological_parameter_rank_stability.R')

# current_population <- base_population_w_physiological_and_modifiable_risk_factors
base_population_w_risk_factors <- 
  base_population_w_physiological_and_modifiable_risk_factors %>% 
  apply_ckd_physiological_parameter_rank_stability(ckd_prevalence) %>% 
  apply_pad_physiological_parameter_rank_stability(pad_prevalence) %>% 
  apply_vte_physiological_parameter_rank_stability(vte_prevalence) %>% 
  apply_af_physiological_parameter_rank_stability(af_prevalence)

##########################################
# build the risks that have no correlated percentile as a function of
# risks of other risks and constrained to age-sex-deprivation above
##########################################

#names(base_population_w_risk_factors)
# OR skip .... 
#base_population_w_risk_factors <- base_population

##########################################
# build the risks that have no correlated percentile as a function of
# risks of other risks and constrained to age-sex-deprivation above
##########################################

write.fst(ungroup(base_population_w_risk_factors),'base_population_w_risk_factors.fst')

count(base_population_w_risk_factors, bmi)


# base_population_w_risk_factors <- read.fst('base_population_w_risk_factors.fst')

base_population_w_risk_factors <- ungroup(base_population_w_risk_factors)

#CVD
base_population_w_risk_factors <- base_population_w_risk_factors %>% 
  #apply_cvd_risk() %>%.
  apply_cvd_risk_wo_risk_factors() %>% 
  mutate(qrisk_year_risk = transform_10y_probability_to_1y(qrisk_score)) 

# base_population_w_risk_factors <- base_population_w_risk_factors %>%
#   apply_cvd_risk_wo_risk_factors_dt()
# # base_population_w_risk_factors$qrisk_score  = NULL
# base_population_w_risk_factors <- base_population_w_risk_factors [
#   , qrisk_year_risk := transform_10y_probability_to_1y(qrisk_score)
# ]

#graph_inspect_apply_risk(base_population_w_risk_factors, age, qrisk_year_risk, bmi, facet_formula = ~sex)

#STROKE 1
base_population_w_risk_factors <- base_population_w_risk_factors %>% 
  apply_stroke_risk_wo_risk_factors() %>% 
  mutate(stroke_year_risk = transform_10y_probability_to_1y(stroke_risk))# %>% 
# declare_absolute_incident_morbidity(morbidity = 'stroke')
## function to convert risk of morbidity each year to the absolute declaration of state occupancy ----

#CHD 2
base_population_w_risk_factors <- base_population_w_risk_factors %>% 
  apply_chd_risk() %>%
  mutate(chd_year_risk = transform_10y_probability_to_1y(chd_risk)) 

#DIABETES 3
base_population_w_risk_factors <- base_population_w_risk_factors %>%
  apply_diabetes_risk_wo_risk_factors() %>%
  mutate(diabetes_year_risk = transform_10y_probability_to_1y(diabetes_risk))

#NDH 11
base_population_w_risk_factors <- base_population_w_risk_factors %>%
  mutate(non_diabetic_hyperglycaemia_year_risk = diabetes_year_risk)

#DEMENTIA 4 - Two implementations
#  - UKBDRS
#  - DRS

base_population_w_risk_factors <- base_population_w_risk_factors %>%
  # apply_dementia_ukbdrs_14yr_risk_wo_risk_factors() %>%
  apply_dementia_drs_5yr_risk_wo_risk_factors() %>%
  # mutate(dementia_year_risk = transform_probability_to_1y(dementia_risk, tot_years = 14))
  mutate(dementia_year_risk = transform_probability_to_1y(dementia_risk, tot_years = 5))

##HEART FAILURE 5
base_population_w_risk_factors <- base_population_w_risk_factors %>%
  apply_hf_risk_wo_risk_factors() %>% 
  mutate(heart_failure_year_risk = transform_probability_to_1y(hf_risk, tot_years = 4))

##HYPERTENSION 6
base_population_w_risk_factors <- base_population_w_risk_factors %>%
  apply_hypertension_risk_wo_risk_factors() %>%
  mutate(hypertension_year_risk = transform_probability_to_1y(hypertension_risk, tot_years = 4))

##ATRIAL FIBRILLATION  7
base_population_w_risk_factors <- base_population_w_risk_factors %>%
  apply_af_risk_wo_risk_factors() %>%
  mutate(atrial_fibrillation_year_risk = transform_probability_to_1y(af_risk, tot_years = 10))

#CHRONIC KIDNEY DISEASE 8
base_population_w_risk_factors <- base_population_w_risk_factors %>%
  apply_ckd_risk_wo_risk_factors() %>%
  mutate(chronic_kidney_disease_year_risk = transform_probability_to_1y(ckd_risk, tot_years = 5))

############  Respiratory ############ 

# # asthma
base_population_w_risk_factors <- base_population_w_risk_factors %>%
  apply_asthma_risk_factors(asthma_theoretical_minimum)

# # COPD
base_population_w_risk_factors<- base_population_w_risk_factors %>%
  apply_copd_risk_factors(copd_theoretical_minimum)

################# Other CVD ################# 

##PERIPHERAL ARTERIAL DISEASE
base_population_w_risk_factors<- base_population_w_risk_factors %>%
  apply_pad_risk_wo_risk_factors() %>%
  mutate(pad_year_risk = transform_probability_to_1y(pad_risk, tot_years = 4))

##VENOUS THROMBOEMBELISM
base_population_w_risk_factors <- base_population_w_risk_factors %>%
  apply_vte_risk_wo_risk_factors() %>%
  mutate(vte_year_risk = transform_probability_to_1y(vte_risk, tot_years = 5))

################# Other  ################# 

# # Depression
base_population_w_risk_factors <- base_population_w_risk_factors %>%
  mutate(depression_year_risk = depression_percentile)

# # EPILEPSY
base_population_w_risk_factors <- base_population_w_risk_factors %>%
  apply_epilepsy_risk()

# ABDOMINABLE AORTIC ANEURYSM
base_population_w_risk_factors <- base_population_w_risk_factors %>%
  apply_aaa_risk_factors(aaa_theoretical_minimum)

# INTERSISTAL LUNG DISEASE
base_population_w_risk_factors <- base_population_w_risk_factors %>%
  apply_ILD_risk()

# OSTEOPOROSIS
base_population_w_risk_factors <- base_population_w_risk_factors %>%
  apply_osteoporosis_risk()

# HYPOTHYROIDISM
base_population_w_risk_factors <- base_population_w_risk_factors %>%
  apply_hypothyroid_risk()

#INFLAMMATORY BOWEL DISEASE
base_population_w_risk_factors <- base_population_w_risk_factors %>%
  apply_ibd_risk()

#NAFLD
base_population_w_risk_factors <- base_population_w_risk_factors %>%
  apply_liver_disease_risk_wo_risk_factors() %>% 
  mutate(nafld_year_risk = transform_probability_to_1y(nafld_risk, tot_years = 6))
# also engine coming soon ----

#ARLD
# also engine comming soon ----
base_population_w_risk_factors <- base_population_w_risk_factors %>%
apply_arld_risk()

#SLE
base_population_w_risk_factors <- base_population_w_risk_factors %>%
  apply_sle_risk()

# RHEUMATOID ARTHRITIS
base_population_w_risk_factors <- base_population_w_risk_factors %>%
  apply_ra_risk_factors(rheumatoid_arthritis_theoretical_minimum)

# OSTEOARTHRITIS
base_population_w_risk_factors <- base_population_w_risk_factors %>%
  apply_oa_risk_factors(osteoarthritis_theoretical_minimum)

# GLAUCOMA
# CATARACTS
# https://jamanetwork.com/journals/jamaophthalmology/fullarticle/261561

############### CANCERS ################# 

base_population_w_risk_factors <- base_population_w_risk_factors %>% 
  apply_cancer_risk() 

#LUNG CANCER 9
base_population_w_risk_factors <- base_population_w_risk_factors %>% 
  apply_lungcancer_risk_wo_risk_factors() %>% 
  mutate(lung_cancer_year_risk = transform_probability_to_1y(lungcancer_risk, tot_years = 5))

# # #COLORECTAL CANCER
base_population_w_risk_factors <- base_population_w_risk_factors %>%
  apply_colorectal_cancer_risk_wo_risk_factors() %>%
  mutate(colorectal_cancer_year_risk = transform_probability_to_1y(colorectal_cancer_risk, tot_years = 5))

base_population_w_risk_factors <- base_population_w_risk_factors %>%
  apply_brain_cancer_risk_wo_risk_factors() 

base_population_w_risk_factors <- base_population_w_risk_factors %>% 
  apply_cervical_cancer_risk_wo_risk_factors()

base_population_w_risk_factors <- base_population_w_risk_factors %>%
  apply_breast_cancer_risk_wo_risk_factors() %>%
  mutate(female_breast_cancer_year_risk = transform_probability_to_1y(breast_cancer_risk, tot_years = 5))

base_population_w_risk_factors <- base_population_w_risk_factors %>%
  apply_prostate_cancer_risk_wo_risk_factors() %>%
  mutate(prostate_cancer_year_risk = transform_probability_to_1y(prostate_cancer_risk, tot_years = 5))

base_population_w_risk_factors <- base_population_w_risk_factors %>%
  apply_pancreatic_cancer_risk_wo_risk_factors() %>%
  mutate(pancreatic_cancer_year_risk = transform_probability_to_1y(pancreatic_cancer_risk, tot_years = 5))

base_population_w_risk_factors <- base_population_w_risk_factors %>%
  apply_renal_cancer_risk_wo_risk_factors() %>%
  mutate(renal_cancer_year_risk = transform_probability_to_1y(renal_cancer_risk, tot_years = 5))

base_population_w_risk_factors <- base_population_w_risk_factors %>%
  apply_uterian_cancer_risk_wo_risk_factors() %>%
  mutate(uterine_cancer_year_risk = transform_probability_to_1y(uterine_cancer_risk, tot_years = 5))

base_population_w_risk_factors <- base_population_w_risk_factors %>%
  apply_ovarian_cancer_risk_wo_risk_factors() %>%
  mutate(ovarian_cancer_year_risk = transform_probability_to_1y(ovarian_cancer_risk, tot_years = 5))

base_population_w_risk_factors <- base_population_w_risk_factors %>%
  apply_blood_cancer_risk_wo_risk_factors() %>%
  mutate(blood_cancer_year_risk = transform_probability_to_1y(blood_cancer_risk, tot_years = 5))

base_population_w_risk_factors <- base_population_w_risk_factors %>%
  apply_osteogastric_cancer_risk_wo_risk_factors() %>%
  mutate(osteogastric_cancer_year_risk = transform_probability_to_1y(osteogastric_cancer_risk, tot_years = 5))

base_population_w_risk_factors <- base_population_w_risk_factors %>% 
  apply_oral_cancer_risk_wo_risk_factors() %>%
  mutate(oral_cancer_year_risk = transform_probability_to_1y(oral_cancer_risk, tot_years = 5))

#FALLS 

# source("./risk_correct_eq/risk_qfracture_hip_wrist_shoulder_spine.R")

# # OESTEOPOROSIS FRACTURE OF THE HIP ( WRIST, SHOULDER, HIP, SPINE)
# https://fingertips.phe.org.uk/static-reports/health-trends-in-england/England/musculoskeletal_health.html
base_population_w_risk_factors <- base_population_w_risk_factors %>% #select(-c("lung_cancer_risk","lung_cancer_year_risk" ))
  apply_fracture4_risk_wo_risk_factors() %>%
  mutate(fracture4_year_risk = transform_probability_to_1y(fracture_risk, tot_years = 10))

# ggplot(base_population_w_risk_factors) +
#   geom_point(aes(age,
#                  fracture4_year_risk, #af_risk, hypertension_risk, hf_risk, dementia_risk, chd_risk,#stroke_risk, #qrisk_score
#                  col=bmi)) +
#   facet_grid(~sex)

# # OESTEOPOROSIS FRACTURE OF THE NECK OF FEMUR
# source("./risk_correct_eq/risk_qfracture_neck_of_femur.R")

base_population_w_risk_factors <- base_population_w_risk_factors %>% #select(-c("lung_cancer_risk","lung_cancer_year_risk" ))
  ungroup() %>%
  apply_nof_risk_wo_risk_factors() %>%
  mutate(nof_year_risk = transform_probability_to_1y(nof_risk, tot_years = 10))

###########----------------------------------------
################# morbidity end ##################
###########----------------------------------------

# chd_engine.R
# stroke_engine.R
# heart_failure_engine.R
# atrial_fibrillation_engine.R
# diabetes_2_engine.R
# diabetes_1_engine.R
# 
# dementia_engine.R
# 
# asthma_engine.R
# copd_engine.R
# 
# colorectal_engine.R
# lung_cancer_engine.R
# prostate_cancer_engine.R
# breast_cancer_engine.R
# brain_cancer_engine.R
# 
# epilepsy_engine.R
# hypothyroidism_engine.R
# lupus_engine.R
# IBD_engine.R
# 
# fracture_engine.R
# arld_disease_engine.R
# nafld_disease_engine.R
# 
# base_population_w_risk_factors <- base_population_w_risk_factors %>% 
#   
#   apply_diabetes_risk_factors( diabetes_theoretical_min_table) %>%
#   apply_chd_risk_factors(chd_theoretical_min_table) %>%
#   apply_atrial_fibrillation_risk_factors(af_theoretical_min_table) %>%
#   apply_chronic_kidney_disease_risk_factors(ckd_theoretical_min_table) %>%
#   apply_heart_failure_risk_factors(hf_theoretical_min_table) %>%
#   apply_stroke_risk_factors(stroke_theoretical_min_table) %>% 
#   
#   apply_dementia_risk_factors(dementia_theoretical_min_table) %>%
#   
#   apply_asthma_risk() %>%
#   apply_copd_risk() %>%
#   
#   apply_lung_cancer_risk_factors() %>%
#   apply_prostate_cancer_risk_factors() %>%
#   apply_crc_risk_factors()
#   apply_breast_cancer_risk_factors() %>%
#   
#   apply_kidney_cancer_engine()
#   apply_uterine_cancer_engine()
#   apply_prostate_cancer_engine()
#   apply_pancreatic_cancer_engine()
#   apply_ovarian_cancer_engine()
#   apply_oral_cancer_engine()
#   apply_gallbladder_cancer_engine()
# 
# cervical_cancer() %>% 
#   brain_Cancer() %>% 
#   prostate_cancer() %>% 
#   
#   # apply_low_birth_weight_risk_factors() %>%
#   # apply_preterm_risk_factors() %>%
#   # apply_still_birth_risk_factors() %>% 
#   
#   # apply_gestational_diabetes_risk() %>% 
#   # apply_congenital_heart_disease_risk() %>% 
#   
#   apply_aaa_risk()
# 
# apply_epilepsy_risk()
# apply_ibd_risk()
# 
# apply_sle_risk()
# apply_hypothyroid_risk()
# apply_ILD_risk()
# apply_osteoporosis_risk()
# apply_rheumatoid_arthritis_risk(rheumatoid_arthritis_incidence)
# apply_osteoarthritis_risk(osteoarthritis_incidence)

# apply_diabetes_1_risk()

# apply_arld_risk() %>% 
# apply_nafld_risk() %>% 

# apply_hypertension__risk()

# apply_crohns_disease_risk()
# apply_ulcerative_colitis_risk()

# apply_fracture_risk()

# write.fst(base_population_w_risk_factors,
#           paste0('./main/base_population_w_risk_factors10down.fst'))

####
# Prevalence #
####

source('./populate_time_zero_prevalence/populate_inflammatory_bowel_disease.R')
source('./populate_time_zero_prevalence/populate_intersistal_lung_disease.R')
source('./populate_time_zero_prevalence/populate_systemic_lupus_erythematosus.R')
source('./populate_time_zero_prevalence/populate_aortic_aneurysm.R')

source('./populate_time_zero_prevalence/populate_alcoholic_related_liver_disease.R')
source('./populate_time_zero_prevalence/populate_non_alcoholic_fatty_liver_disease.R')

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
source('./populate_time_zero_cancers_prevalence/populate_oral_cancer.R')
source('./populate_time_zero_cancers_prevalence/populate_pancreatic_cancer.R')
source('./populate_time_zero_cancers_prevalence/populate_uterine_cancer.R')
source('./populate_time_zero_cancers_prevalence/populate_blood_multiple_myeloma.R')
source('./populate_time_zero_cancers_prevalence/populate_blood_lymphoma.R')
source('./populate_time_zero_cancers_prevalence/populate_blood_leukaemia.R')
source('./populate_time_zero_cancers_prevalence/populate_ovarian_cancer.R')


base_population <- base_population_w_risk_factors

count(base_population, bmi)

base_population <- populate_all_cancers_prevalence(base_population)

base_population <- populate_prostate_cancer(base_population)
base_population <- populate_female_breast_cancer(base_population)
base_population <- populate_renal_cancer_prevalence(base_population)
base_population <- populate_oesophageal_cancer_prevalence(base_population)
base_population <- populate_stomach_cancer_prevalence(base_population)

base_population <- base_population %>% 
  mutate(osteogastric_cancer = case_when( oesophageal_cancer != 0 ~ min(year), 
                                          stomach_cancer != 0 ~ min(year),
                                          T ~0 ))
# count(base_population,stomach_cancer,oesophageal_cancer,osteogastric_cancer)

base_population <- populate_oral_cancer_prevalence(base_population)
base_population <- populate_pancreatic_cancer_prevalence(base_population)
base_population <- populate_uterine_cancer_prevalence(base_population)
base_population <- populate_blood_multiple_myeloma_prevalence(base_population)
base_population <- populate_blood_lymphoma_prevalence(base_population)
base_population <- populate_blood_leukaemia_prevalence(base_population)

base_population <- base_population %>% 
  mutate(blood_cancer = case_when( blood_multiple_myeloma != 0 ~ min(year),
                                   blood_lymphoma != 0 ~ min(year),
                                   blood_leukaemia != 0 ~ min(year),
                                   T ~ 0  ))

# count(base_population, blood_multiple_myeloma, blood_lymphoma, blood_leukaemia, blood_cancer)

base_population <- populate_ovarian_cancer(base_population)
base_population <- populate_colorectal_cancer_prevalence(base_population)
base_population <- populate_lung_cancer_prevalence(base_population )

base_population <- apply_nafld_disease_prevalence(base_population)
base_population <- apply_arld_prevalence(base_population)
base_population <- apply_ild_prevalence(base_population)
base_population <- populate_aortic_aneursym(base_population)
base_population <- apply_sle_prevalence(base_population)
base_population <- populate_inflammatory_bowel_disease(base_population)

base_population <- apply_osteoarthritis_prevalence(base_population ) 
base_population <- populate_epilepsy_prevalence(base_population)

base_population <- base_population %>% 
  apply_doh_disease_prevalence(morbidity = 'osteoporosis') %>% 
  apply_doh_disease_prevalence(morbidity = 'hypothyroidism') %>% 
  apply_doh_disease_prevalence(morbidity = 'pad')  %>% 
  apply_doh_disease_prevalence(morbidity = 'rheumatoid_arthritis') 

########
# QoF
########

df <- c(
  # 'lung_cancer' = 'Cancer',
  'atrial_fibrillation' = 'Atrial Fibrillation',
  'cancer' = 'Cancer',
  'chronic_kidney_disease' = 'Chronic Kidney Disease',
  'chd' = 'Coronary Heart Disease',
  'dementia' = 'Dementia',
  'hypertension' = 'Hypertension',
  'stroke' = 'Stroke & TIA',
  'diabetes' = 'Diabetes Mellitus',
  
  # 'epilepsy' = 'Epilepsy',
  # 'pad' = 'Peripheral Arterial Disease',
  # 'rheumatoid_arthritis' = 'Rheumatoid Arthritis',
  
  'heart_failure' = 'Heart Failure 1',
  'non_diabetic_hyperglycaemia' = 'Non-Diabetic Hyperglycaemia',
  # 'depression'='Depression',
  'asthma' = 'Asthma',
  'copd' = 'Chronic Obstructive Pulmonary Disease') %>% 
  data.frame() %>% 
  rownames_to_column() %>% 
  setnames(c('disease','Disease'))

hsct_pop_count <- base_population %>% 
  count(HSCT) %>% 
  mutate(pop = n*model_specification$population$scale_down_factor) %>% 
  select(-n)

prevalence_hsct <- read_excel("data/DisPrevHsct_nisra_2324.xlsx", 
                              sheet = "Unpivoted") |> 
  filter(`Financial Year` == max(`Financial Year`),
         # `Statistic Label` == 'Raw disease prevalence per 1,000 patients'
         `Statistic Label` == 'Number of patients on the register'
  ) %>% 
  mutate(no = VALUE,#prob = VALUE/1000,
         'HSCT' = `Health and Social Care Trust`,
         Disease = Disease,
         Year=`Financial Year`,
         .keep = 'none') %>% 
  mutate(HSCT = case_when(
    HSCT == 'Belfast' ~ 'BHSCT',
    HSCT == 'Northern' ~ 'NHSCT',
    HSCT == 'Southern' ~ 'SHSCT',
    HSCT == 'South Eastern' ~ 'SEHSCT',
    HSCT == 'Western' ~ 'WHSCT',
    HSCT == 'NI Total' ~ 'Northern Ireland',
    TRUE ~ HSCT
  )) %>% 
  filter(HSCT !=  'Northern Ireland') %>% 
  left_join(df) %>% 
  left_join(hsct_pop_count) %>% 
  filter(!is.na(disease)) %>% 
  filter(!is.na(no))
  # filter(!is.na(prob))

  year = min(base_population$year)
  
  np <- prevalence_hsct %>%
    mutate(prob=no/pop) %>% 
    pivot_wider(id_cols = HSCT, 
                names_from = disease, 
                values_from = no) 
  
  names(np)[-1] <- paste0(names(np)[-1],'_prevalence_prob')

  # x <- base_population
#   base_population <- x
#   
#   base_population <- base_population[as.data.table(np),on='HSCT']
#   base_population[chd_risk!=0, `:=` (chd_percentile = frank(ties.method = 'random',chd_risk)/max(frank(ties.method = 'random',chd_risk)),
#                                      chd_prevalence_prob = chd_prevalence_prob/.N/model_specification$population$scale_down_factor), 
#                   by = 'HSCT']
#   
# #probabilistic
#   base_population[chd_risk!=0, chd := fifelse(runif(.N) < chd_prevalence_prob /0.5*chd_percentile, year,0)]
# #deterministic
#   # base_population[chd_risk!=0, chd := fifelse(chd_prevalence_prob >chd_percentile, year,0)]
# 
  # count( base_population , HSCT, chd) #%>% filter(chd != 0)
#   
#   # 15436  BHSCT
#   # 18687  NHSCT
#   # 14514  SEHSCT
#   # 14966  SHSCT
#   # 11662  WHSCT
# count(base_population, HSCT,wt = chd_risk)
#   count( base_population , age20, wt = chd_percentile)# %>% View()
#   
#   count( base_population , age20, wt = chd_year_risk)# %>% View()
#   count( base_population , age20, chd) #%>% view()
#   
#   count( base_population , age, ovarian_cancer) 
  
base_population <- base_population[as.data.table(np),on='HSCT']

# Calculate percentiles only for rows with non-zero risk
base_population[af_risk!=0, `:=` (atrial_fibrillation_disease_percentile = frank(ties.method = 'random',af_risk)/max(frank(ties.method = 'random',af_risk)),
                                  atrial_fibrillation_prevalence_prob = atrial_fibrillation_prevalence_prob/.N/model_specification$population$scale_down_factor),  by = 'HSCT']
base_population[ckd_risk!=0, `:=` (chronic_kidney_disease_disease_percentile = frank(ties.method = 'random',ckd_risk)/max(frank(ties.method = 'random',ckd_risk)),
                                   chronic_kidney_disease_prevalence_prob = chronic_kidney_disease_prevalence_prob/.N/model_specification$population$scale_down_factor),  by = 'HSCT']
base_population[chd_risk!=0, `:=` (chd_disease_percentile = frank(ties.method = 'random',chd_risk)/max(frank(ties.method = 'random',chd_risk)),
                                   chd_prevalence_prob = chd_prevalence_prob/.N/model_specification$population$scale_down_factor),  by = 'HSCT']
base_population[dementia_risk!=0, `:=` (dementia_disease_percentile = frank(ties.method = 'random',dementia_risk)/max(frank(ties.method = 'random',dementia_risk)),
                                        dementia_prevalence_prob = dementia_prevalence_prob/.N/model_specification$population$scale_down_factor),  by = 'HSCT']
base_population[hypertension_risk!=0, `:=` (hypertension_disease_percentile = frank(ties.method = 'random',hypertension_risk)/max(frank(ties.method = 'random',hypertension_risk)),
                                            hypertension_prevalence_prob = hypertension_prevalence_prob/.N/model_specification$population$scale_down_factor),  by = 'HSCT']
base_population[stroke_risk!=0, `:=` (stroke_disease_percentile = frank(ties.method = 'random',stroke_risk)/max(frank(ties.method = 'random',stroke_risk)),
                                      stroke_prevalence_prob = stroke_prevalence_prob/.N/model_specification$population$scale_down_factor),  by = 'HSCT']
base_population[diabetes_risk!=0, `:=` (diabetes_disease_percentile = frank(ties.method = 'random',diabetes_risk)/max(frank(ties.method = 'random',diabetes_risk)),
                                        diabetes_prevalence_prob = diabetes_prevalence_prob/.N/model_specification$population$scale_down_factor),  by = 'HSCT']
base_population[heart_failure_year_risk!=0, `:=` (heart_failure_disease_percentile = frank(ties.method = 'random',heart_failure_year_risk)/max(frank(ties.method = 'random',heart_failure_year_risk)),
                                                   heart_failure_prevalence_prob = heart_failure_prevalence_prob/.N/model_specification$population$scale_down_factor),  by = 'HSCT']
base_population[non_diabetic_hyperglycaemia_year_risk!=0, `:=` (non_diabetic_hyperglycaemia_disease_percentile = frank(ties.method = 'random',non_diabetic_hyperglycaemia_year_risk)/max(frank(ties.method = 'random',non_diabetic_hyperglycaemia_year_risk)),
                                                                 non_diabetic_hyperglycaemia_prevalence_prob = non_diabetic_hyperglycaemia_prevalence_prob/.N/model_specification$population$scale_down_factor),  by = 'HSCT']
base_population[asthma_year_risk!=0, `:=` (asthma_disease_percentile = frank(ties.method = 'random',asthma_year_risk)/max(frank(ties.method = 'random',asthma_year_risk)),
                                            asthma_prevalence_prob = asthma_prevalence_prob/.N/model_specification$population$scale_down_factor),  by = 'HSCT']
base_population[copd_year_risk!=0, `:=` (copd_disease_percentile = frank(ties.method = 'random',copd_year_risk)/max(frank(ties.method = 'random',copd_year_risk)),
                                          copd_prevalence_prob = copd_prevalence_prob/.N/model_specification$population$scale_down_factor),  by = 'HSCT']

# Initialize percentiles and prevalence_prob to 0 for rows with zero risk
base_population[af_risk==0, `:=` (atrial_fibrillation_disease_percentile = 0, atrial_fibrillation_prevalence_prob = 0)]
base_population[ckd_risk==0, `:=` (chronic_kidney_disease_disease_percentile = 0, chronic_kidney_disease_prevalence_prob = 0)]
base_population[chd_risk==0, `:=` (chd_disease_percentile = 0, chd_prevalence_prob = 0)]
base_population[dementia_risk==0, `:=` (dementia_disease_percentile = 0, dementia_prevalence_prob = 0)]
base_population[hypertension_risk==0, `:=` (hypertension_disease_percentile = 0, hypertension_prevalence_prob = 0)]
base_population[stroke_risk==0, `:=` (stroke_disease_percentile = 0, stroke_prevalence_prob = 0)]
base_population[diabetes_risk==0, `:=` (diabetes_disease_percentile = 0, diabetes_prevalence_prob = 0)]
base_population[heart_failure_year_risk==0, `:=` (heart_failure_disease_percentile = 0, heart_failure_prevalence_prob = 0)]
base_population[non_diabetic_hyperglycaemia_year_risk==0, `:=` (non_diabetic_hyperglycaemia_disease_percentile = 0, non_diabetic_hyperglycaemia_prevalence_prob = 0)]
base_population[asthma_year_risk==0, `:=` (asthma_disease_percentile = 0, asthma_prevalence_prob = 0)]
base_population[copd_year_risk==0, `:=` (copd_disease_percentile = 0, copd_prevalence_prob = 0)]

# Assign disease status probabilistically
base_population <- base_population[,
                `:=` (
    atrial_fibrillation = fifelse(runif(.N) <  atrial_fibrillation_prevalence_prob/0.5 * atrial_fibrillation_disease_percentile ,year,0),
    chronic_kidney_disease = fifelse(runif(.N) <  chronic_kidney_disease_prevalence_prob/0.5 * chronic_kidney_disease_disease_percentile ,year,0),
    chd = fifelse(runif(.N) <  chd_prevalence_prob/0.5 * chd_disease_percentile ,year,0),
    dementia = fifelse(runif(.N) <  dementia_prevalence_prob/0.5 * dementia_disease_percentile ,year,0),
    hypertension = fifelse(runif(.N) <  hypertension_prevalence_prob/0.5 * hypertension_disease_percentile ,year,0),
    stroke = fifelse(runif(.N) <  stroke_prevalence_prob/0.5 * stroke_disease_percentile ,year,0),
    diabetes = fifelse(runif(.N) <  diabetes_prevalence_prob/0.5 * diabetes_disease_percentile ,year,0),
    heart_failure = fifelse(runif(.N) <  heart_failure_prevalence_prob/0.5 * heart_failure_disease_percentile ,year,0),
    non_diabetic_hyperglycaemia = fifelse(runif(.N) <  non_diabetic_hyperglycaemia_prevalence_prob/0.5 * non_diabetic_hyperglycaemia_disease_percentile ,year,0),
    asthma = fifelse(runif(.N) <  asthma_prevalence_prob/0.5 * asthma_disease_percentile ,year,0),
    copd = fifelse(runif(.N) <  copd_prevalence_prob/0.5 * copd_disease_percentile ,year,0)
                )
    ]
     
base_population <- base_population[, c(
                   'cancer_disease_percentile',
                   'atrial_fibrillation_disease_percentile',
                   'chronic_kidney_disease_disease_percentile',
                   'chd_disease_percentile',
                   'dementia_disease_percentile',
                   'hypertension_disease_percentile',
                   'stroke_disease_percentile',
                   'diabetes_disease_percentile',
                   'heart_failure_disease_percentile',
                   'non_diabetic_hyperglycaemia_disease_percentile',
                   'asthma_disease_percentile',
                   'copd_disease_percentile') := NULL]

base_population <- base_population[, c(
  'cancer_prevalence_prob',
  'atrial_fibrillation_prevalence_prob',
  'chronic_kidney_disease_prevalence_prob',
  'chd_prevalence_prob',
  'dementia_prevalence_prob',
  'hypertension_prevalence_prob',
  'stroke_prevalence_prob',
  'diabetes_prevalence_prob',
  'heart_failure_prevalence_prob',
  'non_diabetic_hyperglycaemia_prevalence_prob',
  'asthma_prevalence_prob',
  'copd_prevalence_prob') := NULL]

# correct COPD for age-sex prevalence distribution

base_population <- populate_copd(base_population)
base_population <- populate_stroke(base_population)
base_population <- populate_dementia(base_population)
   
population_w_established_prevalence <- base_population

write.fst(population_w_established_prevalence,
          paste0('./3_pre_main/intermediate_populations/population_w_established_prevalence.fst'))

message('done pre main 2')


