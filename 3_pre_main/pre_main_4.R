library(tidyverse)
library(fst)
library(data.table)

source('./main/main_configuration.R') # model_specification list
source('./main/main_utils_2_4.R')
source('./main/main_duckdb.R')

# source('./main/pre_main_2.4.R')
source('./reindex_risk_percentile.R')
source('./obesity_intervention/engine_bmi.R')

# source('./disease_engines/stroke_engine.R')
# source('./disease_engines/chd_engine.R')
# source('./disease_engines/diabetes_engine.R')
# asthma
# osteoporosis
# osteoarthritis

source("./disease_engines/cervical_cancer.R")
source("./disease_engines/brain_cancer.R")

source("./disease_engines/asthma_engine.R")
source("./disease_engines/copd_engine.R")

# source("./disease_engines/ILD_engine.R")
source("./disease_engines/AAA_engine.R")

source("./disease_engines/epilepsy_engine.R")
source('./disease_engines/hypothyroid_engine.R')
source("./disease_engines/RA_engine.R")
source("./disease_engines/osteoporosis_engine.R")
source("./disease_engines/osteoarthritis_engine.R")

source('./disease_engines/low_birth_weight_engine.R')

#####
# NEW Disease Engines #
#####

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

####
# Deaths #
####

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
source('./populate_time_zero_cancers_prevalence/populate_oral_cancer.R')
source('./populate_time_zero_cancers_prevalence/populate_pancreatic_cancer.R')
source('./populate_time_zero_cancers_prevalence/populate_uterine_cancer.R')
source('./populate_time_zero_cancers_prevalence/populate_blood_multiple_myeloma.R')
source('./populate_time_zero_cancers_prevalence/populate_blood_lymphoma.R')
source('./populate_time_zero_cancers_prevalence/populate_blood_leukaemia.R')
source('./populate_time_zero_cancers_prevalence/populate_ovarian_cancer.R')

source('./populate_time_zero_cancers_prevalence/populate_brain_cancer.R')
source('./populate_time_zero_cancers_prevalence/populate_cervical_cancer.R')

source('./disease_engines/cancer_engine.R')

source('./disease_equation/risk_qcancer_lungcancer.R')

source("./disease_equation/risk_qcancer_colorectal.R")

# ORAL CANCER
source("./disease_equation/site_cancers/oral_cancer.R")
# PANCREATIC CANCER
source("./disease_equation/site_cancers/pancreatic_cancer.R")
# UTERINE (F) CANCER
source("./disease_equation/site_cancers/uterine_female_cancer.R")
# BLOOD CANCER
source("./disease_equation/site_cancers/blood_cancer.R")
# OVARIAN (F) CANCER
source("./disease_equation/site_cancers/ovarian_female_cancer.R")
# OESPHAGEAL-GASTRIC
source("./disease_equation/site_cancers/oesteogastric_cancer.R")
# PROSTATE (M) CANCER
source("./disease_equation/site_cancers/prostate_male_cancer.R")
# BREAST CANCER (F)
source("./disease_equation/site_cancers/breast_female_cancer.R")
# RENAL CANCER
source("./disease_equation/site_cancers/renal_cancer.R")



#load population
# initial_time_zero_population = read.fst('./main/initial_time_zero_population10down.fst')

#re initialise configuration
# initial_time_zero_population <- initial_time_zero_population %>% slice_sample(prop=0.1)
# model_specification$population$scale_down_factor = model_specification$population$scale_down_factor/0.1

# model_specification$model$duration = 8
# model_specification$model$number_of_runs = 10

#Set population start year
initial_time_zero_population$year <- model_specification$model$start_year 

initial_time_zero_population <- initial_time_zero_population %>% 
  apply_pollution_lifestyle_parameter_geography_constant(lookup_dz_raster_cell)

# apply osteoarthritis risk

# initial_time_zero_population <- initial_time_zero_population %>% 
#   apply_osteoporosis_risk() %>% 
#   apply_hypothyroid_risk() %>% 
#   apply_epilepsy_risk() %>%  # Qof
#   apply_ibd_risk() %>%
#   apply_sle_risk() %>% 
#   apply_liver_disease_risk_wo_risk_factors()

initial_time_zero_population$copd = 0
initial_time_zero_population$asthma = 0
initial_time_zero_population$interstitial_lung_disease = 0

initial_time_zero_population$depression = 0

initial_time_zero_population$abdominal_aortic_aneurysm = 0
initial_time_zero_population$liver_disease = 0
initial_time_zero_population$hypothyroidism = 0
initial_time_zero_population$epilepsy = 0
initial_time_zero_population$ibd = 0
initial_time_zero_population$sle = 0

initial_time_zero_population$osteoporosis = 0
initial_time_zero_population$osteoarthritis = 0
initial_time_zero_population$rheumatoid_arthritis = 0

initial_time_zero_population$pad = 0
initial_time_zero_population$non_diabetic_hyperglycaemia = 0
initial_time_zero_population$abdominable_aortic_aneurysm = 0

initial_time_zero_population$cancer = 0
initial_time_zero_population$osteogastric_cancer = 0
initial_time_zero_population$osteoc_cancer = 0

initial_time_zero_population$prostate_cancer = 0
initial_time_zero_population$female_breast_cancer = 0
initial_time_zero_population$renal_cancer = 0
initial_time_zero_population$oral_cancer = 0
initial_time_zero_population$pancreatic_cancer = 0
initial_time_zero_population$uterine_cancer = 0
initial_time_zero_population$blood_cancer = 0
initial_time_zero_population$ovarian_cancer = 0

initial_time_zero_population$lung_cancer <- 0
initial_time_zero_population$colorectal_cancer <- 0

initial_time_zero_population <- initial_time_zero_population %>%
  apply_brain_cancer_risk_wo_risk_factors() 

initial_time_zero_population <- populate_brain_cancer_prevalence(initial_time_zero_population)

initial_time_zero_population <- initial_time_zero_population %>% 
  apply_cervical_cancer_risk_wo_risk_factors()

initial_time_zero_population <- populate_cervical_cancer_prevalence(initial_time_zero_population)

initial_time_zero_population <- initial_time_zero_population %>%
  apply_ovarian_cancer_risk_wo_risk_factors() %>%
  mutate(ovarian_cancer_year_risk = transform_probability_to_1y(ovarian_cancer_risk, tot_years = 5))

initial_time_zero_population <- populate_ovarian_cancer(initial_time_zero_population)

initial_time_zero_population <- initial_time_zero_population %>%
  apply_breast_cancer_risk_wo_risk_factors() %>%
  mutate(female_breast_cancer_year_risk = transform_probability_to_1y(breast_cancer_risk, tot_years = 5))

initial_time_zero_population <- populate_female_breast_cancer(initial_time_zero_population)

initial_time_zero_population <- initial_time_zero_population %>%
  apply_prostate_cancer_risk_wo_risk_factors() %>%
  mutate(prostate_cancer_year_risk = transform_probability_to_1y(prostate_cancer_risk, tot_years = 5))

initial_time_zero_population <- populate_prostate_cancer(initial_time_zero_population)

initial_time_zero_population <- initial_time_zero_population %>% 
  apply_colorectal_cancer_risk_wo_risk_factors() %>%
  mutate(colorectal_year_risk = transform_probability_to_1y(colorectal_cancer_risk, tot_years = 5))

initial_time_zero_population <- populate_colorectal_cancer_prevalence(initial_time_zero_population)

initial_time_zero_population <- initial_time_zero_population %>%
  apply_renal_cancer_risk_wo_risk_factors() %>%
  mutate(renal_cancer_year_risk = transform_probability_to_1y(renal_cancer_risk, tot_years = 5))

initial_time_zero_population <- populate_renal_cancer_prevalence(initial_time_zero_population)

initial_time_zero_population <- initial_time_zero_population %>%
  apply_pancreatic_cancer_risk_wo_risk_factors() %>%
  mutate(pancreatic_cancer_year_risk = transform_probability_to_1y(pancreatic_cancer_risk, tot_years = 5))

initial_time_zero_population <- populate_pancreatic_cancer_prevalence(initial_time_zero_population)

initial_time_zero_population <- initial_time_zero_population %>%
  apply_blood_cancer_risk_wo_risk_factors() %>%
  mutate(blood_cancer_year_risk = transform_probability_to_1y(blood_cancer_risk, tot_years = 5))

# initial_time_zero_population <- populate_blood_cancer_prevalence(initial_time_zero_population)

initial_time_zero_population <- populate_blood_multiple_myeloma_prevalence(initial_time_zero_population)
initial_time_zero_population <- populate_blood_lymphoma_prevalence(initial_time_zero_population)
initial_time_zero_population <- populate_blood_leukaemia_prevalence(initial_time_zero_population)

initial_time_zero_population <- initial_time_zero_population %>% 
  mutate(blood_cancer = case_when( blood_multiple_myeloma != 0 ~ min(year),
                                   blood_lymphoma != 0 ~ min(year),
                                   blood_leukaemia != 0 ~ min(year),
                                   T ~ 0  ))

initial_time_zero_population <- initial_time_zero_population %>% 
  apply_oral_cancer_risk_wo_risk_factors() %>%
  mutate(oral_cancer_year_risk = transform_probability_to_1y(oral_cancer_risk, tot_years = 5))

initial_time_zero_population <- populate_oral_cancer_prevalence(initial_time_zero_population)

initial_time_zero_population <- initial_time_zero_population %>%
  apply_uterian_cancer_risk_wo_risk_factors() %>%
  mutate(uterine_cancer_year_risk = transform_probability_to_1y(uterine_cancer_risk, tot_years = 5))

initial_time_zero_population <- populate_uterine_cancer_prevalence(initial_time_zero_population)

initial_time_zero_population <- initial_time_zero_population %>%
  apply_osteogastric_cancer_risk_wo_risk_factors() %>%
  mutate(osteogastric_cancer_year_risk = transform_probability_to_1y(osteogastric_cancer_risk, tot_years = 5))

# initial_time_zero_population <- populate_osteogastric_cancer_prevalence(initial_time_zero_population)
initial_time_zero_population <- populate_oesophageal_cancer_prevalence(initial_time_zero_population)
initial_time_zero_population <- populate_stomach_cancer_prevalence(initial_time_zero_population)

initial_time_zero_population <- initial_time_zero_population %>% 
  mutate(osteogastric_cancer = case_when( oesophageal_cancer != 0 ~ min(year), 
                                          stomach_cancer != 0 ~ min(year),
                                          T ~0 ))

initial_time_zero_population <- initial_time_zero_population %>% 
  apply_lung_cancer_risk_wo_risk_factors() %>%
  mutate(lung_cancer_year_risk = transform_probability_to_1y(lungcancer_risk, tot_years = 5))

initial_time_zero_population <- populate_lung_cancer_prevalence(initial_time_zero_population)

initial_time_zero_population <- populate_all_cancers_prevalence(initial_time_zero_population)


#Deaths
dead_population <- data.frame()
initial_time_zero_population$death <- 0
initial_time_zero_population$death_reason <- '-1'
initial_time_zero_population$qmortality_risk <- 0.0
initial_time_zero_population$qx <- 0.0


prevalence_hsct_new <- prevalence_hsct  %>% 
  # group_by(Disease,) %>% 
  arrange(Disease,HSCT, Year) %>% 
  fill(prob,.direction = 'down') # %>% 

#Births
initial_time_zero_population$mothers_age <- NA
initial_time_zero_population$mothers_id <- '-1'

# initial_time_zero_population$pad = 0
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
initial_time_zero_population$interstitial_lung_disease_recovered = NA
initial_time_zero_population$abdominable_aortic_aneurysm_recovered = NA
initial_time_zero_population$liver_disease_recovered = NA

initial_time_zero_population$cancer_recovered <- NA
initial_time_zero_population$osteogastric_cancer_recovered <- NA
initial_time_zero_population$prostate_cancer_recovered <- NA
initial_time_zero_population$female_breast_cancer_recovered <- NA
initial_time_zero_population$renal_cancer_recovered <- NA
# initial_time_zero_population$oesophageal_cancer_recovered <- NA
# initial_time_zero_population$oesphageal_cancer_recovered <- NA
initial_time_zero_population$oral_cancer_recovered <- NA
initial_time_zero_population$pancreatic_cancer_recovered <- NA
initial_time_zero_population$uterine_cancer_recovered <- NA
initial_time_zero_population$blood_cancer_recovered <- NA
initial_time_zero_population$ovarian_cancer_recovered <- NA

trusts <- c("BHSCT", "NHSCT","SHSCT", "WHSCT","SEHSCT")
morbidities <- c( 'non_diabetic_hyperglycaemia', 'copd', 'asthma', 'depression', 'atrial_fibrillation', 'cancer', 
                  'chronic_kidney_disease', 'chd', 'dementia', 'hypertension', 'stroke', 'diabetes', 'heart_failure',
                  'rheumatoid_arthritis'#, #'epilepsy', 'pad'
                  # 'osteoporosis','hypothyroidism'
)

initial_time_zero_population <- populate_all_cancers_prevalence(initial_time_zero_population)

# initial_time_zero_population <- populate_prostate_cancer(initial_time_zero_population)
# initial_time_zero_population <- populate_female_breast_cancer(initial_time_zero_population)
# initial_time_zero_population <- populate_renal_cancer_prevalence(initial_time_zero_population)
# initial_time_zero_population <- populate_oesophageal_cancer_prevalence(initial_time_zero_population)
# initial_time_zero_population <- populate_stomach_cancer_prevalence(initial_time_zero_population)
# 
# initial_time_zero_population <- initial_time_zero_population %>% 
#   mutate(osteogastric_cancer = case_when( oesophageal_cancer != 0 ~ min(year), 
#                                           stomach_cancer != 0 ~ min(year),
#                                           T ~0 ))
# # count(initial_time_zero_population,stomach_cancer,oesophageal_cancer,osteogastric_cancer)
# 
# initial_time_zero_population <- populate_oral_cancer_prevalence(initial_time_zero_population)
# initial_time_zero_population <- populate_pancreatic_cancer_prevalence(initial_time_zero_population)
# initial_time_zero_population <- populate_uterine_cancer(initial_time_zero_population);count(initial_time_zero_population,uterine_cancer)
# 
# initial_time_zero_population <- populate_blood_multiple_myeloma_prevalence(initial_time_zero_population)
# initial_time_zero_population <- populate_blood_lymphoma_prevalence(initial_time_zero_population)
# initial_time_zero_population <- populate_blood_leukaemia_prevalence(initial_time_zero_population)
# 
# initial_time_zero_population <- initial_time_zero_population %>% 
#   mutate(blood_cancer = case_when( blood_multiple_myeloma != 0 ~ min(year),
#                                    blood_lymphoma != 0 ~ min(year),
#                                    blood_leukaemia != 0 ~ min(year),
#                                    T ~ 0  ))

# count(initial_time_zero_population, blood_multiple_myeloma, blood_lymphoma, blood_leukaemia, blood_cancer)

# initial_time_zero_population <- populate_ovarian_cancer(initial_time_zero_population)
# initial_time_zero_population <- populate_colorectal_cancer_prevalence(initial_time_zero_population)
# initial_time_zero_population <- populate_lung_cancer_prevalence(initial_time_zero_population)
#count(initial_time_zero_population,lung_cancer)

initial_time_zero_population <- apply_osteoarthritis_prevalence(initial_time_zero_population ) 
initial_time_zero_population <- populate_epilepsy_prevalence(initial_time_zero_population)

initial_time_zero_population <- reduce2(
  .x = rep(trusts,length(morbidities)),
  .y = rep(morbidities,each = length(trusts)),
  .init = initial_time_zero_population,
  .f = function(pop, trust, morbidity) {
    assign_year_minus_one_prevalence(
      input_population = pop,#initial_time_zero_population,#pop,
      trust = trust,
      morbidity = morbidity,
      #year = 2017,
      prevalence_df = prevalence_hsct_new,
      configuration = model_specification
    )
  }
)

initial_time_zero_population <- initial_time_zero_population %>% 
  apply_doh_disease_prevalence(morbidity = 'osteoporosis') %>% 
  apply_doh_disease_prevalence(morbidity = 'hypothyroidism') %>% 
  apply_doh_disease_prevalence(morbidity = 'pad') 

initial_time_zero_population <- initial_time_zero_population %>% 
  mutate(run = run1)

initial_time_zero_population <- initial_time_zero_population %>% 
  mutate(bern_trial = runif(n()))

####################################################################
####################################################################
####################################################################

initial_time_zero_population <- initial_time_zero_population %>% 
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
                right = TRUE)
  )

####################################################################
####################################################################
####################################################################

initial_time_zero_population <- initial_time_zero_population %>%
  filter(is.na(death)| is.null(death)| death==0)

initial_time_zero_population <- initial_time_zero_population %>% 
  apply_age_sex_death(apply_death = T) #%>% 
  # apply_qmortality_mortality(apply_death = F)

initial_time_zero_population$id <- as.character(initial_time_zero_population$id)

initial_time_zero_population <- initial_time_zero_population %>% 
  asfr_births(fertility) %>% 
  apply_low_birth_weight_risk()# %>% 
  # apply_preterm_birth_risk()
  #https://pmc.ncbi.nlm.nih.gov/articles/PMC5860004/#kwx177s3

####################################################################
####################################################################
####################################################################

initial_time_zero_population <- initial_time_zero_population %>% 
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

# initial_time_zero_population$osteogastric_cancer <- NA
# initial_time_zero_population$renal_cancer <- NA

initial_time_zero_population <- initial_time_zero_population %>% 
  
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

# initial_time_zero_population <- initial_time_zero_population %>% 
#   apply_age_sex_death(apply_death = T) 

initial_time_zero_population <- initial_time_zero_population %>%

  # apply_case_death( morbidity = 'cancer') %>%
  apply_case_death( morbidity = 'stroke') %>%
  apply_case_death( morbidity = 'chd') %>%
  
  apply_case_death( morbidity = 'diabetes') %>%
  apply_case_death( morbidity = 'asthma') %>%
  apply_case_death( morbidity = 'copd') %>%
  
  apply_case_death( morbidity = 'chronic_kidney_disease') %>%
  apply_case_death( morbidity = 'dementia') %>%
  apply_case_death( morbidity = 'heart_failure')

initial_time_zero_population <- initial_time_zero_population %>%
  apply_case_death( morbidity = 'lung_cancer') %>%
  apply_case_death( morbidity = 'colorectal_cancer') %>%
  apply_case_death( morbidity = 'oral_cancer') %>%
  apply_case_death( morbidity = 'pancreatic_cancer') %>%
  apply_case_death( morbidity = 'uterine_cancer') %>%
  apply_case_death( morbidity = 'blood_cancer') %>%
  apply_case_death( morbidity = 'ovarian_cancer') %>%
  apply_case_death( morbidity = 'osteogastric_cancer') %>%
  apply_case_death( morbidity = 'prostate_cancer') %>%
  apply_case_death( morbidity = 'female_breast_cancer') %>%
  apply_case_death( morbidity = 'renal_cancer')

time_one_population <- initial_time_zero_population

message('done pre main 4')

