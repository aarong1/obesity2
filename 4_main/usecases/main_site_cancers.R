# library(tidyverse)
# library(fst)
# 
# source('./risk_prevalence_operators/transform_probability.R')
# initial_time_zero_population = read.fst('./main/initial_time_zero_population10down.fst')
# 
# source('./populate_lung_cancer.R') 
# initial_time_zero_population <- populate_lung_cancer_prevalence(initial_time_zero_population)
# source('./populate_colorectal_cancer.R')
# initial_time_zero_population <- populate_colorectal_cancer_prevalence(initial_time_zero_population)
# source('./populate_stomach_cancer.R') 
# initial_time_zero_population <- populate_stomach_cancer_prevalence(initial_time_zero_population)
# source('./populate_prostate_cancer.R')
# initial_time_zero_population <- populate_prostate_cancer(initial_time_zero_population)
# source('./populate_female_breast_cancer.R')
# initial_time_zero_population <- populate_female_breast_cancer(initial_time_zero_population)
# source('./populate_kidney_cancer.R')
# initial_time_zero_population <- populate_kidney_cancer_prevalence(initial_time_zero_population)
# # source('./populate_oesphageal_gastric_cancer.R')
# # initial_time_zero_population <- populate_oesphageal_gastric_cancer_prevalence(initial_time_zero_population)
# source('./populate_oesophageal_cancer.R')
# source('./populate_stomach_cancer.R')
# initial_time_zero_population <- populate_oesophageal_cancer_prevalence(initial_time_zero_population)
# initial_time_zero_population <- populate_stomach_cancer_prevalence(initial_time_zero_population)
# 
# initial_time_zero_population <- initial_time_zero_population %>% 
#   mutate(oesphageal_gastric_cancer = coalesce( oesophageal_cancer,stomach_cancer))
# 
# source('./populate_oral_cancer.R')
# initial_time_zero_population <- populate_oral_cancer_prevalence(initial_time_zero_population)
# source('./populate_pancreatic_cancer.R')
# initial_time_zero_population <- populate_pancreatic_cancer_prevalence(initial_time_zero_population)
# source('./populate_uterine_cancer.R')
# initial_time_zero_population <- populate_uterine_cancer(initial_time_zero_population)
# 
# source('./populate_blood_multiple_myeloma.R')
# source('./populate_blood_lymphoma.R')
# source('./populate_blood_leukaemia.R')
# initial_time_zero_population <- populate_blood_multiple_myeloma_prevalence(initial_time_zero_population)
# initial_time_zero_population <- populate_blood_lymphoma_prevalence(initial_time_zero_population)
# initial_time_zero_population <- populate_blood_leukaemia_prevalence(initial_time_zero_population)
# 
# initial_time_zero_population <- initial_time_zero_population %>% 
#   mutate(blood_cancer = coalesce( oesophageal_cancer,stomach_cancer))
# 
# source('./populate_ovarian_cancer.R')
# initial_time_zero_population <- populate_ovarian_cancer(initial_time_zero_population)
# 
# # source('./populate_brain_cancer.R')
# # source('./populate_testicular_cancer.R')
# # source('./populate_thyroid_cancer.R')
# # source('./populate_liver_cancer.R')
# # source('./populate_gallbladder_cancer.R')
# # source('./populate_bladder_cancer.R')
# # source('./populate_cervical_cancer.R')
# 
# 
#     # BREAST CANCER (F)
#    source("./risk_correct_eq/site_cancers/breast_female_cancer.R")
#    
#    initial_time_zero_population <- initial_time_zero_population |>
#      apply_breastcancer_risk_wo_risk_factors() |>
#      mutate(female_breast_cancer_year_risk = transform_probability_to_1y(breastcancer_risk, tot_years = 5))
# 
#   
#     # PROSTATE (M)
#    source("./risk_correct_eq/site_cancers/prostate_male_cancer.R")
#    
#    initial_time_zero_population <- initial_time_zero_population |>
#      apply_prostate_cancer_risk_wo_risk_factors() |>
#      mutate(prostate_cancer_year_risk = transform_probability_to_1y(prostate_cancer_risk, tot_years = 5))
# 
#   
#     # PANCREATIC
#    source("./risk_correct_eq/site_cancers/pancreatic_cancer.R")
#    
#    initial_time_zero_population <- initial_time_zero_population |>
#      apply_pancreatic_cancer_risk_wo_risk_factors() |>
#      mutate(pancreatic_cancer_year_risk = transform_probability_to_1y(pancreatic_cancer_risk, tot_years = 5))
#   
#   
#     # RENAL 
#    source("./risk_correct_eq/site_cancers/renal_cancer.R")
# 
#    initial_time_zero_population <- initial_time_zero_population |>
#      apply_renal_cancer_risk_wo_risk_factors() |>
#      mutate(renal_cancer_year_risk = transform_probability_to_1y(renal_cancer_risk, tot_years = 5))
#   
#     # UTERINE (F)
#    source("./risk_correct_eq/site_cancers/uterine_female_cancer.R")
#   
#    initial_time_zero_population <- initial_time_zero_population |>
#      apply_uterian_cancer_risk_wo_risk_factors() |>
#      mutate(uterine_cancer_year_risk = transform_probability_to_1y(uterine_cancer_risk, tot_years = 5))
#   
#     # OVARIAN (F)
#    source("./risk_correct_eq/site_cancers/ovarian_female_cancer.R")
#    
#    initial_time_zero_population <- initial_time_zero_population |>
#      apply_ovariancancer_risk_wo_risk_factors() |>
#      mutate(ovariancancer_year_risk = transform_probability_to_1y(ovariancancer_risk, tot_years = 5))
# 
#     # BLOOD CANCER
#    source("./risk_correct_eq/site_cancers/blood_cancer.R")
#   
#    initial_time_zero_population <- initial_time_zero_population |>
#      apply_bloodcancer_risk_wo_risk_factors() |>
#      mutate(bloodcancer_year_risk = transform_probability_to_1y(bloodcancer_risk, tot_years = 5))
# 
#     # OESPHAGEAL-GASTRIC
#    source("./risk_correct_eq/site_cancers/oesteogastric_cancer.R")
#    
#    initial_time_zero_population <- initial_time_zero_population |>
#      apply_osteogastric_cancer_risk_wo_risk_factors() |>
#      mutate(osteogastric_year_risk = transform_probability_to_1y(osteogastric_risk, tot_years = 5))
#   
#     # ORAL
#    source("./risk_correct_eq/site_cancers/oral_cancer.R")
#   
#    initial_time_zero_population <- initial_time_zero_population |> 
#      apply_oralcancer_risk_wo_risk_factors() |>
#      mutate(oralcancer_year_risk = transform_probability_to_1y(oralcancer_risk, tot_years = 5))
#   