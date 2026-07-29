message('start pre main 3')

# source('./main/pre_main_2.4.R')
# source('./reindex_risk_percentile.R')
# source('./obesity_intervention/engine_bmi.R')

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

# source('./disease_engines/low_birth_weight_engine.R')
# source('./disease_engines/non_alcoholic_fatty_liver_disease.R')
# source('./disease_engines/alcohol_related_liver_disease.R')
# source('./disease_engines/risk_data_congential_heart_disease.R')
# source('./disease_engines/risk_data_gestational_diabetes.R')

population_w_established_prevalence <- read.fst('./3_pre_main/intermediate_populations/population_w_established_prevalence.fst')

count(population_w_established_prevalence,age,bmi)

# count(population_w_established_prevalence, sex)

safe_population <- population_w_established_prevalence
# initial_time_zero_population
# base_population_w_physiological_and_modifiable_risk_factors

# safe_population[1:10, tail(names(safe_population), 5), with = FALSE]

cervical_theoretical_minimum <- calculate_cervical_theoretical_min(safe_population)
crc_theoretical_minimum <- calculate_crc_theoretical_min(safe_population)
breast_theoretical_minimum <- calculate_breast_theoretical_min(safe_population)
gallbladder_theoretical_minimum <- calculate_gallbladder_theoretical_min(safe_population)
kidney_cancer_theoretical_minimum <- calculate_kidney_cancer_theoretical_min(safe_population)

lung_cancer_theoretical_minimum <- calculate_lung_cancer_theoretical_min(safe_population)

oesophageal_theoretical_minimum <- calculate_oesophageal_theoretical_min(safe_population)
oral_theoretical_minimum <- calculate_oral_theoretical_min(safe_population)
ovarian_theoretical_minimum <- calculate_ovarian_theoretical_min(safe_population)
pancreatic_theoretical_minimum <- calculate_pancreatic_theoretical_min(safe_population)
uterine_theoretical_minimum <- calculate_uterine_theoretical_min(safe_population)

af_theoretical_minimum <- calculate_af_theoretical_min(safe_population)
chd_theoretical_minimum <- calculate_chd_theoretical_min(safe_population)
heart_failure_theoretical_minimum <- calculate_heart_failure_theoretical_min(safe_population)
stroke_theoretical_minimum <- calculate_stroke_theoretical_min(safe_population)
diabetes_theoretical_minimum <- calculate_diabetes_theoretical_min(safe_population)

dementia_theoretical_minimum <- calculate_dementia_theoretical_min(safe_population)

kidney_disease_theoretical_minimum <- calculate_kidney_disease_theoretical_min(safe_population)

aaa_theoretical_minimum <- calculate_aaa_theoretical_min(safe_population)

copd_theoretical_minimum <- calculate_copd_theoretical_min(safe_population)
asthma_theoretical_minimum <- calculate_asthma_theoretical_min(safe_population)

osteoarthritis_theoretical_minimum <- calculate_oa_theoretical_min(safe_population)
rheumatoid_arthritis_theoretical_minimum <- calculate_ra_theoretical_min(safe_population)

asthma_theoretical_minimum <- calculate_asthma_theoretical_min(safe_population)

osteoporotic_fracture_theoretical_minimum <- calculate_osteoporotic_fracture_theoretical_min(safe_population)
hip_fracture_theoretical_minimum <- calculate_hip_fracture_theoretical_min(safe_population)
rheumatoid_arthritis_theoretical_minimum <- calculate_ra_theoretical_min(safe_population)
osteoarthritis_theoretical_minimum <- calculate_oa_theoretical_min(safe_population)

#Apply risks
safe_population <- apply_ra_risk_factors(safe_population, rheumatoid_arthritis_theoretical_minimum)
safe_population <- apply_oa_risk_factors(safe_population,osteoarthritis_theoretical_minimum )

safe_population <- apply_cervical_risk_factors(safe_population, cervical_theoretical_minimum)
safe_population <- apply_crc_risk_factors(safe_population, crc_theoretical_minimum)

# safe_population <- population_w_established_prevalence
# count(safe_population,female_breast_cancer)
# safe_population <- populate_female_breast_cancer(safe_population)
safe_population <- apply_breast_risk_factors(safe_population, breast_theoretical_minimum)
# count(safe_population,female_breast_cancer)

safe_population <- apply_gallbladder_risk_factors(safe_population, gallbladder_theoretical_minimum)
safe_population <- apply_kidney_cancer_risk_factors(safe_population, kidney_cancer_theoretical_minimum)

safe_population <- apply_lung_cancer_risk_factors(safe_population, lung_cancer_theoretical_minimum)

safe_population <- apply_oesophageal_risk_factors(safe_population, oesophageal_theoretical_minimum)
safe_population <- apply_oral_risk_factors(safe_population, oral_theoretical_minimum)
safe_population <- apply_ovarian_risk_factors(safe_population, ovarian_theoretical_minimum)
safe_population <- apply_pancreatic_risk_factors(safe_population, pancreatic_theoretical_minimum)
safe_population <- apply_uterine_risk_factors(safe_population, uterine_theoretical_minimum)

safe_population <- apply_prostate_risk_engine_age_sex(safe_population )
safe_population <- apply_brain_cancer_risk_wo_risk_factors(safe_population )

safe_population <- apply_af_risk_factors(safe_population, af_theoretical_minimum)
safe_population <- apply_chd_risk_factors(safe_population, chd_theoretical_minimum)
safe_population <- apply_heart_failure_risk_factors(safe_population, heart_failure_theoretical_minimum)
safe_population <- apply_stroke_risk_factors(safe_population, stroke_theoretical_minimum)
safe_population <- apply_diabetes_risk_factors(safe_population, diabetes_theoretical_minimum)

safe_population <- apply_dementia_risk_factors(safe_population, dementia_theoretical_minimum)
safe_population <- apply_kidney_disease_risk_factors(safe_population, kidney_disease_theoretical_minimum)

safe_population <- apply_asthma_risk_factors(safe_population, asthma_theoretical_minimum)
safe_population <- apply_copd_risk_factors(safe_population, copd_theoretical_minimum)

safe_population <- apply_aaa_risk_factors(safe_population,aaa_theoretical_minimum )

safe_population <- apply_osteoporotic_fracture_risk_factors(safe_population, osteoporotic_fracture_theoretical_minimum)
safe_population <- apply_hip_fracture_risk_factors(safe_population, hip_fracture_theoretical_minimum)

safe_population <- apply_ra_risk_factors(safe_population, rheumatoid_arthritis_theoretical_minimum)
safe_population <- apply_oa_risk_factors(safe_population, osteoarthritis_theoretical_minimum )

safe_population <- apply_type1_diabetes_risk_engine_age_sex(safe_population)

safe_population <- apply_hypothyroid_risk(safe_population )
safe_population <- apply_epilepsy_risk(safe_population )

safe_population <- apply_ILD_risk(safe_population )
safe_population <- apply_sle_risk(safe_population )

safe_population <-  apply_ibd_risk(safe_population )

safe_population <- apply_osteoporosis_risk(safe_population )

safe_population <- apply_arld_risk(safe_population)
safe_population <- apply_nafld_risk(safe_population)

# hypertension_engine.R
# apply_crohns_risk()
# apply_ulcerative_colitis_risk()

initial_time_zero_population <- safe_population

write.fst(initial_time_zero_population,
          paste0('./3_pre_main/intermediate_populations/initial_time_zero_population.fst'))

write.fst(cervical_theoretical_minimum, './3_pre_main/theoretical_minimum/cervical_theoretical_minimum.fst')
write.fst(crc_theoretical_minimum, './3_pre_main/theoretical_minimum/crc_theoretical_minimum.fst')
write.fst(breast_theoretical_minimum, './3_pre_main/theoretical_minimum/breast_theoretical_minimum.fst')
write.fst(gallbladder_theoretical_minimum, './3_pre_main/theoretical_minimum/gallbladder_theoretical_minimum.fst')
write.fst(kidney_cancer_theoretical_minimum, './3_pre_main/theoretical_minimum/kidney_cancer_theoretical_minimum.fst')
write.fst(lung_cancer_theoretical_minimum, './3_pre_main/theoretical_minimum/lung_cancer_theoretical_minimum.fst')
write.fst(oesophageal_theoretical_minimum, './3_pre_main/theoretical_minimum/oesophageal_theoretical_minimum.fst')
write.fst(oral_theoretical_minimum, './3_pre_main/theoretical_minimum/oral_theoretical_minimum.fst')
write.fst(ovarian_theoretical_minimum, './3_pre_main/theoretical_minimum/ovarian_theoretical_minimum.fst')
write.fst(pancreatic_theoretical_minimum, './3_pre_main/theoretical_minimum/pancreatic_theoretical_minimum.fst')
write.fst(uterine_theoretical_minimum, './3_pre_main/theoretical_minimum/uterine_theoretical_minimum.fst')
write.fst(af_theoretical_minimum, './3_pre_main/theoretical_minimum/af_theoretical_minimum.fst')
write.fst(chd_theoretical_minimum, './3_pre_main/theoretical_minimum/chd_theoretical_minimum.fst')
write.fst(heart_failure_theoretical_minimum, './3_pre_main/theoretical_minimum/heart_failure_theoretical_minimum.fst')
write.fst(stroke_theoretical_minimum, './3_pre_main/theoretical_minimum/stroke_theoretical_minimum.fst')
write.fst(diabetes_theoretical_minimum, './3_pre_main/theoretical_minimum/diabetes_theoretical_minimum.fst')
write.fst(dementia_theoretical_minimum, './3_pre_main/theoretical_minimum/dementia_theoretical_minimum.fst')
write.fst(kidney_disease_theoretical_minimum, './3_pre_main/theoretical_minimum/kidney_disease_theoretical_minimum.fst')
write.fst(aaa_theoretical_minimum, './3_pre_main/theoretical_minimum/aaa_theoretical_minimum.fst')
write.fst(copd_theoretical_minimum, './3_pre_main/theoretical_minimum/copd_theoretical_minimum.fst')
write.fst(asthma_theoretical_minimum, './3_pre_main/theoretical_minimum/asthma_theoretical_minimum.fst')
write.fst(osteoarthritis_theoretical_minimum, './3_pre_main/theoretical_minimum/osteoarthritis_theoretical_minimum.fst')
write.fst(rheumatoid_arthritis_theoretical_minimum, './3_pre_main/theoretical_minimum/rheumatoid_arthritis_theoretical_minimum.fst')
write.fst(asthma_theoretical_minimum, './3_pre_main/theoretical_minimum/asthma_theoretical_minimum.fst')
write.fst(osteoporotic_fracture_theoretical_minimum, './3_pre_main/theoretical_minimum/osteoporotic_fracture_theoretical_minimum.fst')
write.fst(hip_fracture_theoretical_minimum, './3_pre_main/theoretical_minimum/hip_fracture_theoretical_minimum.fst')
write.fst(rheumatoid_arthritis_theoretical_minimum, './3_pre_main/theoretical_minimum/rheumatoid_arthritis_theoretical_minimum.fst')
write.fst(osteoarthritis_theoretical_minimum, './3_pre_main/theoretical_minimum/osteoarthritis_theoretical_minimum.fst')

message('done pre main 3')

