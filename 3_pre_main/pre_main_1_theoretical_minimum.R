message('start pre main 1 theoretical minimum')
##########################################

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

base_population_w_physiological_and_modifiable_risk_factors <- read.fst('./3_pre_main/intermediate_populations/base_population_w_physiological_and_modifiable_risk_factors.fst')

safe_population <- base_population_w_physiological_and_modifiable_risk_factors

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

message('done pre main 1 theoretical minimum')


# write.fst(cervical_theoretical_minimum, 'cervical_theoretical_minimum.fst')
# write.fst(crc_theoretical_minimum, 'crc_theoretical_minimum.fst')
# write.fst(breast_theoretical_minimum, 'breast_theoretical_minimum.fst')
# write.fst(gallbladder_theoretical_minimum, 'gallbladder_theoretical_minimum.fst')
# write.fst(kidney_cancer_theoretical_minimum, 'kidney_cancer_theoretical_minimum.fst')
# write.fst(lung_cancer_theoretical_minimum, 'lung_cancer_theoretical_minimum.fst')
# write.fst(oesophageal_theoretical_minimum, 'oesophageal_theoretical_minimum.fst')
# write.fst(oral_theoretical_minimum, 'oral_theoretical_minimum.fst')
# write.fst(ovarian_theoretical_minimum, 'ovarian_theoretical_minimum.fst')
# write.fst(pancreatic_theoretical_minimum, 'pancreatic_theoretical_minimum.fst')
# write.fst(uterine_theoretical_minimum, 'uterine_theoretical_minimum.fst')
# write.fst(af_theoretical_minimum, 'af_theoretical_minimum.fst')
# write.fst(chd_theoretical_minimum, 'chd_theoretical_minimum.fst')
# write.fst(heart_failure_theoretical_minimum, 'heart_failure_theoretical_minimum.fst')
# write.fst(stroke_theoretical_minimum, 'stroke_theoretical_minimum.fst')
# write.fst(diabetes_theoretical_minimum, 'diabetes_theoretical_minimum.fst')
# write.fst(dementia_theoretical_minimum, 'dementia_theoretical_minimum.fst')
# write.fst(kidney_disease_theoretical_minimum, 'kidney_disease_theoretical_minimum.fst')
# write.fst(aaa_theoretical_minimum, 'aaa_theoretical_minimum.fst')
# write.fst(copd_theoretical_minimum, 'copd_theoretical_minimum.fst')
# write.fst(asthma_theoretical_minimum, 'asthma_theoretical_minimum.fst')
# write.fst(osteoarthritis_theoretical_minimum, 'osteoarthritis_theoretical_minimum.fst')
# write.fst(rheumatoid_arthritis_theoretical_minimum, 'rheumatoid_arthritis_theoretical_minimum.fst')
# write.fst(asthma_theoretical_minimum, 'asthma_theoretical_minimum.fst')
# write.fst(osteoporotic_fracture_theoretical_minimum, 'osteoporotic_fracture_theoretical_minimum.fst')
# write.fst(hip_fracture_theoretical_minimum, 'hip_fracture_theoretical_minimum.fst')
# write.fst(rheumatoid_arthritis_theoretical_minimum, 'rheumatoid_arthritis_theoretical_minimum.fst')
# write.fst(osteoarthritis_theoretical_minimum, 'osteoarthritis_theoretical_minimum.fst')


