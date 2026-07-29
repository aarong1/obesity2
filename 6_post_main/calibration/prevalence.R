'site_cancers'

'Stroke'
'CHD'
'Heart Failure'
'Atrial Fibrillation'
'Hypertension'
'Diabetes (1/2)'
'NDH'
'Cancer'
'Asthma'
'COPD'
'Dementia'
'Chronic Kidney Disease'

'Hypothyroidism'
'Epilepsy'
'Osteoporosis'
'Rhematoid Arthritis'
'Peripheral Arterial Disease'

( prevalence <- read_excel("data/DiseasePrevDoH_2324.xlsx", 
                           sheet = "Table 1 Prevalence Registers", 
                           skip = 2) %>% 
    rename('Disease' = 1) %>% 
    pivot_longer(-Disease,
                 names_to = 'Year',
                 values_to = 'Count') %>% 
    arrange(Disease, Year) %>% 
    group_by(Disease) %>%
    fill(Count,.direction = 'down')  %>% filter(Year == max(Year))
  )

'Osteoarthritis'
'Aortic Aneurysm'
'Systemic Lupus Erythematosus'
'Inflammatory Bowel Disease'
'Intersistal Lung Disease'
'Liver Disease'

'~/Documents/SIB/PHM/PHModel/new_files/osteoarthritis.R'
'~/Documents/SIB/PHM/PHModel/new_files/abdominal_aortic_aneurysm.R'
'~/Documents/SIB/PHM/PHModel/new_files/sle.R'
'~/Documents/SIB/PHM/PHModel/new_files/IBD.R'
'~/Documents/SIB/PHM/PHModel/new_files/interstital_lung_disease.R'
'~/Documents/SIB/PHM/PHModel/new_files/liver_cirrhosis.R'


read_excel("data/NIcancer_registry/all_cancers_data_tables.xlsx", sheet = "T19", skip = 6)                             
read_excel("data/NIcancer_registry/Bladder cancer data tables.xlsx", sheet = "T15", skip = 6)                          
read_excel("data/NIcancer_registry/Blood_leukaemia_data_tables.xlsx", sheet = "T13", skip = 7)                         
read_excel("data/NIcancer_registry/Blood_lymphoma_data_tables.xlsx", sheet = "T15", skip = 7)                          
read_excel("data/NIcancer_registry/Blood_Multiple_myeloma_data_tables.xlsx", sheet = "T13", skip = 7)                  
read_excel("data/NIcancer_registry/Brain cancer data tables.xlsx", sheet = "T13", skip = 7)                            
read_excel("data/NIcancer_registry/Cervical cancer data tables.xlsx", sheet = "T15", skip = 7)                         
read_excel("data/NIcancer_registry/Female_breast_cancer_data_tables.xlsx", sheet = "T17", skip = 6)                    
read_excel("data/NIcancer_registry/Colorectal_cancer_data_tables.xlsx", sheet = "T19", skip = 7)                       
read_excel("data/NIcancer_registry/Female_breast_insitu_tumour_data_tables.xlsx", sheet = "T10", skip = 7)             
read_excel("data/NIcancer_registry/Gallbladder and other biliary cancer data tables.xlsx", sheet = "T15", skip = 7)    
read_excel("data/NIcancer_registry/Kidney_cancer_data_tables.xlsx", sheet = "T15", skip = 7)                           
read_excel("data/NIcancer_registry/Liver cancer data tables.xlsx", sheet = "T15", skip = 7)                            
read_excel("data/NIcancer_registry/Lung_cancer_data_tables.xlsx", sheet = "T19", skip = 7)                             
# read_excel("data/NIcancer_registry/Male_breast_cancer_data_tables.xlsx", sheet = "T17", skip = 6)                      
read_excel("data/NIcancer_registry/Oesophageal_cancer_data_tables.xlsx", sheet = "T17", skip = 7)                      
read_excel("data/NIcancer_registry/Oral_cancer_data_tables.xlsx", sheet = "T19", skip = 4)                             
read_excel("data/NIcancer_registry/Ovarian_cancer_data_tables.xlsx", sheet = "T17", skip = 6)                          
read_excel("data/NIcancer_registry/Pancreatic_cancer_data_tables.xlsx", sheet = "T17", skip = 6)                       
read_excel("data/NIcancer_registry/Prostate_cancer_data_tables.xlsx", sheet = "T17", skip = 6)                         
read_excel("data/NIcancer_registry/Stomach_cancer_data_tables.xlsx", sheet = "T17", skip = 6)                          
read_excel("data/NIcancer_registry/Testicular_cancer_data_tables.xlsx", sheet = "T17", skip = 6)                       
read_excel("data/NIcancer_registry/Thyroid cancer data tables.xlsx", sheet = "T17", skip = 6)                          
read_excel("data/NIcancer_registry/Uterine_cancer_data_tables.xlsx", sheet = "T17", skip = 6)      

prevalence <- prevalence %>% 
  group_by(Disease) %>% 
  fill(.direction = 'down',Count)

ni_prevalence_plot(prevalence, past_populations, Disease = 'Stroke & TIA',save_plot = F, morbidity = stroke,model_specification = model_specification)
ni_prevalence_plot(prevalence, past_populations, Disease = 'Coronary Heart Disease',save_plot = F, morbidity = chd,model_specification = model_specification)
ni_prevalence_plot(prevalence, past_populations, Disease = 'Heart Failure',save_plot = F, morbidity = heart_failure,model_specification = model_specification)
ni_prevalence_plot(prevalence, past_populations, Disease = 'Atrial Fibrillation',save_plot = F, morbidity = atrial_fibrillation,model_specification = model_specification)
ni_prevalence_plot(prevalence, past_populations, Disease = 'Hypertension',save_plot = F, morbidity = heart_failure,model_specification = model_specification)
ni_prevalence_plot(prevalence, past_populations, Disease = 'Diabetes',save_plot = F, morbidity = diabetes,model_specification = model_specification)
ni_prevalence_plot(prevalence, past_populations, Disease = 'Non-Diabetic Hyperglycaemia',save_plot = F, morbidity = NDH,model_specification = model_specification)
ni_prevalence_plot(prevalence, past_populations, Disease = 'Cancer',save_plot = F, morbidity = cancer,model_specification = model_specification)
ni_prevalence_plot(prevalence, past_populations, Disease = 'Asthma',save_plot = F, morbidity = asthma,model_specification = model_specification)
ni_prevalence_plot(prevalence, past_populations, Disease = 'COPD',save_plot = F, morbidity = copd,model_specification = model_specification)
ni_prevalence_plot(prevalence, past_populations, Disease = 'Dementia',save_plot = F, morbidity = dementia,model_specification = model_specification)
ni_prevalence_plot(prevalence, past_populations, Disease = 'Chronic Kidney Disease',save_plot = F, morbidity = chronic_kidney_disease,model_specification = model_specification)
ni_prevalence_plot(prevalence, past_populations, Disease = 'Hypothyroidism',save_plot = F, morbidity = hypothyroidism,model_specification = model_specification)
ni_prevalence_plot(prevalence, past_populations, Disease = 'Epilepsy',save_plot = F, morbidity = epilepsy,model_specification = model_specification)
ni_prevalence_plot(prevalence, past_populations, Disease = 'Osteoporosis',save_plot = F, morbidity = osteoporosis,model_specification = model_specification)
ni_prevalence_plot(prevalence, past_populations, Disease = 'Rheumatoid Arthritis',save_plot = F, morbidity = rheumatoid_arthritis,model_specification = model_specification)
# ni_prevalence_plot(prevalence, past_populations, Disease = 'Peripheral Arterial Disease',save_plot = F, morbidity = peripheral_arterial_disease,model_specification = model_specification)



'Osteoarthritis'
'Aortic Aneurysm'
'Systemic Lupus Erythematosus'
'Inflammatory Bowel Disease'
'Intersistal Lung Disease'
'Liver Disease'